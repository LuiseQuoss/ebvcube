#install all packages if not yet installed
packages_needed <- c('devtools', 'shinyalert', 'terra', 'ebvcube',
                     'stringr', 'shiny','shinyFiles', 'ggplot2',
                     'rhdf5', 'shinyWidgets')

if (!require("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

for (pack in packages_needed){
  t <- tryCatch(
    {find.package(pack)},
    error = function(e){
	 if(pack=='rhdf5'){
        BiocManager::install("rhdf5")
      }else{
        install.packages(pack)
      }
    }
  )
}

#load the libraries
library(shiny)
library(stringr)
library(terra)
library(shinyalert)
library(shinyFiles)
library(ebvcube)
library(ggplot2)
library(rhdf5)
library(shinyWidgets)


#FUNCTIONS----


#load entity data---
p <- function(row){
  return(gsub(pattern = "(^ +| +$)",
              replacement = "",
              x = paste0(row, collapse='')))
}

#read att----
ebv_i_read_att <-  function(h5obj, name, verbose=TRUE){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  # read attribute ----
  #check if attribute exists
  if(!rhdf5::H5Aexists(h5obj, name)){
    if(verbose){
      warning(paste0('The attribute ', name, ' does not exist. Or maybe wrong location in netCDF?\n'))
    }
    return(NULL)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
    attribute <- rhdf5::H5Aread(aid)
    rhdf5::H5Aclose(aid)
    #ensure utf-8encoding for string attributes
    if(checkmate::check_character(attribute)==TRUE){
      attribute <- stringr::str_conv(attribute ,"UTF-8")
    }
    return(attribute)
  }
}

ebv_i_get_dates <- function(hdf){
  #read time integers and turn into dates
  add <- 40177
  time_data <- suppressWarnings(rhdf5::h5read(hdf, 'time'))
  dates <- as.Date(time_data - add, origin = '1970-01-01')
  #get time resolution
  res <-ebv_i_read_att(hdf, 'time_coverage_resolution', verbose)
  #modify date
  if(res!='Irregular'){
    y <- stringr::str_split(stringr::str_remove(res, 'P')[[1]], '-')[[1]][1]
    m <- stringr::str_split(stringr::str_remove(res, 'P')[[1]], '-')[[1]][2]
    d <- stringr::str_split(stringr::str_remove(res, 'P')[[1]], '-')[[1]][3]
    if(d == '00' & m == '00'){
      time_natural <- strtrim(dates, 4)
      # time_natural <- gsub("-.*", "", time_natural)
    } else if(d == '00'){
      time_natural <- strtrim(dates, 7)
    }
  }
  names(dates) <- time_natural
  return(dates)
}

# available netCDFs----
# print('here')
filelist <- list.files('./data/', '*.nc', full.names = T) #'/home/lq39quba/ShinyApps/ebv-taxonomy
titles <- c()
for (f in filelist){
  hdf <- rhdf5::H5Fopen(f, flags = "H5F_ACC_RDONLY")
  titles <- c(titles, ebv_i_read_att(hdf, 'title'))
}
names(filelist) <- titles
# print(filelist)

#USER INTERFACE ----
ui <- fluidPage(
  #set theme
  theme = bslib::bs_theme(bootswatch = "flatly"),
  #title
  titlePanel("Browse the Taxonomy of your EBV netCDF file"),

  #short how-to
  uiOutput('ui_howto'),

  #load netcdf
  uiOutput('ui_load_netCDF'),

  #get netCDF
  selectInput("load_netcdf", label = '', choices = filelist, multiple=FALSE, width='90.5%'),

  #some space
  tags$div(
    style="margin-bottom:10px;"
  ),

  #add 'Choose an entity'
  uiOutput('ui_title'),
  #some space
  tags$div(
    style="margin-bottom:25px;"
  ),

  #some space
  tags$div(
    style="margin-bottom:5px;"
  ),

  #create selectInput based on the taxonomy levels
  uiOutput('select_pane'),

  #some space
  tags$div(
    style="margin-bottom:25px;"
  ),

  #some space
  tags$div(
    style="margin-bottom:10px;"
  ),

  #choose datacube
  uiOutput('ui_datacube_txt'),
  uiOutput('ui_datacube'),

  #some space
  tags$div(
    style="margin-bottom:5px;"
  ),

  actionButton('button_map', label = "Plot Map", class = "btn-lg btn-success"),

  #some space
  tags$div(
    style="margin-bottom:20px;"
  ),

  #describe stuff in plot
  fluidRow(
    #empty column
    column(1,),
    #description column
    column(6,
         #description
         uiOutput('ui_title2'),
         uiOutput('ui_scenario'),
         uiOutput('ui_metric'),
         uiOutput('ui_entity'),
         #some space
         tags$div(
           style="margin-bottom:15px;"
         ),
           fluidRow(#text time slider & quantiles
             column(6,uiOutput("txt_date")),
             column(4,uiOutput('txt_quantiles'))
           ),
         fluidRow(#time slider & quantile selection
           column(6,uiOutput("timeslider")),
           column(2,uiOutput('classes_quantiles'))
         ),

         ),
    #help text for map
    # column(2,uiOutput('ui_map_help')),
    # column(2,actionButton('button_trend', label = "Plot Trend", class = "btn-lg btn-success"),)
    ),

  fluidRow(
    # #empty column
    # column(1,),
    #map
    column(8, plotOutput("plot_map",# width='900px',
             #height = '500px',
             click="plot_click",
             dblclick = "plot1_dblclick",
             brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE))
           ),
    #trend
    # column(5, plotOutput("plot_trend")
    #        )
    ),
  
  tags$script("$(document).on('change', '.dynamicSI select', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });"),
  
  fluidRow(
    #empty column
    column(1,),
    column(8,
      #zoom explanation
      uiOutput('ui_map_help')
    )
  ),
  
  #some space
  tags$div(
    style="margin-bottom:25px;"
  )


)


#SERVER ----
server <- function(input, output, session) {
  #set reactive values ----
  cat('set reactive values\n')
  filepath <- reactiveValues()
  # filepath <- ''
  s <- reactiveVal(c())
  taxonlevels_data <- reactiveValues()
  taxonlevels_data <- c()
  entity_data <- reactiveValues()
  entity_data <- data.frame()
  map_index <- reactiveValues()
  map_index <- c(1)
  ranges <- reactiveValues(x = NULL, y = NULL)
  title <- reactiveValues()
  datacubes <- reactiveValues()
  #get boundry data
  data("world_boundaries")
  borders <- terra::vect(world_boundaries, geom='geometry',crs='EPSG:4326')
  
  # options(shiny.maxRequestSize=3*1024^3) #set max file size for fileInput to 3GB

  #on stop----
  onStop(function(hdf, did) {
    cat("Doing application cleanup\n")
    rhdf5::h5closeAll()
  })

  #how to
  output$ui_howto <-renderUI({
    span(HTML("<b>How-To:</b></br>
              Choose one of the netCDFs in the drop down menu.</br>
              Then have a little bit of patience while the data is loading...
              Afterwards you can browse throgh the levels to find a specific entity (e.g. species).</br>
              You can also start typing in the drop-down menus to search. Often there are more options that you can see - so start typing!
              </br></br>"), style="font-size:16px")
  })

  output$ui_load_netCDF <-renderUI({
    span(renderText({paste0('Choose a netCDF: ')}), style="font-size:20px; font-weight: bold")
  })

  #load netCDF data----
  observeEvent(input$load_netcdf,{
    if(!is.null(input$load_netcdf)){
      cat('now cleaning\n')
      #clean output UI
      output$ui_title2 <- renderUI(NULL)
      output$ui_scenario <- renderUI(NULL)
      output$ui_metric <- renderUI(NULL)
      output$ui_entity <- renderUI(NULL)
      output$ui_datacube <- renderUI(NULL)
      output$ui_datacube_txt <- renderUI(NULL)
      output$ui_title <- renderUI(NULL)
      output$select_pane <- renderUI(NULL)
      output$plot_map <- renderPlot(NULL)
      output$plot_trend <- renderPlot(NULL)

      # #set reactive values ----
      # cat('set reactive values\n')
      # filepath <- reactiveValues()
      # filepath <- ''
      # s <- reactiveVal(c())
      # taxonlevels_data <- reactiveValues()
      # taxonlevels_data <- c()
      # entity_data <- reactiveValues()
      # entity_data <- data.frame()
      # map_index <- reactiveValues()
      # map_index <- c(1)
      # # ranges <- reactiveValues(x = NULL, y = NULL)
      # title <- reactiveValues()
      # datacubes <- reactiveValues()
      # #get boundry data
      # data("world_boundaries")
      # borders <- terra::vect(world_boundaries, geom='geometry',crs='EPSG:4326')
      # # cat('ranges x:', ranges$x, '\n')
      # # cat('ranges y:', ranges$y, '\n')
      
      #close 'old' rhdf5 handles - just in case
      rhdf5::h5closeAll()


      #get filepath----
      filepath <- input$load_netcdf
      print(filepath)

      #open file
      cat(filepath, 'filepath when loading')

      #add title to pane ----
      prop <- ebv_properties(filepath, verbose=F)
      title <- prop@general$title
      # output$ui_title <- renderUI({
      #   span(renderText({paste0('Dataset: ', title)}), style="font-size:20px; font-weight: bold")
      # })
      output$ui_title <- renderUI({
        span(renderText({'Choose an entity: '}), style="font-size:20px; font-weight: bold")
      })

      #get taxoninfo
      cat('-----------------------new netCDF-----------------------',  '\n')
      cat('reading levels of the ebv netCDF',  '\n')
      hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
      taxonlevels_data <- names(prop@general$taxonomy)
      
      #get whole entity list
      # hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
      did_list <- rhdf5::H5Dopen(hdf, 'entity_list')
      did_list_data <- rhdf5::H5Dread(did_list)
      entity_data <- data.frame('taxon_level'=NA, 'species_id'=NA, 'values'=NA)
      dims <- dim(did_list_data)

      if (!is.na(nrow(did_list_data))) {
        #loop through levels
        for (level in 1:dims[1]) {
          cat(level, '\t')
          taxon_level <- c()
          species_id <- c()
          values <- c()


          values <-apply(did_list_data[level,,], 1, p)
          taxon_level <- rep.int(level, dims[2])
          species_id <- 1:dims[2]

          #add data to final output
          part <- cbind(taxon_level, species_id, values)
          entity_data <- rbind(entity_data, part)
        }

      }
      cat('done', '\n')
      entity_data <- entity_data[-1,] #remove first empty line (NA)

      rhdf5::H5Dclose(did_list)

      #close file
      rhdf5::H5Fclose(hdf)
    #}

      #fill selectInputs based on hierarchy----
      if(length(entity_data)>0){
        output$select_pane <- renderUI({

          cat('loading selectizeInputs',  '\n')
          cat(taxonlevels_data,  '\n')
          #get taxon levels values:
          taxon_levels <- list()
          for (i in 1:length(taxonlevels_data)){
            taxon_levels[[taxonlevels_data[i]]] <- i
          }
          taxon_levels <- rev(taxon_levels)

          #add empty selectizeInput
          output = tagList()

          for (tax_lev in names(taxon_levels)){

            id <- paste0(tax_lev, '_level')
            output[[tax_lev]] <- selectizeInput(id, paste0('Choose a(n) ',tax_lev),
                                                width='90.5%',
                                                choices = NULL,
                                                multiple = F)

          }

          div( class = "dynamicSI", #additional div to track changes
               output
          )

        })
      }

      if(length(entity_data)>0){
        #add data to server-based selectizeInput
        taxon_levels <- list()
        taxon_levels[taxonlevels_data] <- 1:length(taxonlevels_data)
        taxon_levels <- rev(taxon_levels)

        for (tax_lev in names(taxon_levels)){
          #print(tax_lev)
          input_levs <- list()

          levs <- unique(entity_data[entity_data$taxon_level==taxon_levels[[tax_lev]],'values'])
          #put into named list for select input
          input_levs[levs] <- levs

          id <- paste0(tax_lev, '_level')
          if(id != paste0(taxonlevels_data[1],'_level')){
            input_levs[['All']] <- 'all'
          }
          id <- paste0(tax_lev, '_level')

          updateSelectizeInput(session, id, choices=input_levs, server = TRUE)

        }

        #end: entity_data >0
      }

        #add ebvcube selection ----
        datacubes <- ebv_datacubepaths(filepath, verbose=F)
        
        output$ui_datacube_txt <-renderUI({
          span(renderText({paste0('Choose a datacube: ')}), style="font-size:20px; font-weight: bold")
        })
        
        output$ui_datacube <- renderUI({
          cubes <- list()
          cubes[1:dim(datacubes)[1]] <- datacubes$datacubepaths
          if('scenario_names' %in% names(datacubes)){
            names(cubes) <- paste0(datacubes$scenario_names,': ', datacubes$metric_names)
          }else{
            names(cubes) <- datacubes$metric_names
          }
          
          #output$datacubes <-
          selectInput(
            'select_datacube',
            '',
            width = '90.5%',
            choices = cubes,
            multiple = F
          )
          
        })


      #end !is.null(input$load_netcdf)
    }

    #update SelectizeInput----
    #https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs
    observe({
      if (!is.null(input$lastSelectId) ) {
        if(input$lastSelectId != paste0(taxonlevels_data[1],'_level')){

          #get the ID of the select input that was changed
          select_id <- input$lastSelectId


          if (length(entity_data) > 0) {
            cat('----update selectInput',  '\n')

            #print(taxonlevels_data)
            if(any(taxonlevels_data==stringr::str_remove(select_id, '_level') )){
              #print('NOW UPDATE')

              #get taxon levels values:
              taxon_levels <- list()
              taxon_levels[taxonlevels_data] <- 1:length(taxonlevels_data)
              taxon_levels <- rev(taxon_levels)

              #print(select_id)
              tax_id <- stringr::str_remove(select_id, '_level')
              #print('tax_id')
              cat('selected level: ', tax_id,  '\n')

              #value at currently 'highest' taxon level
              selected_value <- input[[select_id]]
              cat('selected value: ', selected_value, '\n')

              k = as.numeric(taxon_levels[tax_id])
              subset <- entity_data

              if(selected_value!='all'){
                subset_species <- subset[which(subset$taxon_level==as.character(k) & subset$values == selected_value),]$species_id
                subset <- subset[subset$species_id%in%subset_species,]
              }else{
                k_all <- k + 1
                #make sure that k+1 never goes higher than the actual highest level
                if(k_all <= max(unlist(taxon_levels))){
                  print('subset based on selection')
                  select_id_all <- paste0(names(taxon_levels[taxon_levels==k_all]), '_level')
                  #get selected value of the level that is one higher, use that selection as basis for the next selection
                  selected_value_all <- input[[select_id_all]]

                  if(selected_value_all=='all'){
                    print('not changing the subset')
                  }else{
                    subset_species <- subset[which(subset$taxon_level==as.character(k+1) & subset$values == selected_value_all),]$species_id
                    subset <- subset[subset$species_id%in%subset_species,]
                  }
                }else{
                  print('include all')
                }
              }


              #loop though all 'lower' taxons, adjust them
              for (k in (as.numeric(taxon_levels[tax_id]) - 1):1) {
                #print('in k loop')
                #print(k)

                #get data for selectInput below
                input_levs <- list()
                #print(unique(subset[subset$taxon_level==as.character(k),'values']))
                levs <- unique(subset[subset$taxon_level==k,'values'])

                #put into named list for select input
                input_levs[levs] <- levs

                update_id <- paste0(names(rev(taxon_levels))[k], '_level')
                if(update_id != paste0(taxonlevels_data[1],'_level')){
                  input_levs[['All']] <- 'all' #paste0(1:length(levs), collapse=',')
                }

                updateSelectizeInput(session, update_id, choices=input_levs, server = TRUE)

              }

            }else{
              cat('NO UPDATE', '\n')
            }
          }
        }
      }
    })

    #calculate quantiles----
    observeEvent(input$classes,{
      datacubepath <- input$select_datacube
      if(length(map_index)>0){
        print('calculating quantiles')
        data.all <- rhdf5::h5read(file = filepath,
                                  name = datacubepath,
                                  index = list(NULL, NULL,NULL,map_index))
        s(as.numeric(stats::quantile(data.all, probs = seq(0, 1, (1/input$classes)), na.rm=TRUE)))
      }

    })

    observeEvent(input$select_datacube,{
      datacubepath <- input$select_datacube
      if(length(map_index)>0 & !is.null(input$classes)){
        print('calculating quantiles')
        data.all <- rhdf5::h5read(file = filepath,
                                  name = datacubepath,
                                  index = list(NULL, NULL,NULL,map_index))
        s(as.numeric(stats::quantile(data.all, probs = seq(0, 1, (1/input$classes)), na.rm=TRUE)))
      }

    })

    #plot map button----
    observeEvent(input$button_map,{
      cat('----plot map\n')

      #else there is an error when you load another netcdf - old value sticks there
      if((any(taxonlevels_data==stringr::str_remove(input$lastSelectId, '_level') ) | is.null(input$lastSelectId)) &
        file.exists(filepath) & stringr::str_ends(file.path(getwd(),filepath), '.nc')){

        cat('current filepath: ', filepath, '\n')

        #get map index for plotting
        map_index <- which(entity_data[entity_data$taxon_level==1,'values']== input[[paste0(taxonlevels_data[1],'_level')]])
        cat('mapindex: ', map_index,  '\n')

        #get timesteps
        hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
        timesteps <- ebv_i_get_dates(hdf)
        rhdf5::H5Fclose(hdf)

        #create date slider
        output$txt_date <-renderUI({
          span('Choose the date:', style="font-size:16px")
        })
        
        output$timeslider<-renderUI({ sliderTextInput(inputId = 'daterange',
                                                     label = "",
                                                     choices = timesteps,
                                                     selected = timesteps[1],
                                                     grid = TRUE
                                                     )
        })
        
        #quantile selection
        output$txt_quantiles <-renderUI({
          span('Set amount of quantiles (max=20):', style="font-size:16px")
        })

        output$classes_quantiles<-renderUI({ numericInput('classes',
                                                   '',
                                                   value=10, min=1, max=20
                                                   )
        })

        if(length(map_index)>0 ){ #& !is.null(input$daterange)

          #create plot
          output$plot_map <- renderPlot({
            #cat('map index in render plot: ', map_index,  '\n')

            datacubepath <- input$select_datacube
            cat('read data for cube: ', datacubepath, '\n')
            
            req(input$daterange)
            print(input$daterange)
            cat('here reading: ', filepath, '\n')
            data.raster <- ebv_read(filepath, datacubepath, entity=map_index, timestep = input$daterange,
                                    type='r', ignore_RAM=TRUE,
                                    verbose=FALSE)

            #check color
            mini <- suppressWarnings(min(as.array(data.raster), na.rm=T))
            maxi <- suppressWarnings(max(as.array(data.raster), na.rm=T))
            if(mini==-Inf|mini== Inf|maxi== Inf|maxi==-Inf){
              plot <- terra::plot(data.raster, fun=function()lines(borders))
            }else{
              if(mini==maxi){
                col_def <- 'aquamarine4'
                s <- maxi
              }else{
                if (mini<0 && maxi>0){
                  col_def <- colorRampPalette(c( "brown2",'lemonchiffon1',"cornflowerblue"))(input$classes)
                } else {
                  col_def <- colorRampPalette(c("#ffffcc","#c2e699",'#78c679', "#238443"))(input$classes)
                }
              }
              # print(col_def)
              cat('generate plot',  '\n')
              plot <- terra::plot(data.raster,col=col_def,
                                  breaks = s(),
                                  fun=function()lines(borders),
                                  decreasing=FALSE)
            }
            plot
          })

          #create description text----
          f <- filepath
          #cat('file for description', f,  '\n')

          if(file.exists(f) & stringr::str_ends(file.path(getwd(),f), '.nc')){
            cat('create description',  '\n')
            prop <- ebv_properties(f, input$select_datacube, verbose=FALSE)

            output$ui_title2 <-renderUI({
              span(HTML(paste0('<b>Dataset: ', prop@general$title, '</b>')), style="font-size:20px")
            })

            if('scenario_names' %in% names(datacubes)){
              output$ui_scenario <-renderUI({
                scenario_name <- datacubes$scenario_names[datacubes$datacubepaths==input$select_datacube]
                span(HTML(paste0('<b>Scenario:</b> ', scenario_name)), style="font-size:16px")
              })
            }

            output$ui_metric <-renderUI({
              metric_name <- datacubes$metric_names[datacubes$datacubepaths==input$select_datacube]
              span(HTML(paste0('<b>Metric:</b> ', metric_name, ' (', prop@ebv_cube$units,')')), style="font-size:16px")
            })

            output$ui_entity <-renderUI({
              span(HTML(paste0('<b>Entity:</b> ', prop@general$entity_names[map_index])), style="font-size:16px")
            })
          }
        }
      }

      #add help description text
      output$ui_map_help <-renderUI({
        span(HTML("<b>How-To Zoom:</b></br>
                  Draw a rectangle over the area you want to zoom in on. Then double
                -click on that area. If you want to zoom out to the global extent,
                  simply double-click on the map."
              ), style="font-size:16px;" )
      })
      
    })#end observe plot button
    
    # #plot trend----
    # observeEvent(input$button_trend,{
    #   cat('----create trend plot\n')
    #   
    #   output$plot_trend <- renderPlot({
    #     map_index <- which(entity_data[entity_data$taxon_level==1,'values']== input[[paste0(taxonlevels_data[1],'_level')]]) 
    #     datacubepath <- input$select_datacube
    #     ebv_trend(filepath, datacubepath, entity=map_index, method = 'mean',
    #               verbose=TRUE)
    #     p <- ggplot2::last_plot()
    #     print(p)
    # })
    #   
      
      
    # })#end render trend plot

    #https://stackoverflow.com/questions/44131861/shiny-ggplot2-plotoutput-zoom-floating-geom-text-position
    #zoom map----
    observeEvent(input$plot1_dblclick, {
      filepath <- input$load_netcdf
      cat('zoom',  '\n')
      brush <- input$plot1_brush
      cat(brush$xmin, brush$xmax, '\n')
      #print(brush)
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
      

      #get map index for plotting
      map_index <- which(entity_data[entity_data$taxon_level==1,'values']== input[[paste0(taxonlevels_data[1],'_level')]])

      output$plot_map <- renderPlot({#
        
        datacubepath <- input$select_datacube
        print(input$daterange)
        # print(filepath)
        # print('\n')
        data.raster <- ebv_read(filepath, datacubepath, entity=map_index, timestep = input$daterange,
                                type='r', ignore_RAM=TRUE,
                                verbose=FALSE)

        #check color
        mini <- suppressWarnings(min(as.array(data.raster), na.rm=T))
        maxi <- suppressWarnings(max(as.array(data.raster), na.rm=T))
        if(mini==-Inf|mini== Inf|maxi== Inf|maxi==-Inf){
          plot <- terra::plot(data.raster, fun=function()lines(borders))
        }else{
          if(mini==maxi){
            col_def <- 'aquamarine4'
            s <- maxi
          }else{
            if (mini<0 && maxi>0){
              col_def <- colorRampPalette(c("cornflowerblue",'lemonchiffon1', "brown2"))(input$classes)
            } else {
              col_def <- colorRampPalette(c("#ffffcc","#c2e699",'#78c679', "#238443"))(input$classes)
            }
          }
          cat('generate plot',  '\n')
          plot <- terra::plot(data.raster,col=col_def,
                              breaks = s(),
                              fun=function()lines(borders),
                              xlim = ranges$x, ylim = ranges$y,
                              decreasing=FALSE)
        }
        plot
      })#end renderPlot
    })#end observeEvent plot1_dblclick
    

  })#end check input netCDF

}

#run app----
shinyApp(ui = ui, server = server)
