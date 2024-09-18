#' The Shiny App Server for ebv_taxonomy_app
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#' @noRd
ebv_taxonomy_server <- function(input, output, session) {

  #on stop----
  shiny::onStop(function(hdf, did) {
    message("Doing application cleanup\n")
    rhdf5::h5closeAll()
  })

  #how to
  output$ui_howto <- shiny::renderUI({
    shiny::span(shiny::HTML("<b>How-To:</b>
              Click on the 'Load netCDF' button to select an EBV netCDF file with taxonomy information.</br>
              Then have a little bit of patience while the data is loading...
              Afterwards you can browse throgh the levels to find a specific entity (lowest level).</br>
              You can also type into the drop-down menus to search. Often there are more options that you can see - so start typing!
              </br></br>"), style="font-size:16px")
  })

  #load netCDF data----
  shiny::observeEvent(input$load_netcdf, {
    if(!is.null(input$load_netcdf)){
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
      output$txt_date <- renderUI(NULL)
      output$txt_quantiles <- renderUI(NULL)
      output$classes_quantiles <- renderUI(NULL)
      output$classes <- renderUI(NULL)
      output$daterange <- renderUI(NULL)

      #set reactive values ----
      filepath <- shiny::reactiveValues()
      filepath <- ''
      taxonlevels_data <- shiny::reactiveValues()
      taxonlevels_data <- c()
      entity_data <- shiny::reactiveValues()
      entity_data <- data.frame()
      map_index <- reactiveVal(1)
      ranges <- shiny::reactiveValues(x = NULL, y = NULL)
      title <- shiny::reactiveValues()
      datacubes <- shiny::reactiveValues()
      current_date <- shiny::reactiveValues()
      current_date <- 1
      timesteps <- shiny::reactiveValues()
      s <- reactiveVal(c())

      #close 'old' rhdf5 handles - just in case
      rhdf5::h5closeAll()

      #get filepath
      shinyFiles::shinyFileChoose(input, "load_netcdf", roots = shinyFiles::getVolumes(), session = session)
      fileinfo <- shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$load_netcdf)

      if (nrow(fileinfo) > 0) {
        #open file
        filepath <- fileinfo$datapath
        hdf <- rhdf5::H5Fopen(as.character(filepath))
        if(verbose){message(filepath, ' - filepath when loading')}

        #get timesteps ----
        timesteps <- ebv_i_get_dates(hdf)

        #add title to pane ----
        title <- ebv_i_read_att(hdf, 'title', verbose=FALSE)
        output$ui_title <- shiny::renderUI({
          shiny::span(shiny::renderText({paste0('Dataset: ', title)}), style="font-size:20px; font-weight: bold")
        })

        #add datacube selection ----
        datacubes <- ebv_datacubepaths(fileinfo$datapath, verbose=FALSE)

        output$ui_datacube_txt <-shiny::renderUI({
          shiny::span(shiny::renderText({paste0('Choose a datacube: ')}), style="font-size:20px; font-weight: bold")
        })

        output$ui_datacube <- shiny::renderUI({
          cubes <- list()
          cubes[1:dim(datacubes)[1]] <- datacubes$datacubepaths
          if('scenario_names' %in% names(datacubes)){
            names(cubes) <- paste0(datacubes$scenario_names, ': ', datacubes$metric_names)
          }else{
            names(cubes) <- datacubes$metric_names
          }

          #output$datacubes <-
          shiny::selectInput(
            'select_datacube',
            '',
            width = '90.5%',
            choices = cubes,
            selected = cubes[1],
            multiple = FALSE
          )

        })

        #get taxoninfo
        if(verbose){message('-----------------------new netCDF-----------------------')}
        if(verbose){message('reading levels of the ebv netCDF')}
        did <- rhdf5::H5Dopen(hdf, 'entity_levels')
        did_data <- rhdf5::H5Dread(did)
        taxon_levels <- c()
        if (!is.na(nrow(did_data))) {
          for (row in 1:nrow(did_data)) {
            name <- paste0(did_data[row, ], collapse = '')
            taxon_levels <- c(taxon_levels, name)
          }
          #trim whitespaces
          taxon_levels <- gsub(pattern = "(^ +| +$)",
                               replacement = "",
                               x = taxon_levels)
        }
        taxonlevels_data <-taxon_levels
        rhdf5::H5Dclose(did)

        #get whole entity list
        did_list <- rhdf5::H5Dopen(hdf, 'entity_list')
        did_list_data <- rhdf5::H5Dread(did_list)
        entity_data <- data.frame('taxon_level'=NA, 'species_id'=NA, 'values'=NA)
        dims <- dim(did_list_data)

        if (!is.na(nrow(did_list_data))) {
          #loop through levels
          for (level in 1:dims[1]) {
            if(verbose){message(level, '\t')}
            taxon_level <- c()
            species_id <- c()
            values <- c()


            values <-apply(did_list_data[level, , ], 1, ebv_i_p)
            taxon_level <- rep.int(level, dims[2])
            species_id <- 1:dims[2]

            #add data to final output
            part <- cbind(taxon_level, species_id, values)
            entity_data <- rbind(entity_data, part)
          }

        }
        if(verbose){message('done')}
        entity_data <- entity_data[-1, ] #remove first empty line (NA)

        rhdf5::H5Dclose(did_list)

        #close file
        rhdf5::H5Fclose(hdf)

      }

      #fill selectInputs based on hierarchy----
      if(length(entity_data)>0){
        output$select_pane <- shiny::renderUI({

          if(verbose){message('loading selectizeInputs')}
          if(verbose){message(taxonlevels_data)}
          #get taxon levels values:
          taxon_levels <- list()
          for (i in 1:length(taxonlevels_data)){
            taxon_levels[[taxonlevels_data[i]]] <- i
          }
          taxon_levels <- rev(taxon_levels)

          #add empty selectizeInput
          output <- tagList()

          for (tax_lev in names(taxon_levels)){

            id <- paste0(tax_lev, '_level')
            output[[tax_lev]] <- shiny::selectizeInput(id, paste0('Choose a(n) ', tax_lev),
                                                width='90.5%',
                                                choices = NULL,
                                                multiple = FALSE)

          }

          shiny::div(class = "dynamicSI", #additional div to track changes
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
          input_levs <- list()

          levs <- unique(entity_data[entity_data$taxon_level==taxon_levels[[tax_lev]], 'values'])
          #put into named list for select input
          input_levs[levs] <- levs

          id <- paste0(tax_lev, '_level')
          if(id != paste0(taxonlevels_data[1], '_level')){
            input_levs[['All']] <- 'all'
          }
          id <- paste0(tax_lev, '_level')

          shiny::updateSelectizeInput(session, id, choices=input_levs, server = TRUE)

        }

        #end: entity_data >0
      }




      #end !is.null(input$load_netcdf)
    }

    #update SelectizeInput----
    #https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs
    shiny::observe({
      if (!is.null(input$lastSelectId)) {
        if(input$lastSelectId != paste0(taxonlevels_data[1], '_level')){

          #get the ID of the select input that was changed
          select_id <- input$lastSelectId


          if (length(entity_data) > 0) {
            if(verbose){message('----update selectInput')}

            if(any(taxonlevels_data==stringr::str_remove(select_id, '_level'))){

              #get taxon levels values:
              taxon_levels <- list()
              taxon_levels[taxonlevels_data] <- 1:length(taxonlevels_data)
              taxon_levels <- rev(taxon_levels)
              tax_id <- stringr::str_remove(select_id, '_level')
              if(verbose){message('selected level: ', tax_id)}

              #value at currently 'highest' taxon level
              selected_value <- input[[select_id]]
              if(verbose){message('selected value: ', selected_value)}

              k <- as.numeric(taxon_levels[tax_id])
              subset <- entity_data

              if(selected_value!='all'){
                subset_species <- subset[which(subset$taxon_level==as.character(k) & subset$values == selected_value), ]$species_id
                subset <- subset[subset$species_id%in%subset_species, ]
              }else{
                k_all <- k + 1
                #make sure that k+1 never goes higher than the actual highest level
                if(k_all <= max(unlist(taxon_levels))){
                  if(verbose){message('subset based on selection')}
                  select_id_all <- paste0(names(taxon_levels[taxon_levels==k_all]), '_level')
                  #get selected value of the level that is one higher, use that selection as basis for the next selection
                  selected_value_all <- input[[select_id_all]]

                  if(selected_value_all=='all'){
                    if(verbose){message('not changing the subset')}
                  }else{
                    subset_species <- subset[which(subset$taxon_level==as.character(k+1) & subset$values == selected_value_all), ]$species_id
                    subset <- subset[subset$species_id%in%subset_species, ]
                  }
                }else{
                  if(verbose){message('include all')}
                }
              }


              #loop though all 'lower' taxons, adjust them
              for (k in (as.numeric(taxon_levels[tax_id]) - 1):1) {

                #get data for selectInput below
                input_levs <- list()
                levs <- unique(subset[subset$taxon_level==k, 'values'])

                #put into named list for select input
                input_levs[levs] <- levs

                update_id <- paste0(names(rev(taxon_levels))[k], '_level')
                if(update_id != paste0(taxonlevels_data[1], '_level')){
                  input_levs[['All']] <- 'all'
                }

                shiny::updateSelectizeInput(session, update_id, choices=input_levs, server = TRUE)

              }


            }else{
              if(verbose){message('NO UPDATE')}
            }


          }

        }


      }

    })

    #action plot map button----
    shiny::observeEvent(input$button_map, {
      #create quantile slider----
      output$txt_quantiles <- shiny::renderUI({
        shiny::span('Set amount of quantiles (max=20):', style="font-size:16px")
      })
      output$classes_quantiles<- shiny::renderUI({
        shiny::numericInput('classes', '',
                            value=10, min=1, max=20)
      })

      if(verbose){message('----plot map')}

      #else there is an error when you load another netcdf - old value sticks there
      if((any(taxonlevels_data==stringr::str_remove(input$lastSelectId, '_level')) | is.null(input$lastSelectId)) &
         file.exists(filepath) & stringr::str_ends(file.path(getwd(), filepath), '.nc')){

        if(verbose){message('current filepath: ', filepath)}

        #get map index for plotting
        map_index(which(entity_data[entity_data$taxon_level==1, 'values']== input[[paste0(taxonlevels_data[1], '_level')]]))

        #create date slider ----
        if(length(timesteps)>1){
          output$txt_date <- shiny::renderUI({
            shiny::span('Choose the date:', style="font-size:16px")
          })
          output$timeslider<-renderUI({
            shinyWidgets::sliderTextInput(inputId = 'daterange',
                                          label = "",
                                          choices = timesteps,
                                          selected = timesteps[1],
                                          grid = TRUE)
          })
        }else{
          #just print date if only one date
          output$txt_date <- shiny::renderUI({
            shiny::span(shiny::HTML(paste0('<b>Single date:</b> ', timesteps[1])), style="font-size:16px")
          })
          current_date <- as.character(timesteps[1])
        }


        if(verbose){message('mapindex: ', map_index())}

        if(length(map_index())>0){

          #create plot ----
          output$plot_map <- shiny::renderPlot({

            borders <- terra::vect(world_boundaries, geom='geometry', crs='EPSG:4326')
            if(length(timesteps)>1){
              req(input$daterange)
              current_date <- input$daterange
            }

            datacubepath <- input$select_datacube
            if(verbose){message('read data for cube: ', datacubepath)}
            data.raster <- ebv_read(filepath, datacubepath, entity=map_index(), timestep = current_date,
                                    type='r', ignore_RAM=TRUE,
                                    verbose=FALSE)

            #calculate quantiles ----
            observeEvent(c(input$classes, input$select_datacube), {
              datacubepath <- input$select_datacube
              if(length(map_index())>0 & !is.null(input$classes)){
                data.all <- rhdf5::h5read(file = as.character(fileinfo$datapath),
                                          name = datacubepath,
                                          index = list(NULL, NULL, NULL, map_index()))
                s(as.numeric(stats::quantile(data.all, probs = seq(0, 1, (1/input$classes)), na.rm=TRUE)))
              }

            })

            #define color 1----
            mini <- suppressWarnings(min(terra::as.array(data.raster), na.rm=TRUE))
            maxi <- suppressWarnings(max(terra::as.array(data.raster), na.rm=TRUE))
            if(mini==-Inf|mini== Inf|maxi== Inf|maxi==-Inf){
              plot <- terra::plot(data.raster, fun=function()terra::lines(borders))
            }else{
              if(mini==maxi){
                #single value----
                output$txt_quantiles <- shiny::renderUI({
                  shiny::span(shiny::HTML(paste0('<b>Single value:</b> ', mini)), style="font-size:16px")
                })
                col_def <- 'aquamarine4'
                output$classes_quantiles <- NULL
                s(NULL)
              }else{
                if (mini<0 && maxi>0){
                  col_def <- colorRampPalette(c("cornflowerblue", 'lemonchiffon1', "brown2"))(input$classes)
                } else {
                  col_def <- colorRampPalette(c("#ffffcc", "#c2e699", '#78c679', "#238443"))(input$classes)
                }
              }
              if(verbose){message('generate plot')}
              plot <- terra::plot(data.raster, col=col_def,
                                  breaks = s(),
                                  fun=function()terra::lines(borders)
                                  )

              # s <- round(as.numeric(s()), 2)
              # legend_s <- rev(paste0(s[1:length(s)-1], ' - ',s[2:length(s)]))
              # terra::add_legend("topright",
              #        legend = legend_s,
              #        fill = col_def,
              #        border = "black",
              #        cex=0.5)
            }

            plot

          })

          #create description text----
          f <- filepath

          if(file.exists(f) & stringr::str_ends(file.path(getwd(), f), '.nc')){
            if(verbose){message('create description')}
            prop <- ebv_properties(f, input$select_datacube, verbose=FALSE)

            output$ui_title2 <- shiny::renderUI({
              shiny::span(shiny::HTML(paste0('<b>Dataset: ', prop@general$title, '</b>')), style="font-size:20px")
            })

            if('scenario_names' %in% names(datacubes)){
              output$ui_scenario <-shiny::renderUI({
                scenario_name <- datacubes$scenario_names[datacubes$datacubepaths==input$select_datacube]
                shiny::span(shiny::HTML(paste0('<b>Scenario:</b> ', scenario_name)), style="font-size:16px")
              })
            }

            output$ui_metric <-shiny::renderUI({
              metric_name <- datacubes$metric_names[datacubes$datacubepaths==input$select_datacube]
              shiny::span(shiny::HTML(paste0('<b>Metric:</b> ', metric_name, ' (', prop@ebv_cube$units, ')')), style="font-size:16px")
            })

            output$ui_entity <- shiny::renderUI({
              shiny::span(shiny::HTML(paste0('<b>Entity:</b> ', prop@general$entity_names[map_index()])), style="font-size:16px")
            })
          }


        }


      }

      #add help description text
      output$ui_map_help <- shiny::renderUI({
        shiny::span(shiny::HTML("<b>How-To Zoom:</b></br>
                  Draw a rectangle over the area you want to zoom in on. Then double-click on that area.
                                If you want to zoom out to the global extent, simply double-click on the map."
        ), style="font-size:16px;")
      })

      #end observe plot button
    })

    #https://stackoverflow.com/questions/44131861/shiny-ggplot2-plotoutput-zoom-floating-geom-text-position
    #action when zoom----
    shiny::observeEvent(input$plot1_dblclick, {
      if(verbose){message('zoom')}
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }

      #get map index for plotting
      map_index(which(entity_data[entity_data$taxon_level==1, 'values']== input[[paste0(taxonlevels_data[1], '_level')]]))

      output$plot_map <- shiny::renderPlot({
        # data("world_boundaries")
        borders <- terra::vect(world_boundaries, geom='geometry', crs='EPSG:4326')
        # if(verbose){message(timesteps)}
        if(length(timesteps)>1){
          req(input$daterange)
          current_date <- input$daterange
        }

        datacubepath <- input$select_datacube
        data.raster <- ebv_read(filepath, datacubepath, entity=map_index(), timestep = current_date,
                                type='r', ignore_RAM=TRUE,
                                verbose=FALSE)
        #define color 2----
        mini <- suppressWarnings(min(terra::as.array(data.raster), na.rm=TRUE))
        maxi <- suppressWarnings(max(terra::as.array(data.raster), na.rm=TRUE))
        if(mini==-Inf|mini== Inf|maxi== Inf|maxi==-Inf){
          plot <- terra::plot(data.raster, fun=function()terra::lines(borders))
        }else{
          if(mini==maxi){
            #single value----
            output$txt_quantiles <- shiny::renderUI({
              shiny::span(shiny::HTML(paste0('<b>Single value:</b> ', mini)), style="font-size:16px")
            })
            col_def <- 'aquamarine4'
            output$classes_quantiles <- NULL
            s(NULL)
          }else{
            if (mini<0 && maxi>0){
              col_def <- colorRampPalette(c("cornflowerblue", 'lemonchiffon1', "brown2"))(input$classes)
            } else {
              col_def <- colorRampPalette(c("#ffffcc", "#c2e699", '#78c679', "#238443"))(input$classes)
            }
          }
          if(verbose){message('generate plot')}
          plot <- terra::plot(data.raster, col=col_def,
                              fun=function()terra::lines(borders),
                              breaks = s(),
                              xlim = ranges$x, ylim = ranges$y)
        }
        plot
      })
    })

    #end check input netCDF
  })

}
