#install all packages if not yet installed
packages_needed <- c('stringr', 'shiny', 'shinyFiles', 'checkmate', 'withr')

for (pack in packages_needed){
  t <- tryCatch(
    {find.package(pack)},
    error = function(e){
        install.packages(pack)
    }
  )
}

#load libraries
library(shiny)
library(stringr)
library(checkmate)
library(withr)
library(shinyFiles)

#get volumes----

getVols <- function(){
  exclude <- ''
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    volumes <- dir_ls("/Volumes")
    names(volumes) <- basename(volumes)
  }
  else if (osSystem == "Linux") {
    volumes <- c(Computer = "/")
    if (isTRUE(dir_exists("/media"))) {
      media <- dir_ls("/media")
      names(media) <- basename(media)
      volumes <- c(volumes, media)
    }
  }
  else if (osSystem == "Windows") {
    wmic <- paste0(Sys.getenv("SystemRoot"), "\\System32\\Wbem\\WMIC.exe")
    if (!file.exists(wmic)) {
      volumes_info <- system2("powershell", "$dvr=[System.IO.DriveInfo]::GetDrives();Write-Output $dvr.length $dvr.name $dvr.VolumeLabel;",
                              stdout = TRUE)
      num <- as.integer(volumes_info[1])
      if (num == 0)
        return(NULL)
      mat <- matrix(volumes_info[-1], nrow = num, ncol = 2)
      mat[, 1] <- gsub(":\\\\$", ":/", mat[, 1])
      sel <- mat[, 2] == ""
      mat[sel, 2] <- mat[sel, 1]
      volumes <- mat[, 1]
      volNames <- mat[, 2]
      volNames <- paste0(volNames, " (", gsub(":/$", ":",
                                              volumes), ")")
    }
    else {
      volumes <- system(paste(wmic, "logicaldisk get Caption"),
                        intern = TRUE, ignore.stderr = TRUE)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system(paste(wmic, "/FAILFAST:1000 logicaldisk get VolumeName"),
                         intern = TRUE, ignore.stderr = TRUE)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
    }
    names(volumes) <- volNames
    volumes <- gsub(":$", ":/", volumes)
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  return(volumes)
}

ui <- fluidPage(
  #set style
  # tags$style('.tags { width: 80%;}'),

  #set theme
  theme = bslib::bs_theme(bootswatch = "flatly"),

  #title
  titlePanel("EBV netCDF Metadata"),

  #create several tabs
  tabsetPanel(
    #1. tab: general information----
    tabPanel('General Information',
             #2 cols
             fluidRow(
               #title
               column(6,
                      textInput('title', tags$span(style="font-size: 18px; font-weight: bold", 'Title*'), width='80%',
                                placeholder='The title of the dataset.'),
                      #span(textOutput("title_desc"), style="font-size:14px")
                      ),
               #date of creation
               column(6,
                      dateInput('date_created', tags$span(style="font-size: 18px; font-weight: bold", 'Date of creation*'), width='80%', max = Sys.Date())
                      )
               ),

    #Summary
    textAreaInput('summary', tags$span(style="font-size: 18px; font-weight: bold", 'Summary*'), width='90.5%',
                  placeholder = 'A paragraph describing the dataset. Allowed: 1500 characters'),
    span(textOutput("summary_error"), style="color:red"),
    #actionButton("summary_example", "Look at an example for a summary."),

    #references
    textAreaInput('references', tags$span(style="font-size: 18px; font-weight: bold", 'References'), width='90.5%',
                  placeholder = 'Provide the DOI URL of the dataset and/or associated publications. You can add several DOIs by seperating them by comma.'),

    #Methods
    textAreaInput('methods', tags$span(style="font-size: 18px; font-weight: bold", 'Methods*'), width='90.5%',
                  placeholder = 'The method of production of the original data. If it was model-generated, source should name the model and its version. If it is observational, source should characterize it.'),

    #2 cols
      #coverage content type

    selectInput('coverage_content_type', tags$span(style="font-size: 18px; font-weight: bold", 'Coverage Content Type*'), width='90.5%',
                c("Image" = "image",
                  "Thematic Classification" = "thematicClassification",
                  "Physical Measurement" = "physicalMeasurement",
                  "Auxiliary Information" = "auxiliaryInformation",
                  "Quality Information" = "qualityInformation",
                  "Reference Information" = "referenceInformation",
                  "Model Result" = "modelResult",
                  "Coordinate" = "coordinate"),
                multiple = TRUE
    ),
    span(textOutput("cct_desc"), style="font-size:14px"),


    #2 cols
    fluidRow(
      #project name
      column(6,
             textInput('project_name', tags$span(style="font-size: 18px; font-weight: bold", 'Project Name'), width='80%',
                       value = '',
                       placeholder='The name(s) of the Project principally responsible for originating this data. Several values should be separated by comma.'),
             #span(textOutput("title_desc"), style="font-size:14px")
      ),
      #project url
      column(6,
             textInput('project_url', tags$span(style="font-size: 18px; font-weight: bold", 'Project URL'), width='80%',
                       value = "",
                       placeholder='The URL from the project(s) website. Several values should be separated by comma.')
      )
    ),

      #2 cols
      fluidRow(
        #Creator Name
        column(6,
               textInput('creator_name', tags$span(style="font-size: 18px; font-weight: bold", 'Creator Name*'), width='80%',
                         placeholder='The name of the person or other creator type principally responsible for creating this data.'),

        ),
        #Creator Email
        column(6,
               textInput('creator_email', tags$span(style="font-size: 18px; font-weight: bold", 'Creator Email'), width='80%',
                         placeholder='The email of the person or other creator type principally responsible for creating this data.')
        )
      ),


    #Creator Institution
    textAreaInput('creator_institution', tags$span(style="font-size: 18px; font-weight: bold", 'Creator Institution*'), width='90.5%',
                  placeholder = 'Name of the institution of the creator.'),

    #2 cols
    fluidRow(
      #publisher Name
      column(6,
             textInput('publisher_name', tags$span(style="font-size: 18px; font-weight: bold", 'Publisher Name* (responsible PI)'), width='80%',
                       placeholder='The name of the person publishing the data. The publisher is also the contact person.'),

      ),
      #publisher Email
      column(6,
             textInput('publisher_email', tags$span(style="font-size: 18px; font-weight: bold", 'Publisher Email*'), width='80%',
                       placeholder='The email of the person publishing the data. The publisher is also the contact person.')
      )
    ),


    #Publisher Institution
    textAreaInput('publisher_institution', tags$span(style="font-size: 18px; font-weight: bold", 'Publisher Institution*'), width='90.5%',
                  placeholder = 'Name of the institution of the publisher.'),

    #Co-creators
    textAreaInput('contributor_names', tags$span(style="font-size: 18px; font-weight: bold", 'Co-creators '), width='90.5%',
                  placeholder = 'The names of the co-creators responsible for creating this data. Seperate several by comma.'),

    #license
    selectInput("license", tags$span(style="font-size: 18px; font-weight: bold", "License*"),
                c('CC0'= 'https://creativecommons.org/publicdomain/zero/1.0/',
                  'CC BY'= 'https://creativecommons.org/licenses/by/4.0/',
                  'CC BY-SA'= 'https://creativecommons.org/licenses/by-sa/4.0/',
                  'CC BY-NC'= 'https://creativecommons.org/licenses/by-nc/4.0/',
                  'CC BY-NC-SA'= 'https://creativecommons.org/licenses/by-nc-sa/4.0/',
                  'CC BY-ND'= 'https://creativecommons.org/licenses/by-nd/4.0/',
                  'CC BY-NC-ND'= 'https://creativecommons.org/licenses/by-nc-nd/4.0/',
                  'other' = 'other'),
                width='80%'),

    #other licesense
    uiOutput('license_other'),

    #comment
    textAreaInput('comment', tags$span(style="font-size: 18px; font-weight: bold", 'Comment'), width='90.5%',
                  placeholder = 'Miscellaneous information about the data, not captured elsewhere.'),


    #end tabPanel 1
    ),

    #2. tab: ebv attributes----
    tabPanel('EBV Attributes',


             #2 cols
             fluidRow(
               #EBV Class and EBV Name
               column(6,
                      #EBV Class
                      selectInput("ebv_class", tags$span(style="font-size: 18px; font-weight: bold", "EBV Class"),
                                  c("Genetic composition" = "Genetic composition",
                                    "Species populations" = "Species populations",
                                    "Species traits" = "Species traits",
                                    "Community composition"="Community composition",
                                    "Ecosystem functioning"="Ecosystem functioning",
                                    "Ecosystem structure"="Ecosystem structure",
                                    "None"="N/A"),
                                  width='80%'),

                      #EBV Name - in server: create selectInput based on EBV Class
                      uiOutput('ebv_name'),

                      #environmental domain
                      selectInput('ebv_domain', tags$span(style="font-size: 18px; font-weight: bold", 'Environmental Domain*'), width='80%',
                                  c("Terrestrial" = "Terrestrial",
                                    "Marine" = "Marine",
                                    "Freshwater" = "Freshwater",
                                    "Other" = "Other",
                                    "None" = "N/A"),
                                  #selected = 'N/A',
                                  multiple = TRUE
                      ),
                      #define other for environmental domain
                      uiOutput('ebv_domain_other'),

                      #terranova type (keywords)
                      selectInput('terranova_type', tags$span(style="font-size: 18px; font-weight: bold", 'Type of Data*'), width='80%',
                                  c("Climate " = "climate",
                                    "Biodiversity" = "biodiversity ",
                                    'Anthropogenic activity'='anthropogenic activity ')
                      ),


               ),
               #Biological entity
               column(6,
                      selectInput("biological_entity", tags$span(style="font-size: 18px; font-weight: bold", "Entity type"),
                                  c("None"="N/A",
                                    "Species" = "Species",
                                    "Communities" = "Communities",
                                    "Ecosystems" = "Ecosystems",
                                    "Other"="Other"
                                   ),
                                  width='80%'),
                      uiOutput('entity_other'),
                      uiOutput('entity_scope'),
                      uiOutput('entity_classification_name'),
                      uiOutput('entity_classification_ref')
                      )
             ),

             #2 cols
             fluidRow(
               #metric
               column(6,
                      span(textOutput("metric_header"), style="font-size:21px; font-weight: bold"),
                      numericInput('metric_no', 'Please inicate the amount of Metrics. Minimum 1.', value = 1,
                                   width = '80%', min = 1, max = 10),
                      column(10,
                             span("Note: First choose the correct number of metrics and then start typing.\n
                             Whenever you adjust the number of metrics, all the fields below will be cleaned up."
                                  ), #the aditional column makes the width 80%
                             ),
                      #span(textOutput("metric_desc"), style="font-size:16px"),
                      uiOutput('metric_container')
                      ),
               #scenario
               column(6,
                      span(textOutput("scenario_header"), style="font-size:21px; font-weight: bold"),
                      numericInput('scenario_no', 'Please inicate the amount of Scenarios. None needed.', value = 0,
                                   width = '80%', min = 0, max = 10),
                      span(textOutput("scenario_desc"), style="font-size:14px"),
                      uiOutput('scenario_container'),

                      )
             )

             #end 2. tab
             ),
    #3. tab: spat. temp. attributes----
    tabPanel('Spatial and temporal domain',
             #2 cols
             fluidRow(
               #spatial domain
               column(6,
                      #tags$b('Spatial Domain'),

                      #spatial scope
                      selectInput("spatial_scope", tags$span(style="font-size: 18px; font-weight: bold", "Spatial Scope*"),
                                  c("Global" = "Global",
                                    "Continental/Regional" = "Continental/Regional",
                                    "National" = "National",
                                    "Sub-national/Local"="Sub-national/Local"),
                                  width='80%'),

                      #spatial description
                      uiOutput('spatial_desc'),

                      #spatial units
                      selectInput("spatial_units", tags$span(style="font-size: 18px; font-weight: bold", "Spatial Units*"),
                                  c("degree" = "degree",
                                    "meter" = "meter"),
                                  width='80%'),


                      ),
               #temporal domain
               column(6,
                      #tags$b('Temporal Domain'),

                      #temporal resoultion
                      selectInput("temporal_resolution", tags$span(style="font-size: 18px; font-weight: bold", "Temporal Resolution*"),
                                  c("decadal" = "P0010-00-00",
                                    "annually" = "P0001-00-00",
                                    "monthly" = "P0000-01-00",
                                    "weekly" = "P0000-00-07",
                                    "daily" = "P0000-00-01",
                                    'one timestep' = "P0000-00-00",
                                    "irregular"  = "Irregular",
                                    "paleo" = "Paleo",
                                    "other" = "other"),
                                  width='80%'),

                      #other temporal resolution
                      column(10, uiOutput('temporal_resoultion_other')), #the additional column makes the width 80%

                      #irregular temporal resolution
                      uiOutput('temporal_resoultion_irregular'),

                      #temporal extent
                      uiOutput('temporal_extent'),

                      )
             )

             #end 3. tab
             ),
    #4. tab: check and output generation----
    tabPanel('Save metadata to file',

             column(10,
                    #some space
                    tags$div(
                      style="margin-bottom:10px;"
                    ),
                    span("First, check if you entered all metadata successfully.", style='font-size: 18px'
                    ),
                    #some space
                    tags$div(
                      style="margin-bottom:10px;"
                    ),
                    #button for metadata check
                    actionButton('check_metadata', label = "Check your metadata.", class = "btn-lg btn-success"),

                    #some space
                    tags$div(
                      style="margin-bottom:7px;"
                    ),

                    #shows missing input/successful filled metadata message
                    verbatimTextOutput("value"),

                    #create save button when all checks are done
                    uiOutput('field_name'),
                    uiOutput('save_btn'),
             ),




             # shinySaveButton("save_json", "Save JSON file", "Save file as ...", filetype=list(json="json"), class = "btn-lg btn-success"),

             # textInput('outputpath', tags$span(style="font-size: 18px; font-weight: bold", 'Outputpath*')), #value='C:\\Users\\lq39quba\\Desktop\\ebv_terranova\\bla.json'
             # actionButton('create_json', label = "Create JSON file.", class = "btn-lg btn-success"),


             )
  )
)

server <- function(input, output, session) {
  #reactive values----
  react_values <- reactiveValues()
  #constant value
  nodata <- 'N/A'

  #check summary length
  summary_text <- reactive({input$summary})
    output$summary_error <- renderText({
      if(nchar(summary_text())>1500){
        "Your summary is too long! Max. 1500 characters."
      }
      })

    #add info to cct
    output$cct_desc <- renderText({'The coverage content type indicates the source of the data. Several options may be selected.'})

    #Get EBV Class to define EBV Names
    output$ebv_name <- renderUI({
      if(input$ebv_class=='Genetic composition'){
        selectInput("ebv_name", tags$span(style="font-size: 18px; font-weight: bold", "EBV Name"),
                    c("Genetic diversity (richness and heterozygosity)" = "Genetic diversity",
                      "Genetic differentiation (number of genetic units and genetic distance)"="Genetic differentiation",
                      "Effective population size"="Effective population size",
                      "Inbreeding"="Inbreeding",
                      "None"="N/A"),
                    width='80%')
      }else if(input$ebv_class=='Species populations'){
        selectInput("ebv_name", "EBV Name",
                    c('Species distributions'='Species distributions',
                      'Species abundances'='Species abundances',
                      'None'='N/A'),
                    width='80%')
      }else if(input$ebv_class=='Species traits'){
        selectInput("ebv_name", "EBV Name",
                    c('Morphology'='Morphology',
                      'Physiology'='Physiology',
                      'Phenology'='Phenology',
                      'Movement'='Movement',
                      'None'='N/A'),
                    width='80%')
      }else if(input$ebv_class=='Community composition'){
        selectInput("ebv_name", "EBV Name",
                    c('Community abundance'='Community abundance',
                      'Taxonomic/phylogenetic diversity'='Taxonomic/phylogenetic diversity',
                      'Trait diversity'='Trait diversity',
                      'Interaction diversity'='Interaction diversity',
                      'None'='N/A'),
                    width='80%')
      }else if(input$ebv_class=='Ecosystem functioning'){
        selectInput("ebv_name", "EBV Name",
                    c('Primary productivity'='Primary productivity',
                      'Ecosystem phenology'='Ecosystem phenology',
                      'Ecosystem disturbances'='Ecosystem disturbances',
                      'None'='N/A'),
                    width='80%')
      }else if(input$ebv_class=='Ecosystem structure'){
        selectInput("ebv_name", "EBV Name",
                    c('Live cover fraction'='Live cover fraction',
                      'Ecosystem distribution'='Ecosystem distribution',
                      'Ecosystem Vertical Profile'='Ecosystem Vertical Profile',
                      'None'='N/A'),
                    width='80%')
      }else if(input$ebv_class=='N/A'){
        selectInput("ebv_name", "EBV Name",
                    c('None'='N/A'),
                    width='80%')
      }

    })

    #add UI for ebv_domain
    output$ebv_domain_other <- renderUI({
      e <- tryCatch(
        {
          if('Other' %in% input$ebv_domain){
          textInput('ebv_domain_other_txt', tags$span(style="font-weight: bold", 'Other environmental domain*'), width='80%',
                    placeholder = 'Name of other environmental domain here.')}
          },
          error = function(e){}
        )
    })

    #add UI for biological entity
    output$entity_other <- renderUI({
        if(input$biological_entity=='Other'){
          textAreaInput('entity_other_txt', 'Other', width='90.5%',
                        placeholder = 'Name of the other entity type.')
        }
    })

    output$entity_scope <- renderUI({
      if(input$biological_entity!='N/A'){
        textAreaInput('entity_scope_txt', tags$span(style="font-weight: bold", 'Entity Scope'), width='90.5%',
                      placeholder = 'A description of the range of taxa or ecosystem types addressed in the dataset. E.g. "300 species of mammals”, “Forests”, etc..')
      }
    })

    output$entity_classification_name <- renderUI({
      if(input$biological_entity!='N/A'){
        textInput('entity_classification_name_txt', tags$span(style="font-weight: bold", 'Classification System Name'), width='90.5%',
                      placeholder = 'E.g. Linnaean classification')
      }
    })

    output$entity_classification_ref <- renderUI({
      if(input$biological_entity!='N/A'){
        textInput('entity_classification_ref_txt', tags$span(style="font-weight: bold", 'Classification System Reference'), width='90.5%',
                  placeholder = 'Reference of the classification system as a URL.')
      }
    })

    #metric definition
    output$metric_header <- renderText({'Metric*'})
    #output$metric_desc <- renderText({'Provide the name, description and units of the metric(s).'})

    #create input fields for all metrics
    output$metric_container <- renderUI({
      out <- lapply(1:input$metric_no, function(i) {

        #linebreak + header per metric
        b <- tags$br()
        h <- tags$b(tags$span(style="font-size: 17px", paste0('Metric ', i)))

        #standard_name
        sn <- textInput(paste0('metric_standard_name_', i), 'Name*', width='80%',
                  placeholder = paste0('Name of Metric ', i))


        #description/long_name
        ln <- textAreaInput(paste0('metric_long_name_', i), 'Description*', width='80%',
                      placeholder = paste0('Description of Metric ', i))

        #units
        u <- textInput(paste0('metric_units_', i), 'Units*', width='80%',
                  placeholder = paste0('Units of Metric ', i))

        return(list(b, h, sn, ln, u))

        })

      })

    #scenario definition
    output$scenario_header <- renderText({'Scenario'})
    output$scenario_desc <- renderText({
      if(input$scenario_no>0){
        "If applicable, name the scenario's classification system, the version and provide a URL to the reference."
      }
    })

    #ask for scenario classification
    output$scenario_container <- renderUI({
      if(input$scenario_no>0){
        #create input fields for all scenarios
        out <- lapply(1:input$scenario_no, function(i) {

          if(i==1){
            scn <- textInput('scenario_classification_name', 'Classification Name', width='90.5%',
                      placeholder = 'Name of scenario classification system, e.g. SSP')

            scv <- textInput('scenario_classification_version', 'Classification Version', width='90.5%',
                           placeholder = 'Version of scenario classification system, e.g. 1.0')

            scu <- textInput('scenario_classification_url', 'Classification URL', width='90.5%',
                             placeholder = 'Reference of scenario classification system, e.g. https://www.aza.org/species-survival-plan-programs')
          }

          #linebreak + header per scenario
          b <- tags$br()
          h <- tags$b(tags$span(style="font-size: 17px", paste0('Scenario ', i)))

          #standard_name
          sn <- textInput(paste0('scenario_standard_name_', i), 'Name*', width='90.5%',
                          placeholder = paste0('Name of Scenario ', i))


          #description/long_name
          ln <- textAreaInput(paste0('scenario_long_name_', i), 'Description', width='90.5%',
                              placeholder = paste0('Description of Scenario ', i))

          if(i==1){
            return(list(scn, scv, scu, b, h, sn, ln))
          }else{
            return(list(b, h, sn, ln))
          }

        })
      }

    })

    #spatial description
    output$spatial_desc <- renderUI({
      if(input$spatial_scope!='Global'){
        textInput('spatial_desc_txt', tags$span(style="font-size: 18px; font-weight: bold", 'Spatial Description*'), width='80%',
                  placeholder='Name(s)/description of the continent/region/country/area')
      }
    })

    #other license url
    output$license_other <- renderUI({
      if(input$license=='other'){
        t <- renderText('Provide a license for you dataset as a URL.')

        s <- textInput('license_other_txt', '', width='80%', placeholder='https://www.gnu.org/licenses/gpl-3.0.html')
        return(list(t, s))
      }
    })


    #other temporal resoultion
    output$temporal_resoultion_other <- renderUI({
      if(input$temporal_resolution=='other'){
        t <- renderText('Provide the definition of your temporal resolution in the ISO 8601:2004 duration format P(YYYY)-(MM)-(DD).
                    Examples: decadal: P0010-00-00 and daily: P0000-00-01')

        s <- textInput('temp_res_txt', '', width='80%', placeholder='P0000-00-00')
        return(list(t, s))
      }
    })
    output$temporal_resoultion_irregular <- renderUI({
      if(input$temporal_resolution=='Irregular' | input$temporal_resolution=='Paleo'){
        if(input$temporal_resolution=='Irregular'){
          t <- renderText('Provide the definition of your timesteps as a comma-separated list in the format YYYY-MM-DD or short YYYY.')
          s <- textInput('temp_res_irr', '', width='80%', placeholder='YYYY, YYYY, ...')
          return(list(t, s))
        }else{
          t <- renderText('Provide the definition of your timesteps as a comma-separated list. The values will represent "kyr B.P.".')
          s <- textInput('temp_res_irr', '', width='80%', placeholder='126, 125, ...')
          return(list(t, s))
        }
      }
    })


    #check variables
    to_do_list <- c()
    create <- TRUE

    #temporal extent----
    output$temporal_extent <- renderUI({
      if(input$temporal_resolution!='Irregular' & input$temporal_resolution!='Paleo'){
        if(input$temporal_resolution=='other'){
          #user defined temporal resolution
          if(grepl('^P{1}\\d{4}-00-00', input$temp_res_txt)){
            dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                              'Start and end date of the dataset*'),
                           format = "yyyy")
          } else if(grepl('^P{1}\\d{4}-\\d{2}-00$', input$temp_res_txt)){
            dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                              'Start and end date of the dataset*'),
                           format = "mm-yyyy")
          } else if (grepl('^P{1}\\d{4}-\\d{2}-\\d{2}$', input$temp_res_txt)){
            dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                              'Start and end date of the dataset*'),
                           format = "dd-mm-yyyy")
          }else{
            create <- FALSE
            to_do_list <- c(to_do_list, 'Your temporal resolution does not match the ISO duration format.')
          }

        }else if(input$temporal_resolution=='P0000-00-00'){
          #one timestep only
          dateInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                            'Date of the dataset*'), width='80%',
                    startview ='year')

        }else if(grepl('^P{1}\\d{4}-00-00$', input$temporal_resolution)){
          dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                            'Start and end date of the dataset*'),
                         format = "yyyy")
        } else if(grepl('^P{1}\\d{4}-\\d{2}-00$', input$temporal_resolution)){
          dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                            'Start and end date of the dataset*'),
                         format = "mm-yyyy")
        } else {
          dateRangeInput('temporal_extent_input', tags$span(style="font-size: 18px; font-weight: bold",
                                                            'Start and end date of the dataset*'),
                         format = "dd-mm-yyyy")
        }
      }

    })

    #observe outputpath----
    observeEvent(input$save_json, {
      # volumes <- c("Current Folder"=".", getVols())
      # shinyFileSave(input, "save_json", roots=volumes, session=session)
      # fileinfo <- parseSavePath(volumes, input$save_json)

      # print(input$save_json)
      if (nchar(input$save_json)>0 && endsWith(input$save_json, '.json')) {
        root <- './json-data/'
        react_values$outputpath <- file.path(root, as.character(input$save_json))
        # print(react_values$outputpath)

        output$save_btn <- renderUI({
          downloadButton("download", label = "Download")
          # actionButton('download', label = "Download File now", class = "btn-lg btn-success")
        })

        #create json----
        #create metric
        if(input$metric_no>0){
          ebv_metric <- ''
          for(i in 1:input$metric_no){
            ebv_metric <- paste0(ebv_metric, '\n\t\t\t\t"ebv_metric_', i, '": {
                    ":standard_name": "', eval(parse(text = paste0('input$metric_standard_name_', i))), '",
                    ":long_name": "', eval(parse(text = paste0('input$metric_long_name_', i))), '",
                    ":units": "', eval(parse(text = paste0('input$metric_units_', i))), '"\n\t\t\t\t}')
            if(i != input$metric_no){
              ebv_metric <- paste0(ebv_metric, ',')
            }
          }
        }

        #create scenario
        if (input$scenario_no > 0){
          ebv_scenario <- paste0('"ebv_scenario": {
                "ebv_scenario_classification_name": "', react_values$scenario_classification_name, '",
                "ebv_scenario_classification_version": "', react_values$scenario_classification_version, '",
                "ebv_scenario_classification_url": "', react_values$scenario_classification_url, '",')
          for(i in 1:input$scenario_no){
            if(!is.null(need(eval(parse(text = paste0('input$scenario_long_name_', i))) != '', TRUE))){
              long_name <- nodata
            } else{
              long_name <- eval(parse(text = paste0('input$scenario_long_name_', i)))
            }
            ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t\t"ebv_scenario_', i, '": {
                    ":standard_name": "', eval(parse(text = paste0('input$scenario_standard_name_', i))), '",
                    ":long_name": "', long_name, '"
                }')
            if(i != input$scenario_no){
              ebv_scenario <- paste0(ebv_scenario, ',')
            }
          }
          ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t},')
        } else {
          ebv_scenario <- '"ebv_scenario": "N/A",'
        }


        #create json
        react_values$json <- paste0('{
    "data": [
        {
            "id": "pending",
            "naming_authority": "The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
            "title": "', input$title, '",
            "date_created": "', input$date_created, '",
            "summary": "', input$summary, '",
            "references": [\n\t\t\t\t"', paste0(unlist(react_values$references), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "source": "', input$methods, '",
            "coverage_content_type": [\n\t\t\t\t"', paste0(input$coverage_content_type, collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "project": "', react_values$project_name, '",
            "project_url": "', react_values$project_url, '",
            "creator": {
                "creator_name": "', input$creator_name, '",
                "creator_email": "', react_values$creator_email, '",
                "creator_institution": "', input$creator_institution, '"
            },
            "contributor_name": [\n\t\t\t\t"', paste0(stringr::str_remove_all(stringr::str_split(react_values$contributors_names, ',')[[1]], ' '), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "license": "', input$license, '",
            "publisher": {
                "publisher_name": "', input$publisher_name, '",
                "publisher_email": "', input$publisher_email, '",
                "publisher_institution": "', input$publisher_institution, '"
            },
            "ebv": {
                "ebv_class": "', react_values$ebv_class, '",
                "ebv_name": "', react_values$ebv_name, '"
            },
            "ebv_entity": {
                "ebv_entity_type": "', react_values$biological_entity, '",
                "ebv_entity_scope": "', react_values$entity_scope_txt, '",
                "ebv_entity_classification_name": "', react_values$entity_classification_name_txt, '",
                "ebv_entity_classification_url": "', react_values$entity_classification_ref_txt, '"
            },
            "ebv_metric": {',
                       ebv_metric,
                       '\n\t\t\t},\n\t\t\t',
                       ebv_scenario,
                       '\n\t\t\t"ebv_geospatial": {
                "ebv_geospatial_scope": "', input$spatial_scope, '",
                "ebv_geospatial_description": "', react_values$spatial_desc_txt, '"
            },
            "geospatial_lat_units": "', input$spatial_units, '",
            "geospatial_lon_units": "', input$spatial_units, '",
            "time_coverage": {
                "time_coverage_resolution": "', react_values$t_res, '",
                "time_coverage_start": "', react_values$t_start, '",
                "time_coverage_end": "', react_values$t_end, '"
            },
            "ebv_domain": [\n\t\t\t\t"', paste0(react_values$ebv_domain_value, collapse = '",\n\t\t\t\t"'), '"\n\t\t\t],
            "comment": "', react_values$comment, '",
            "terranova_type": "', input$terranova_type, '",
            "timesteps": [\n\t\t\t\t', react_values$timesteps_irr, '\n\t\t\t]
        }
    ]
}')
        # print(react_values$outputpath)
        #write to file and set encoding ----
        # withr::with_options(list(encoding = "UTF-8"), write(react_values$json, react_values$outputpath))
        # print('done')
        # print(react_values$json)
      }

    })

    #download data ----
    output$download <- downloadHandler(filename = function() {basename(react_values$outputpath)},
                    content = function(file){
                      withr::with_options(list(encoding = "UTF-8"), write(react_values$json, file))
                    })

    #metadata check button clicked -----
    observeEvent(input$check_metadata, {
      #start checks----

          #check title
          if(nchar(input$title)==0){
            to_do_list <- c('You must give a title.', to_do_list)
            create <- FALSE

          } else if(!is.na(suppressWarnings(as.numeric(input$title)))){
            to_do_list <- c('Your title must contain characters.', to_do_list)
            create <- FALSE
          }

          #check summary
          if(nchar(input$summary)==0){
            to_do_list <- c('You must provide a summary.', to_do_list)
            create <- FALSE

          } else if(!is.na(suppressWarnings(as.numeric(input$summary)))){
            to_do_list <- c('Your summary must contain characters.', to_do_list)
            create <- FALSE
          }
          if (nchar(input$summary)!=0 & nchar(input$summary)>1500){
            to_do_list <- c('Your summary is too long. Max. 1500 characters.', to_do_list)
            create <- FALSE
          }

          #check references - not mandatory
          if(nchar(input$references)==0){
            react_values$references <- nodata
          }else{
            react_values$references <- stringr::str_split(input$references, ',')[[1]]
            react_values$references <- stringr::str_remove_all(react_values$references, ' ')
          }

          #check methods
          if(nchar(input$methods)==0){
            to_do_list <- c('You must provide a description of the method.', to_do_list)
            create <- FALSE

          } else if(!is.na(suppressWarnings(as.numeric(input$methods)))){
            to_do_list <- c('You method must contain characters.', to_do_list)
            create <- FALSE
          }

          #check content coverage type
          if(!is.null(need(input$coverage_content_type != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to choose at least one value for the content coverage type.')
            create <- FALSE
          }

          #check project_name
          if(!is.null(need(input$project_name != '', TRUE))){
            react_values$project_name <- nodata
          }else{
            react_values$project_name <- input$project_name
          }

          #check project_url
          if(!is.null(need(input$project_url != '', TRUE))){
            react_values$project_url <- nodata
          }else{
            react_values$project_url <- input$project_url
          }

          #check creator_name
          if(!is.null(need(input$creator_name != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a creator name.')
            create <- FALSE
          }

          #check creator_email
          if(!is.null(need(input$creator_email != '', TRUE))){
            react_values$creator_email <- nodata
          }else{
            react_values$creator_email <- input$creator_email
          }

          #check creator_institution
          if(!is.null(need(input$creator_institution != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a creator institution.')
            create <- FALSE
          }

          #check publisher_name
          if(!is.null(need(input$publisher_name != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a publisher name.')
            create <- FALSE
          }

          #check publisher_email
          if(!is.null(need(input$publisher_email != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a publisher email.')
            create <- FALSE
          }

          #check publisher_instiution
          if(!is.null(need(input$publisher_institution != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a publisher institution.')
            create <- FALSE
          }

          #check contributors_names
          if(!is.null(need(input$contributor_names != '', TRUE))){
            react_values$contributors_names <- nodata
          }else if(length(contributors_names)==0){
            react_values$contributors_names <- nodata
          }else{
            react_values$contributors_names <- input$contributor_names
          }

          #check license
          if(input$license =='other'){
            t_res <- input$license_other_txt
            if(!is.null(need(input$license_other_txt != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to define the license URL.')
              create <- FALSE
            }
          }else{
            t_res <-input$license
            if(!is.null(need(input$license != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to select a license.')
              create <- FALSE
            }
          }

          #check comment
          if(!is.null(need(input$comment != '', TRUE))){
            react_values$comment <- nodata
          }else{
            react_values$comment <- input$comment
          }

          #check ebv_class
          if(!is.null(need(input$ebv_class != '', TRUE))){
            react_values$ebv_class <- nodata
          }else{
            react_values$ebv_class <- input$ebv_class
          }

          #check ebv_name
          if(!is.null(need(input$ebv_name != '', TRUE))){
            react_values$ebv_name <- nodata
          }else{
            react_values$ebv_name <- input$ebv_name
          }

          #check ebv_domain
          if(!is.null(need(input$ebv_domain != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide an environmental domain.')
            create <- FALSE
          }else{
            if('Other' %in% input$ebv_domain){
              if(!is.null(need(input$ebv_domain_other_txt != '', TRUE))){
                to_do_list <- c(to_do_list, 'You need to provide a description of the other environmental domain.')
                create <- FALSE
              }
              #create ebv_domain value
              react_values$ebv_domain_value <- c(input$ebv_domain[! input$ebv_domain %in% c('Other')], input$ebv_domain_other_txt)

            }else{
              #create ebv_domain value
              react_values$ebv_domain_value <- c(input$ebv_domain)
              }
            }

          #check biological_entity
          if(!is.null(need(input$biological_entity != '', TRUE))){
            react_values$biological_entity <- nodata
          }else{
            react_values$biological_entity <- input$biological_entity
          }

          #check entity_scope
          if(react_values$biological_entity=='Other'){
            if(!is.null(need(input$entity_other_txt != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to provide a description of the entity type as you chose Other.')
              create <- FALSE
              }
            } else{
              #entity_other_txt
              if(!is.null(need(input$entity_other_txt != '', TRUE))){
                react_values$entity_other_txt <- nodata
              }else{
                react_values$entity_other_txt <- input$entity_other_txt
              }
            }

              #entity_class_name
              if(!is.null(need(input$entity_scope_txt != '', TRUE))){#HERE
                react_values$entity_scope_txt <- nodata
              }else{
                react_values$entity_scope_txt <- input$entity_scope_txt
              }

              #entity_class_name
              if(!is.null(need(input$entity_classification_name_txt != '', TRUE))){
                react_values$entity_classification_name_txt <- nodata
              }else{
                react_values$entity_classification_name_txt <- input$entity_classification_name_txt
              }

              #entity_class_url
              if(!is.null(need(input$entity_classification_ref_txt != '', TRUE))){
                react_values$entity_classification_ref_txt <- nodata
              }else{
                react_values$entity_classification_ref_txt <- input$entity_classification_ref_txt
              }


          #check metric attributes
          if(input$metric_no>0){
            for(i in 1:input$metric_no){
              if(!is.null(need(eval(parse(text = paste0('input$metric_standard_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The name for metric ', i, ' is missing.'))
                create <- FALSE
              }
              if(!is.null(need(eval(parse(text = paste0('input$metric_long_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The description for metric ', i, ' is missing.'))
                create <- FALSE
              }
              if(!is.null(need(eval(parse(text = paste0('input$metric_units_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The units for metric ', i, ' are missing.'))
                create <- FALSE
              }

              }
          }

          #check scenario attributes
          if(input$scenario_no>0){
            for(i in 1:input$scenario_no){
              if(!is.null(need(eval(parse(text = paste0('input$scenario_standard_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The name for scenario ', i, ' is missing.'))
                create <- FALSE
              }
              # if(!is.null(need(eval(parse(text = paste0('input$scenario_long_name_', i))) != '', TRUE))){
              #   to_do_list <- c(to_do_list, paste0('The description for scenario ',i,' is missing.'))
              #   create <- FALSE
              # }
            }

            #scenario_classification_name
            if(!is.null(need(input$scenario_classification_name != '', TRUE))){
              react_values$scenario_classification_name <- nodata
            }else{
              react_values$scenario_classification_name <- input$scenario_classification_name
            }

            #scenario_classification_version
            if(!is.null(need(input$scenario_classification_version != '', TRUE))){
              react_values$scenario_classification_version <- nodata
            }else{
              react_values$scenario_classification_version <- input$scenario_classification_version
            }

            #scenario_classification_url
            if(!is.null(need(input$scenario_classification_url != '', TRUE))){
              react_values$scenario_classification_url <- nodata
            }else{
              react_values$scenario_classification_url <- input$scenario_classification_url
            }
          }

          #spatial scope
          if(!is.null(need(input$spatial_scope != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a value for the spatial scope.')
            create <- FALSE
          }

          #spatial description
          if(input$spatial_scope != 'Global'){
            if(!is.null(need(input$spatial_desc_txt != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to provide a description of the spatial scope.')
              create <- FALSE
            }else{
              react_values$spatial_desc_txt <- input$spatial_desc_txt
            }
          } else{
            react_values$spatial_desc_txt <- nodata
          }

          #temporal resolution
          if(input$temporal_resolution =='other'){
            react_values$t_res <- input$temp_res_txt
            if(!is.null(need(input$temp_res_txt != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to define the temporal resolution.')
              create <- FALSE
            }else{
              if(!grepl('^P\\d{4}-\\d{2}-\\d{2}$', input$temp_res_txt)){
                to_do_list <- c(to_do_list, 'The definition of the temporal resoultion is incorrect. Follow the format: PYYYY-MM-DD. Example: P0010-00-00.')
                create <- FALSE
              }
            }
          }else{
            react_values$t_res <- input$temporal_resolution
          }

          #check the input of the irregulare timesteps
          if(input$temporal_resolution=='Irregular'){
            react_values$timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            react_values$timestep_list <- gsub(' ', '', react_values$timestep_list) #remove whitespaces
            print(react_values$timestep_list)
            ts_irr_wrong <- FALSE
            for(ts in react_values$timestep_list){
              if(grepl('^\\d{4}-\\d{2}-\\d{2}$', ts) | grepl('^\\d{4}$', ts)){
                #pass
              }else{
                ts_irr_wrong <- TRUE
              }
            }
            if(ts_irr_wrong){
              to_do_list <- c(to_do_list, 'You chose >>Irregular<< temporal resolution and the input you provided does not match the required form: comma-separated YYYY-MM-DD or comma-separated YYYY. Please check.')
              create <- FALSE
            }
          }
          if(input$temporal_resolution=='Paleo'){
            react_values$timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            for(ts in react_values$timestep_list){
              if(is.na(suppressWarnings(as.numeric(ts)))){
                to_do_list <- c(to_do_list, paste0('You chose "paleo" temporal resolution. The value "', ts, '" does not seem to be a numeric value. Please check.'))
                create <- FALSE
              }
            }
          }

          #temporal extent
          if(input$temporal_resolution=='Irregular'){
            react_values$timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            react_values$t_start <- min(react_values$timestep_list)
            react_values$t_end <- max(react_values$timestep_list)
            react_values$timesteps_irr <- paste0('"', paste0(react_values$timestep_list, collapse = '", "'), '"')
          }else if(input$temporal_resolution=='Paleo'){
            react_values$timestep_list <- as.numeric(stringr::str_split(input$temp_res_irr, ',')[[1]])
            react_values$t_start <- max(react_values$timestep_list)
            react_values$t_end <- min(react_values$timestep_list)
            react_values$timesteps_irr <- paste0('"', paste0(react_values$timestep_list, collapse = '", "'), '"')
          }else if(input$temporal_resolution=='P0000-00-00'){
            react_values$t_start <-input$temporal_extent_input
            react_values$t_end <- input$temporal_extent_input
          }else{
            react_values$t_start <- input$temporal_extent_input[1]
            react_values$t_end <- input$temporal_extent_input[2]
          }
          if(input$temporal_resolution!='Irregular' & input$temporal_resolution!='Paleo'){
            react_values$timesteps_irr <- '"N/A"'
          }


          #check terranova type
          if(!is.null(need(input$terranova_type != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a value for the type of the data.')
            create <- FALSE
          }

          #output errors for user ----
          if(create==FALSE){
            to_do_list <- c('Your metadata contains the following errors:', to_do_list)
            output$value <- renderPrint({paste(to_do_list, sep='\n')})
          }

          #create json----
          if(create){
            #maybe here?
            output$field_name <- renderUI({
              textInput('save_json', span('Please enter the name of your json file (fileending should be *.json)', style='font-size: 18px'))
              # shinySaveButton("save_json", "Save JSON file", "Save file as ...", filetype=list(json="json"), class = "btn-lg btn-success")
            })

            #inform user
            output$value <- renderPrint({'All metadata terms were entered successfully.'})




          } #if create==TRUE

    })



}

shinyApp(ui, server)

#To Do
#pre-filled values for keywords: keywords:
