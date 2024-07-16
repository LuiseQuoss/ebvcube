#install all packages if not yet installed
t <- tryCatch(
  {find.package("shiny")},
  error = function(e){
    install.packages('shiny')
  }
)
t <- tryCatch(
  {find.package("stringr")},
  error = function(e){
    install.packages('stringr')
  }
)
t <- tryCatch(
  {find.package("checkmate")},
  error = function(e){
    install.packages('checkmate')
  }
)
t <- tryCatch(
  {find.package("withr")},
  error = function(e){
    install.packages('withr')
  }
)

#load libraries
library(shiny)
library(stringr)
library(checkmate)
library(withr)


ui <- fluidPage(
  #set style
  #tags$style(type='text/css', '.selectize-label { font-size: 60px;}'),
  #".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
  #tags$span(style="color:red", 'Summary*')

  #set theme
  theme = bslib::bs_theme(bootswatch = "flatly"),

  #title
  titlePanel("EBV netCDF Metadata - Terranova Atlas"),

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
                multiple = T
    ),
    span(textOutput("cct_desc"), style="font-size:14px"),


    #2 cols
    fluidRow(
      #project name
      column(6,
             textInput('project_name', tags$span(style="font-size: 18px; font-weight: bold", 'Project Name'), width='80%',
                       value = 'TERRANOVA - The European Landscape Learning Initiative',
                       placeholder='The name(s) of the Project principally responsible for originating this data. Several values should be separated by comma.'),
             #span(textOutput("title_desc"), style="font-size:14px")
      ),
      #project url
      column(6,
             textInput('project_url', tags$span(style="font-size: 18px; font-weight: bold", 'Project URL'), width='80%',
                       value = "https://www.terranova-itn.eu",
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
    textAreaInput('license', tags$span(style="font-size: 18px; font-weight: bold", 'License*'), width='90.5%',
                  value =  "https://creativecommons.org/licenses/by/4.0",
                  placeholder = 'Give the URL of a licence. Prefereble CC-License, e.g. https://creativecommons.org/licenses/by/4.0/'),
    #add a text: link to CC licenses
    tags$a(href="https://creativecommons.org/licenses/", "Link to CC licenses."),

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
                                  multiple = T
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
                                  c( "None"="N/A",
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
                                   width = '80%',min = 1, max = 10),
                      span(textOutput("metric_desc"), style="font-size:14px"),
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
                                  c("degrees" = "degrees",
                                    "meters" = "meters"),
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
                      uiOutput('temporal_resoultion_other'),

                      #irregular temporal resolution
                      uiOutput('temporal_resoultion_irregular'),

                      #temporal extent
                      uiOutput('temporal_extent'),

                      #tags$b('Temporal Extent'),
                      # dateInput('temp_start', tags$span(style="font-size: 18px; font-weight: bold",
                      #                                   'Start date of the dataset*'), width='80%',
                      #           startview ='year'),
                      # dateInput('temp_end', tags$span(style="font-size: 18px; font-weight: bold",
                      #                                 'End date of the dataset*'), width='80%',
                      #           startview ='year')



                      )
             )

             #end 3. tab
             ),
    #4. tab: check and output generation----
    tabPanel('Save metadata to file',

             textInput('outputpath', tags$span(style="font-size: 18px; font-weight: bold", 'Outputpath*')), #value='C:\\Users\\lq39quba\\Desktop\\ebv_terranova\\bla.json'

             actionButton('create_json', label = "Create JSON file.", class = "btn-lg btn-success"),

             verbatimTextOutput("value")
             )
  )
)

server <- function(input, output) {

  nodata <- 'N/A'
  # #show summary example
  # observeEvent(input$summary_example, {
  #   output$summary_error <- renderText({'jey'})
  #   showModal(modalDialog(
  #     title= "Summary Example",
  #     text = "Add a summary Example",
  #     type = 'info',
  #     closeOnClickOutside = T,
  #     easyClose = T
  #   ))
  # })

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
    output$metric_desc <- renderText({'Provide the name, description and units of the metric(s).'})

    #create input fields for all metrics
    output$metric_container <- renderUI({
      out <- lapply(1:input$metric_no, function(i) {

        #linebreak + header per metric
        b <- tags$br()
        h <- tags$b(tags$span(style="font-size: 17px", paste0('Metric ', i)))

        #standard_name
        sn <- textInput(paste0('metric_standard_name_',i), 'Name*', width='90.5%',
                  placeholder = paste0('Name of Metric ', i))


        #description/long_name
        ln <- textAreaInput(paste0('metric_long_name_',i), 'Description*', width='90.5%',
                      placeholder = paste0('Description of Metric ', i))

        #units
        u <- textInput(paste0('metric_units_',i), 'Units*', width='90.5%',
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
          sn <- textInput(paste0('scenario_standard_name_',i), 'Name*', width='90.5%',
                          placeholder = paste0('Name of Scenario ', i))


          #description/long_name
          ln <- textAreaInput(paste0('scenario_long_name_',i), 'Description', width='90.5%',
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

    #other temporal resoultion
    output$temporal_resoultion_other <- renderUI({
      if(input$temporal_resolution=='other'){
        t <- renderText('Provide the definition of your temporal resolution in the ISO 8601:2004 duration format P(YYYY)-(MM)-(DD).
                   Examples: decadal: P0010-00-00 and daily: P0000-00-01')

        s <- textInput('temp_res_txt', '', width='80%', placeholder='P0000-00-00')
        return(list(t,s))
      }
    })
    output$temporal_resoultion_irregular <- renderUI({
      if(input$temporal_resolution=='Irregular' | input$temporal_resolution=='Paleo'){
        if(input$temporal_resolution=='Irregular'){
          t <- renderText('Provide the definition of your timesteps as a comma-separated list in the format YYYY-MM-DD or short YYYY.')
          s <- textInput('temp_res_irr', '', width='80%', placeholder='YYYY, YYYY, ...')
          return(list(t,s))
        }else{
          t <- renderText('Provide the definition of your timesteps as a comma-separated list. The values will represent "kyr B.P.".')
          s <- textInput('temp_res_irr', '', width='80%', placeholder='126, 125, ...')
          return(list(t,s))
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


    #submit button clicked -----
    observeEvent(input$create_json,{
      #start checks----

      #check if outputpath is empty
      if(input$outputpath==''){
        output$value <- renderPrint({'You need to give an outputpath.'})
      }else{
        #check filepath
        if (checkmate::checkCharacter(input$outputpath) != TRUE){
          output$value <- renderPrint({'Outputpath must be of type character.'})
        } else if(checkmate::checkDirectoryExists(dirname(input$outputpath)) != TRUE){
          output$value <- renderPrint({paste0('Output directory does not exist.\n', dirname(input$outputpath))})
        } else if(!endsWith(input$outputpath, '.json')){
          output$value <- renderPrint({'Outputpath needs to end with *.json'})
        } else {

          #check title
          if(nchar(input$title)==0){
            to_do_list <- c('You must give a title.', to_do_list)
            create <- FALSE

          } else if(!is.na(suppressWarnings(as.numeric(input$title)))){
            to_do_list <- c('You title must contain characters.', to_do_list)
            create <- FALSE
          }

          #check summary
          if(nchar(input$summary)==0){
            to_do_list <- c('You must provide a summary.', to_do_list)
            create <- FALSE

          } else if(!is.na(suppressWarnings(as.numeric(input$summary)))){
            to_do_list <- c('You summary must contain characters.', to_do_list)
            create <- FALSE
          }
          if (nchar(input$summary)!=0 & nchar(input$summary)>1500){
            to_do_list <- c('Your summary is too long. Max. 1500 characters.', to_do_list)
            create <- FALSE
          }

          #check references - not mandatory
          if(nchar(input$references)==0){
            references <- nodata
          }else{
            references <- stringr::str_split(input$references, ',')[[1]]
            references <- stringr::str_remove_all(references, ' ')
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
            project_name <- 'TERRANOVA - The European Landscape Learning Initiative'
          }else{
            project_name <- 'TERRANOVA - The European Landscape Learning Initiative'
          }

          #check project_url
          if(!is.null(need(input$project_url != '', TRUE))){
            project_url <- 'https://www.terranova-itn.eu'
          }else{
            project_url <- 'https://www.terranova-itn.eu'
          }

          #check creator_name
          if(!is.null(need(input$creator_name != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a creator name.')
            create <- FALSE
          }

          #check creator_email
          if(!is.null(need(input$creator_email != '', TRUE))){
            creator_email <- nodata
          }else{
            creator_email <- input$creator_email
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
            contributors_names <- nodata
          }else{
            contributors_names <- input$contributor_names
          }

          #check license
          if(!is.null(need(input$license != '', TRUE))){
            to_do_list <- c(to_do_list, 'You need to provide a license URL.')
            create <- FALSE
          }

          #check comment
          if(!is.null(need(input$comment != '', TRUE))){
            comment <- nodata
          }else{
            comment <- input$comment
          }

          #check ebv_class
          if(!is.null(need(input$ebv_class != '', TRUE))){
            ebv_class <- nodata
          }else{
            ebv_class <- input$ebv_class
          }

          #check ebv_name
          if(!is.null(need(input$ebv_name != '', TRUE))){
            ebv_name <- nodata
          }else{
            ebv_name <- input$ebv_name
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
              ebv_domain_value <- c(input$ebv_domain[! input$ebv_domain %in% c('Other')], input$ebv_domain_other_txt)

            }else{
              #create ebv_domain value
              ebv_domain_value <- c(input$ebv_domain)
              }
            }

          #check biological_entity
          if(!is.null(need(input$biological_entity != '', TRUE))){
            biological_entity <- nodata
          }else{
            biological_entity <- input$biological_entity
          }

          #check entity_scope
          if(biological_entity=='Other'){
            if(!is.null(need(input$entity_other_txt != '', TRUE))){
              to_do_list <- c(to_do_list, 'You need to provide a description of the entity type as you chose Other.')
              create <- FALSE
              }
            } else{
              #entity_other_txt
              if(!is.null(need(input$entity_other_txt != '', TRUE))){
                entity_other_txt <- nodata
              }else{
                entity_other_txt <- input$entity_other_txt
              }
            }

              #entity_class_name
              if(!is.null(need(input$entity_scope_txt != '', TRUE))){#HERE
                entity_scope_txt <- nodata
              }else{
                entity_scope_txt <- input$entity_scope_txt
              }

              #entity_class_name
              if(!is.null(need(input$entity_classification_name_txt != '', TRUE))){
                entity_classification_name_txt <- nodata
              }else{
                entity_classification_name_txt <- input$entity_classification_name_txt
              }

              #entity_class_url
              if(!is.null(need(input$entity_classification_ref_txt != '', TRUE))){
                entity_classification_ref_txt <- nodata
              }else{
                entity_classification_ref_txt <- input$entity_classification_ref_txt
              }


          #check metric attributes
          if(input$metric_no>0){
            for(i in 1:input$metric_no){
              if(!is.null(need(eval(parse(text = paste0('input$metric_standard_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The name for metric ',i,' is missing.'))
                create <- FALSE
              }
              if(!is.null(need(eval(parse(text = paste0('input$metric_long_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The description for metric ',i,' is missing.'))
                create <- FALSE
              }
              if(!is.null(need(eval(parse(text = paste0('input$metric_units_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The units for metric ',i,' are missing.'))
                create <- FALSE
              }

              }
          }

          #check scenario attributes
          if(input$scenario_no>0){
            for(i in 1:input$scenario_no){
              if(!is.null(need(eval(parse(text = paste0('input$scenario_standard_name_', i))) != '', TRUE))){
                to_do_list <- c(to_do_list, paste0('The name for scenario ',i,' is missing.'))
                create <- FALSE
              }
              # if(!is.null(need(eval(parse(text = paste0('input$scenario_long_name_', i))) != '', TRUE))){
              #   to_do_list <- c(to_do_list, paste0('The description for scenario ',i,' is missing.'))
              #   create <- FALSE
              # }
            }

            #scenario_classification_name
            if(!is.null(need(input$scenario_classification_name != '', TRUE))){
              scenario_classification_name <- nodata
            }else{
              scenario_classification_name <- input$scenario_classification_name
            }

            #scenario_classification_version
            if(!is.null(need(input$scenario_classification_version != '', TRUE))){
              scenario_classification_version <- nodata
            }else{
              scenario_classification_version <- input$scenario_classification_version
            }

            #scenario_classification_url
            if(!is.null(need(input$scenario_classification_url != '', TRUE))){
              scenario_classification_url <- nodata
            }else{
              scenario_classification_url <- input$scenario_classification_url
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
              spatial_desc_txt <- input$spatial_desc_txt
            }
          } else{
            spatial_desc_txt <- nodata
          }

          #temporal resolution
          if(input$temporal_resolution =='other'){
            t_res <- input$temp_res_txt
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
            t_res <- input$temporal_resolution
          }

          #check the input of the irregulare timesteps
          if(input$temporal_resolution=='Irregular'){
            timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            timestep_list <- gsub(' ', '', timestep_list) #remove whitespaces
            print(timestep_list)
            ts_irr_wrong <- FALSE
            for(ts in timestep_list){
              if(grepl('^\\d{4}-\\d{2}-\\d{2}$', ts) | grepl('^\\d{4}$', ts)){
                #pass
              }else{
                ts_irr_wrong <- TRUE
              }
            }
            if(ts_irr_wrong){
              to_do_list <- c(to_do_list, 'You chose "Irregular" temporal resolution and the input you provided does not match the required form: comma-separated YYYY-MM-DD or comma-separated YYYY. Please check.')
              create <- FALSE
            }
          }
          if(input$temporal_resolution=='Paleo'){
            timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            for(ts in timestep_list){
              if(is.na(suppressWarnings(as.numeric(ts)))){
                to_do_list <- c(to_do_list, paste0('You chose "paleo" temporal resolution. The value "', ts, '" does not seem to be a numeric value. Please check.'))
                create <- FALSE
              }
            }
          }

          #temporal extent
          if(input$temporal_resolution=='Irregular'){
            timestep_list <- stringr::str_split(input$temp_res_irr, ',')[[1]]
            t_start <- min(timestep_list)
            t_end <- max(timestep_list)
            timesteps_irr <- paste0('"', paste0(timestep_list, collapse = '", "'), '"')
          }else if(input$temporal_resolution=='Paleo'){
            timestep_list <- as.numeric(stringr::str_split(input$temp_res_irr, ',')[[1]])
            t_start <- max(timestep_list)
            t_end <- min(timestep_list)
            timesteps_irr <- paste0('"', paste0(timestep_list, collapse = '", "'), '"')
          }else if(input$temporal_resolution=='P0000-00-00'){
            t_start <-input$temporal_extent_input
            t_end <- input$temporal_extent_input
          }else{
            t_start <- input$temporal_extent_input[1]
            t_end <- input$temporal_extent_input[2]
          }
          if(input$temporal_resolution!='Irregular' & input$temporal_resolution!='Paleo'){
            timesteps_irr <- '"N/A"'
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
            #inform user
            output$value <- renderPrint({'Creating Metadata file'})

            #create metric
            if(input$metric_no>0){
              ebv_metric <- ''
              for(i in 1:input$metric_no){
                ebv_metric <- paste0(ebv_metric, '\n\t\t\t\t"ebv_metric_',i,'": {
                    ":standard_name": "',eval(parse(text = paste0('input$metric_standard_name_', i))),'",
                    ":long_name": "',eval(parse(text = paste0('input$metric_long_name_', i))),'",
                    ":units": "',eval(parse(text = paste0('input$metric_units_', i))),'"\n\t\t\t\t}')
                if(i != input$metric_no){
                  ebv_metric <- paste0(ebv_metric, ',')
                }
              }
            }

            #create scenario
            if (input$scenario_no > 0){
              ebv_scenario <- paste0('"ebv_scenario": {
                "ebv_scenario_classification_name": "',scenario_classification_name,'",
                "ebv_scenario_classification_version": "',scenario_classification_version,'",
                "ebv_scenario_classification_url": "',scenario_classification_url,'",')
              for(i in 1:input$scenario_no){
                if(!is.null(need(eval(parse(text = paste0('input$scenario_long_name_', i))) != '', TRUE))){
                  long_name <- nodata
                } else{
                  long_name <- eval(parse(text = paste0('input$scenario_long_name_', i)))
                }
                ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t\t"ebv_scenario_',i,'": {
                    ":standard_name": "',eval(parse(text = paste0('input$scenario_standard_name_', i))),'",
                    ":long_name": "',long_name,'"
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
            json <- paste0('{
    "data": [
        {
            "id": "pending",
            "naming_authority": "The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
            "title": "', input$title ,'",
            "date_created": "',input$date_created,'",
            "summary": "',input$summary,'",
            "references": [\n\t\t\t\t"',paste0(unlist(references), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "source": "',input$methods,'",
            "coverage_content_type": [\n\t\t\t\t"',paste0(input$coverage_content_type, collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "project": "',project_name,'",
            "project_url": "',project_url,'",
            "creator": {
                "creator_name": "',input$creator_name,'",
                "creator_email": "', creator_email,'",
                "creator_institution": "',input$creator_institution,'"
            },
            "contributor_name": [\n\t\t\t\t"',paste0(stringr::str_remove_all(stringr::str_split(contributors_names, ',')[[1]], ' '), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "license": "',input$license,'",
            "publisher": {
                "publisher_name": "',input$publisher_name,'",
                "publisher_email": "',input$publisher_email,'",
                "publisher_institution": "',input$publisher_institution,'"
            },
            "ebv": {
                "ebv_class": "',ebv_class,'",
                "ebv_name": "',ebv_name,'"
            },
            "ebv_entity": {
                "ebv_entity_type": "',biological_entity,'",
                "ebv_entity_scope": "', entity_scope_txt,'",
                "ebv_entity_classification_name": "',entity_classification_name_txt,'",
                "ebv_entity_classification_url": "',entity_classification_ref_txt,'"
            },
            "ebv_metric": {',
                           ebv_metric,
                           '\n\t\t\t},\n\t\t\t',
                           ebv_scenario,
                           '\n\t\t\t"ebv_geospatial": {
                "ebv_geospatial_scope": "',input$spatial_scope,'",
                "ebv_geospatial_description": "',spatial_desc_txt,'"
            },
            "geospatial_lat_units": "',input$spatial_units,'",
            "geospatial_lon_units": "',input$spatial_units,'",
            "time_coverage": {
                "time_coverage_resolution": "',t_res,'",
                "time_coverage_start": "',t_start,'",
                "time_coverage_end": "',t_end,'"
            },
            "ebv_domain": [\n\t\t\t\t"', paste0(ebv_domain_value, collapse = '",\n\t\t\t\t"'),'"\n\t\t\t],
            "comment": "',comment,'",
            "terranova_type": "', input$terranova_type,'",
            "timesteps": [\n\t\t\t\t', timesteps_irr, '\n\t\t\t]
        }
    ]
}')


            #write to file and set encoding
            withr::with_options(list(encoding = "UTF-8"), write(json, input$outputpath))
          }


        }
      }
    })



}

shinyApp(ui, server)

#To Do
#pre-filled values for keywords: keywords:
#Anthropogenic activity / biodiversity / climate /
