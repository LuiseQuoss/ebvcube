#' Builds the UI for ebv_taxonomy_app
#' @noRd
fluidPage(
  #set theme
  theme = bslib::bs_theme(bootswatch = "flatly"),
  #title
  shiny::titlePanel("Browse your EBV netCDF file based on its taxonomy"),

  #short how-to
  shiny::uiOutput('ui_howto'),

  #get netCDF
  shinyFiles::shinyFilesButton("load_netcdf", "Load netCDF",
                               'Select the EBV netCDF you want to look at.',
                               multiple=FALSE, filetype=list(netCDF = '*.nc')),

  #some space
  shiny::tags$div(
    style="margin-bottom:10px;"
  ),

  #add title of the netCDF when it is loaded
  shiny::uiOutput('ui_title'),

  #some space
  shiny::tags$div(
    style="margin-bottom:5px;"
  ),

  #create selectInput based on the taxonomy levels
  shiny::uiOutput('select_pane'),

  #some space
  shiny::tags$div(
    style="margin-bottom:35px;"
  ),

  #choose datacube
  shiny::uiOutput('ui_datacube_txt'),
  shiny::uiOutput('ui_datacube'),

  #some space
  shiny::tags$div(
    style="margin-bottom:5px;"
  ),

  #button for map
  shiny::actionButton('button_map', label = "Plot Map", class = "btn-lg btn-success"),

  #some space
  tags$div(
    style="margin-bottom:20px;"
  ),

  #describe elements in plot
  shiny::uiOutput('ui_title2'),
  shiny::uiOutput('ui_scenario'),
  shiny::uiOutput('ui_metric'),
  shiny::uiOutput('ui_entity'),

  shiny::fluidRow(
    #map
    shiny::column(8, plotOutput("plot_map",# width='900px',
                         #height = '500px',
                         click="plot_click",
                         dblclick = "plot1_dblclick",
                         brush = shiny::brushOpts(id = "plot1_brush", resetOnNew = TRUE))
    ),
    #help text for map
    shiny::column(1, shiny::uiOutput('ui_map_help')
    )

  ),

  shiny::tags$script("$(document).on('change', '.dynamicSI select', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });")
  )
