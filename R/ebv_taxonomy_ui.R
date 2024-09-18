#' The Shiny App UI for ebv_taxonomy_app
#' @noRd
ebv_taxonomy_ui <- function(){
  shiny::fluidPage(
    #set theme
    theme = bslib::bs_theme(bootswatch = "flatly"),
    #title
    shiny::titlePanel("Browse your EBV netCDF file based on its taxonomy"),

    #short how-to
    shiny::uiOutput('ui_howto'),

    #get netCDF
    shinyFiles::shinyFilesButton("load_netcdf", "Load netCDF",
                                 'Select the EBV netCDF.',
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
    shiny::tags$div(
      style="margin-bottom:20px;"
    ),

    # #describe elements in plot
    # shiny::uiOutput('ui_title2'),
    # shiny::uiOutput('ui_scenario'),
    # shiny::uiOutput('ui_metric'),
    # shiny::uiOutput('ui_entity'),

    #describe elements in plot
    shiny::fluidRow(
      #empty column
      shiny::column(1, ),
      #description column
      shiny::column(6,
             #description
             shiny::uiOutput('ui_title2'),
             shiny::uiOutput('ui_scenario'),
             shiny::uiOutput('ui_metric'),
             shiny::uiOutput('ui_entity'),
             #some space
             shiny::tags$div(
               style="margin-bottom:15px;"
             ),
             shiny::fluidRow(#text time slider & quantiles
               shiny::column(6, shiny::uiOutput("txt_date")),
               shiny::column(4, shiny::uiOutput('txt_quantiles'))
             ),
             shiny::fluidRow(#time slider & quantile selection
               shiny::column(6, shiny::uiOutput("timeslider")),
               shiny::column(2, shiny::uiOutput('classes_quantiles'))
             ),

      ),
    ),

    shiny::fluidRow(
      #map
      shiny::column(8, shiny::plotOutput("plot_map",
                                  click="plot_click",
                                  dblclick = "plot1_dblclick",
                                  brush = shiny::brushOpts(id = "plot1_brush", resetOnNew = TRUE))
      ),

    ),

    shiny::tags$script("$(document).on('change', '.dynamicSI select', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });"),

    shiny::fluidRow(
      #empty column
      shiny::column(1, ),
      shiny::column(8,
             #zoom explanation
             shiny::uiOutput('ui_map_help')
      )
    ),

    #some space
    shiny::tags$div(
      style="margin-bottom:25px;"
    )
  )
}
