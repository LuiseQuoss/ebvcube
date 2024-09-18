#' EBV taxonomy Shiny App
#'
#' @description Load an EBV netCDF that holds taxonomic information to browse
#'   the data and visualize it.
#'
#' @param verbose Logical. Default: TRUE. Turn off additional messages by
#'   setting it to FALSE.
#'
#' @note Works with all EBV netCDFs created with
#'   [ebvcube::ebv_create_taxonomy()].
#'
#' @importFrom shinyWidgets sliderTextInput
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom bslib bs_theme
#' @importFrom grDevices colorRampPalette
#' @export
ebv_taxonomy_app <- function(verbose =  TRUE) {
  shiny_env <- new.env()
  assign('verbose', verbose, shiny_env)
  environment(ebv_taxonomy_ui) <- shiny_env
  environment(ebv_taxonomy_server) <- shiny_env
  app <- shiny::shinyApp(
    ui = ebv_taxonomy_ui,
    server = ebv_taxonomy_server
  )
  shiny::runApp(app)
}
