#' EBV taxonomy Shiny App
#'
#' @description Load an EBV netCDF that holds taxonomic information to browse
#'   the data and visualize it.
#'
#' @note Works with all EBV netCDFs created with
#'   [ebvcube::ebv_create_taxonomy()].
#' @export
ebv_taxonomy_app <- function() {
  shiny::runApp(appDir = system.file('shiny', package='ebvcube'))
}
