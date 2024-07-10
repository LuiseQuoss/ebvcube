#' Create the metadata file (json) for the EBV netCDF creation
#'
#' @description This function collects the metadata terms of the EBV netCDF to
#'   create and collects them in a text file in JSON format. Use
#'   [ebvcube::ebv_create()] with the output file of this function to create
#'   your EBV netCDF. During the actual creation you will be asked for more
#'   information regarding the spatial resolution, CRS etc.
#' @details Not yet implemented: different calender (360_Days).
#'
#' @param outputpath Character. Outputpath of the text-file (JSON format)
#'   containing the metadata. File ending: *.json
#' @param title Character. A short phrase or sentence describing the dataset.
#' @param summary Character. A paragraph describing the dataset, analogous to an
#'   abstract for a paper.
#' @param references Character.
#' @param source Character.
#' @param project_name Character.
#' @param project_url Character.
#' @param scenario Character.
#' @param metric List. defined accordingly: list(standard_name='', long_name='',
#'   units=''). If you have several metrics give a list of lists, e.g. for two:
#'   list(list(standard_name='', long_name='', units=''),list(standard_name='',
#'   long_name='', units=''))
#' @param date_created Character.
#' @param creator_name Character.
#' @param creator_email Character.
#' @param creator_institution Character.
#' @param contributor_name Character.
#' @param publisher_name Character.
#' @param publisher_email Character.
#' @param publisher_institution Character.
#' @param license Character. One of:
#' @param comment Character.
#' @param ebv_name Character.
#' @param ebv_class Character.
#' @param ebv_scenario_classification_name Character.
#' @param ebv_scenario_classification_version Character.
#' @param ebv_scenario_classification_url Character.
#' @param ebv_spatial_scope Character.
#' @param ebv_spatial_description Character.
#' @param ebv_domain Character. One of
#' @param coverage_content_type Character. One of
#' @param ebv_entity_type Character.
#' @param ebv_entity_scope Character.
#' @param ebv_entity_classification_name Character.
#' @param ebv_entity_classification_url Character.
#' @param time_coverage_start Character.
#' @param time_coverage_end Character.
#' @param time_coverage_resolution Character.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the output
#'   file defined by 'outputpath'.
#' @param verbose Logical. Default: TRUE. Turn off additional prints and
#'   warnings by setting it to FALSE.
#'
#' @note Metadata of the EBV netCDFs is based on
#'   \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html}{CF
#'    1.8} and
#'   \href{https://wiki.esipfed.org/Attribute_Convention_for_Data_Discovery_1-3}{ACDD
#'    1.3]} Conventions.
#'   Find more help on
#'   the\href{How-To}{https://portal.geobon.org/downloads/pdf/how_to-10082023.pdf}
#'   on the EBV Portal Website.
#'
#' @return
#' @export
#'
#' @examples
ebv_metadata <- function(outputpath, title, summary,
                         references, source, project_name, project_url,
                         date_created, creator_name, creator_email, creator_institution,
                         contributor_name, publisher_name, publisher_email, publisher_institution,
                         license, comment, ebv_name='N/A', ebv_class='N/A',
                         ebv_scenario_classification_name = 'N/A', ebv_scenario_classification_version = 'N/A',
                         ebv_scenario_classification_url = 'N/A', ebv_spatial_scope,
                         ebv_spatial_description, ebv_domain, coverage_content_type,
                         ebv_entity_type = 'N/A', ebv_entity_scope = 'N/A', ebv_entity_classification_name = 'N/A',
                         ebv_entity_classification_url = 'N/A', time_coverage_start, time_coverage_end,
                         time_coverage_resolution,
                         metric = list(standard_name='', long_name='', units=''),
                         scenario = list(standard_name='', long_name=''),
                         overwrite = FALSE, verbose = TRUE){

  #initial tests start ----
  #are all arguments given?
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }
  for (att in c(title, summary,
                references, source, project_name, project_url,
                date_created, creator_name, creator_email, creator_institution,
                contributor_name, publisher_name, publisher_email, publisher_institution,
                license, comment, ebv_spatial_scope,
                ebv_spatial_description, ebv_domain, coverage_content_type,
                time_coverage_start, time_coverage_end,
                time_coverage_resolution))
  {
    #pass
    #throws error if argument is missing
  }

  #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }
  # #check character arguments
  # for (att in c(title, summary, references, source, project_name, project_url)){
  #   if (checkmate::checkCharacter(att) != TRUE){
  #     stop(paste0(deparse(substitute(att)),' must be of type character.'))
  #   }
  # }

  #outputpath check
  if (checkmate::checkCharacter(outputpath) != TRUE){
    stop('Outputpath must be of type character.')
  }
  if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  if(!endsWith(outputpath, '.json')){
    stop('Outputpath needs to end with *.json ')
  }
  #check if outpufile exists if overwrite is disabled
  if(!overwrite){
    if(checkmate::checkPathForOutput(outputpath) != TRUE){
      stop('Output file already exists. Change name or enable overwrite.')
    }
  }

  #check metric values
  # if(scenario_no==1){
  #   if(scenario$standard_name==""){
  #     scenario_no <- 0
  #   }

  #initial tests end ----

  na <- 'N/A'

  #get amount of metrics and scenarios
  metric_no <- length(unlist(metric))/3
  scenario_no <- length(unlist(scenario))/2
  #check if scenario is empty
  if(scenario_no==1){
    if(scenario$standard_name==""){
      scenario_no <- 0
    }
  }

  if(scenario_no>0 & ebv_scenario_classification_name=='N/A'){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_name. Are you sure you do not want to give that information?')
  }
  if(scenario_no>0 & ebv_scenario_classification_version=='N/A'){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_version. Are you sure you do not want to give that information?')
  }
  if(scenario_no>0 & ebv_scenario_classification_url=='N/A'){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_url. Are you sure you do not want to give that information?')
  }

  #create metrics ----
  ebv_metric <- paste0('"ebv_metric_1": {
                    ":standard_name": "',metric[[1]]$standard_name,'",
                    ":long_name": "',metric[[1]]$long_name,'",
                    ":units": "',metric[[1]]$units,'"\n\t\t\t\t}')
  if(metric_no>1){
    ebv_metric <- paste0(ebv_metric, ',')
    for(i in 2:metric_no){
      ebv_metric <- paste0(ebv_metric, '\n\t\t\t\t"ebv_metric_',i,'": {
                    ":standard_name": "',metric[[i]]$standard_name,'",
                    ":long_name": "',metric[[i]]$standard_name,'",
                    ":units": "',metric[[i]]$standard_name,'"\n\t\t\t\t}')
      if(i != metric_no){
        ebv_metric <- paste0(ebv_metric, ',')
      }
    }
  }
  #create scenarios ----
  if (scenario_no > 0){
    ebv_scenario <- paste0('"ebv_scenario": {
                "ebv_scenario_classification_name": "',ebv_scenario_classification_name,'",
                "ebv_scenario_classification_version": "',ebv_scenario_classification_version,'",
                "ebv_scenario_classification_url": "',ebv_scenario_classification_url,'",')
    for(i in 1:scenario_no){
      ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t\t"ebv_scenario_',i,'": {
                    ":standard_name": "',scenario[[i]]$standard_name,'",
                    ":long_name": "',scenario[[i]]$long_name,'"
                }')
      if(i != scenario_no){
        ebv_scenario <- paste0(ebv_scenario, ',')
      }
    }
    ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t},')
  } else {
    ebv_scenario <- '"ebv_scenario": "N/A",'
  }

  #create json with values ----
  json <- paste0('{
    "data": [
        {
            "id": "pending",
            "naming_authority": "The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
            "title": "', title ,'",
            "date_created": "',date_created,'",
            "summary": "',summary,'",
            "references": [\n\t\t\t\t"',paste0(unlist(references), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "source": "',source,'",
            "coverage_content_type": [\n\t\t\t\t"',paste0(unlist(coverage_content_type), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "project": "',project_name,'",
            "project_url": "',project_url,'",
            "creator": {
                "creator_name": "',creator_name,'",
                "creator_email": "',creator_email,'",
                "creator_institution": "',creator_institution,'"
            },
            "contributor_name": [\n\t\t\t\t"',paste0(unlist(contributor_name), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "license": "',license,'",
            "publisher": {
                "publisher_name": "',publisher_name,'",
                "publisher_email": "',publisher_email,'",
                "publisher_institution": "',publisher_institution,'"
            },
            "ebv": {
                "ebv_class": "',ebv_class,'",
                "ebv_name": "',ebv_name,'"
            },
            "ebv_entity": {
                "ebv_entity_type": "',ebv_entity_type,'",
                "ebv_entity_scope": "',ebv_entity_scope,'",
                "ebv_entity_classification_name": "',ebv_entity_classification_name,'",
                "ebv_entity_classification_url": "',ebv_entity_classification_url,'"
            },
            "ebv_metric": {\n\t\t\t\t',
                 ebv_metric,
                 '\n\t\t\t},\n\t\t\t',
                 ebv_scenario,
                 '\n\t\t\t"ebv_spatial": {
                "ebv_spatial_scope": "',ebv_spatial_scope,'",
                "ebv_spatial_description": "',ebv_spatial_description,'"
            },
            "geospatial_lat_units": "",
            "geospatial_lon_units": "",
            "time_coverage": {
                "time_coverage_resolution": "',time_coverage_resolution,'",
                "time_coverage_start": "',time_coverage_start,'",
                "time_coverage_end": "',time_coverage_end,'"
            },
            "ebv_domain": [\n\t\t\t\t"',paste0(unlist(ebv_domain), collapse='",\n\t\t\t\t"'),'"\n\t\t\t],
            "comment": "',comment,'"
        }
    ]
}')

  #write json to file ----
  write(json, outputpath)
}
