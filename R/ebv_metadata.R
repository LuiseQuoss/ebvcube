#' Create the metadata file (json) for the EBV netCDF creation
#'
#' @description This function collects the metadata terms of the EBV netCDF to
#'   create and collects them in a text file in JSON format. Use
#'   [ebvcube::ebv_create()] with the output file of this function to create
#'   your EBV netCDF. During the actual creation you will be asked for more
#'   information regarding the spatial resolution, CRS etc.
#'
#' @param outputpath Character. Outputpath of the text-file (JSON format)
#'   containing the metadata. File ending: *.json
#' @param title Character. A short phrase or sentence describing the dataset.
#' @param summary Character. A paragraph describing the dataset, analogous to an
#'   abstract for a paper. Maximum character length: 1500.
#' @param references Character. Optional. DOIs (URLs) that describe the data or
#'   methods used to produce it. Multiple DOIs can be given in a vector.
#' @param source Character. The method of production of the original data. If it
#'   was model-generated, source should name the model and its version. If it is
#'   observational, source should characterize it. Corresponding term in the EBV
#'   Data Portal: 'methods'.
#' @param project_name Character. Optional. The name of the project principally
#'   responsible for originating this data.
#' @param project_url Character. Optional. The URL of the project.
#' @param scenario List. Optional. Scenario attributes defined accordingly:
#'   list(standard_name='', long_name=''). If you have several scenarios give a
#'   list of lists, e.g. for two: list(list(standard_name='',
#'   long_name=''),list(standard_name='', long_name='')). It is not mandatory to
#'   have a scenario.
#' @param metric List. Metric attributes defined accordingly:
#'   list(standard_name='', long_name='', units=''). If you have several metrics
#'   give a list of lists, e.g. for two: list(list(standard_name='',
#'   long_name='', units=''),list(standard_name='', long_name='', units=''))
#'   At least one metric is mandatory.
#' @param date_created Character. The date on which this version of the data was
#'   created in YYYY-MM-DD format.
#' @param creator_name Character. The name of the person principally responsible
#'   for creating this data.
#' @param creator_email Character. Optional. The email address of the person principally
#'   responsible for creating this data.
#' @param creator_institution Character. The institution of the creator; should
#'   uniquely identify the creator's institution.
#' @param contributor_name Character. Optional. The name of any individuals, projects, or
#'   institutions that contributed to the creation of this data. Corresponding
#'   term in the EBV Data Portal: 'Co-creators'. Multiple contributors can be
#'   given in a vector.
#' @param license Character. License of the dataset. Will be stored in the
#'   netCDF as the URL to the CC license. Choose one of: 'CC0', 'CC BY', 'CC
#'   BY-SA', 'CC BY-NC', 'CC BY-NC-SA', 'CC BY-ND', 'CC BY-NC-ND'.
#' @param comment Character.  Optional. Miscellaneous information about the data, not
#'   captured elsewhere.
#' @param ebv_class Character. EBV Class of the data set. One of: 'Genetic
#'   composition', 'Species populations', 'Species traits', 'Community
#'   composition', 'Ecosystem functioning', 'Ecosystem structure', 'Ecosystem
#'   services'. For more info see note.
#' @param ebv_name Character. EBV Name of the data set. The possible options
#'   depend on the EBV Class. One of: 'Intraspecific genetic diversity',
#'   'Genetic differentiation', 'Effective population size', 'Inbreeding',
#'   'Species distributions', 'Species abundances', 'Morphology', 'Physiology',
#'   'Phenology', 'Movement', 'Community abundance', 'Taxonomic and phylogenetic
#'   diversity', 'Trait diversity', 'Interaction diversity', 'Primary
#'   productivity', 'Ecosystem phenology', 'Ecosystem disturbances', 'Live cover
#'   fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile',
#'   'Pollination' For more info see note.
#' @param ebv_scenario_classification_name Character. Optional. Name of the classification
#'   system used for the scenarios.
#' @param ebv_scenario_classification_version Character. Optional. Version of the
#'   classification system used for the scenarios.
#' @param ebv_scenario_classification_url Character. Optional. URL of the classification
#'   system used for the scenarios.
#' @param ebv_spatial_scope Character. Spatial scope of the data set. One of:
#'   ‘Continental/Regional’, ‘National’, ‘Sub-national/Local’ or ‘Global’.
#' @param ebv_spatial_description Character. Specific information about the
#'   spatial scope. Mandatory if spatial scope is not Global.
#' @param ebv_domain Character. Environmental domain of the data set. Choose one
#'   or several of: ‘Terrestrial’, ‘Marine’ or ‘Freshwater’. Multiple domains
#'   can be given in a vector.
#' @param coverage_content_type Character. Describes the source of the data
#'   based on an ISO 19115-1 code. Choose one or several of: 'image',
#'   'thematicClassification', 'physicalMeasurement', 'auxiliaryInformation',
#'   'qualityInformation', 'referenceInformation', 'modelResult', or
#'   'coordinate'. Multiple types can be given in a vector.
#' @param ebv_entity_type Character. EBV entity type. This is a free entry
#'   field. Still, if one of the following terms fits your entity scope please
#'   use: 'Species', 'Communities', 'Ecosystems' or 'None'.
#' @param ebv_entity_scope Character. Specifies the entity scope in more detail,
#'   e.g., ‘African great apes’. If the entity type is 'None' this term is not
#'   applied to the metadata.
#' @param ebv_entity_classification_name Character. Optional. Name of the classification
#'   system used for the entity types.
#' @param ebv_entity_classification_url Character. Optional. URL of the classification
#'   system used for the entity types.
#' @param time_coverage_start Character. Start date of the time span covered by
#'   the dataset in YYYY-MM-DD format.
#' @param time_coverage_end Character. End date of the time span covered by the
#'   dataset in YYYY-MM-DD format.
#' @param time_coverage_resolution Character. Describes the targeted time period
#'   between each value in the data set (ISO 8601:2004 date format). Provide the
#'   definition of your temporal resolution in the ISO 8601:2004 duration format
#'   P(YYYY)-(MM)-(DD). Examples: decadal: 'P0010-00-00', daily: 'P0000-00-01',
#'   single timestep: 'P0000-00-00' and irregular time intervals: 'Irregular'.
#'   For the latter the timesteps can be directly defined when using
#'   [ebvcube::ebv_create()] (`timesteps` argument).
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the output
#'   file defined by 'outputpath'.
#' @param verbose Logical. Default: TRUE. Turn off additional prints and
#'   warnings by setting it to FALSE.
#'
#' @note \strong{Metadata Conventions:} The metadata termns of the EBV netCDFs
#'   are based on
#'   \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html}{CF
#'    1.8} and
#'   \href{https://wiki.esipfed.org/Attribute_Convention_for_Data_Discovery_1-3}{ACDD
#'    1.3} Conventions.
#'   Find more help in the
#'   \href{https://portal.geobon.org/downloads/pdf/how_to-10082023.pdf}{How-To}
#'   on the EBV Portal Website.\cr\strong{EBV Class:} The EBV Class is GEO BON's
#'   classification system for biodiversity monitoring. It categorizes and
#'   organizes essential ecological variables across scales and biological
#'   levels. This standardized framework helps identify and prioritize key
#'   variables representing biodiversity aspects like species composition,
#'   population dynamics, ecosystem functioning, and habitat quality.
#'   \cr\strong{EBV Name:} The EBV Name is a unique identifier for a specific
#'   variable in the EBV Class, representing a distinct aspect of biodiversity.
#'   It helps identify and categorize measured or monitored biodiversity
#'   information. The EBV Name reflects the ecological attribute being assessed,
#'   such as "Species Richness," "Population Abundance," "Functional Diversity,"
#'   "Habitat Fragmentation," or "Ecosystem Productivity”.
#'   \cr\strong{Authorship:} Besides the creator and the contributors there is a
#'   third metadata term regarding authorship: the 'publisher'. This is the
#'   person that logs into the EBV Data Portal and uploads the dataset. This
#'   person's personal data is automatically added to the metadata from the
#'   account information.
#'
#'
#' @details
#'   See EBV Classes and their corresponding EBV Names:
#'
#'   | \strong{EBV Class}     | \strong{EBV Names} |
#'   | :---                   | :---- |
#'   | Genetic composition    | 'Intraspecific genetic diversity', 'Genetic differentiation','Effective population size', 'Inbreeding'      |
#'   | Species populations    | 'Species distributions', 'Species abundances'      |
#'   | Species traits         | 'Morphology', 'Physiology', 'Phenology', 'Movement'      |
#'   | Community composition  | 'Community abundance', 'Taxonomic and phylogenetic diversity', 'Trait diversity', 'Interaction diversity'      |
#'   | Ecosystem functioning  | 'Primary productivity', 'Ecosystem phenology', 'Ecosystem disturbances'      |
#'   | Ecosystem structure    | 'Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile'      |
#'   | Ecosystem services     | 'Pollination', 'other'      |
#'
#'
#' @return Returns the outputpath of the JSON file with the metadata.
#' @export
#'
#' @examples
ebv_metadata <- function(outputpath, title, summary,
                         references = NULL, source, project_name = NULL, project_url = NULL,
                         date_created, creator_name, creator_email = NULL, creator_institution,
                         contributor_name = NULL, license, comment = NULL, ebv_class, ebv_name,
                         ebv_scenario_classification_name = NULL, ebv_scenario_classification_version = NULL,
                         ebv_scenario_classification_url = NULL, ebv_spatial_scope,
                         ebv_spatial_description = NULL, ebv_domain, coverage_content_type,
                         ebv_entity_type, ebv_entity_scope, ebv_entity_classification_name = NULL,
                         ebv_entity_classification_url = NULL, time_coverage_start, time_coverage_end,
                         time_coverage_resolution,
                         metric = list(standard_name='', long_name='', units=''),
                         scenario = list(standard_name='', long_name=''),
                         overwrite = FALSE, verbose = TRUE){

  #initial tests start ----
  #are all arguments given?
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }

    #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=FALSE) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('verbose must be of type logical.')
  }

  # #check character arguments ----
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

  na <- 'N/A'

  #get amount of metrics and scenarios----
  metric_no <- length(unlist(metric))/3
  scenario_no <- length(unlist(scenario))/2
  #check if scenario is empty
  if(scenario_no==1){
    if(scenario$standard_name==""){
      scenario_no <- 0
    }
  }

  if(scenario_no>0 && is.null(ebv_scenario_classification_name)){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_name. Are you sure you do not want to give that information?')
  }
  if(scenario_no>0 && is.null(ebv_scenario_classification_version)){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_version. Are you sure you do not want to give that information?')
  }
  if(scenario_no>0 && is.null(ebv_scenario_classification_url)){
    warning('You defined at least one scenario but did not define the ebv_scenario_classification_url. Are you sure you do not want to give that information?')
  }

  #check license----
  valid_licenses <- list('CC0'= 'https://creativecommons.org/publicdomain/zero/1.0/',
                         'CC BY'= 'https://creativecommons.org/licenses/by/4.0/',
                         'CC BY-SA'= 'https://creativecommons.org/licenses/by-sa/4.0/',
                         'CC BY-NC'= 'https://creativecommons.org/licenses/by-nc/4.0/',
                         'CC BY-NC-SA'= 'https://creativecommons.org/licenses/by-nc-sa/4.0/',
                         'CC BY-ND'= 'https://creativecommons.org/licenses/by-nd/4.0/',
                         'CC BY-NC-ND'= 'https://creativecommons.org/licenses/by-nc-nd/4.0/'
                         )
  if(!license %in% names(valid_licenses)){
    stop(paste0('License must be one of the following: ', paste0(names(valid_licenses), collapse = ', ')))
  }else{
    license <- valid_licenses[[license]]
  }

  #check ebv_domain----
  if(! ebv_i_contained(ebv_domain, c('Marine', 'Terrestrial', 'Freshwater'))){
    stop("EBV domain must be one or several of the following: 'Marine', 'Terrestrial', 'Freshwater'")
  }

  #check ebv class and ebv name ----
  ebv_classes <- c('Genetic composition', 'Species populations', 'Species traits', 'Community composition',
                   'Ecosystem functioning', 'Ecosystem structure', 'Ecosystem services')
  ebv_names <- c('Intraspecific genetic diversity', 'Genetic differentiation', 'Effective population size',
                 'Inbreeding', 'Species distributions', 'Species abundances', 'Morphology', 'Physiology',
                 'Phenology', 'Movement', 'Community abundance', 'Taxonomic and phylogenetic diversity',
                 'Trait diversity', 'Interaction diversity', 'Primary productivity', 'Ecosystem phenology',
                 'Ecosystem disturbances', 'Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile',
                 'Pollination')

  #ebv_class correct?
  if(! ebv_class %in% ebv_classes){
    stop('The ebv_class does not exit. Possible ebv_class values: ', paste(ebv_classes, collapse = ', '))
  }
  #ebv_name correct?
  if(! ebv_name %in% ebv_names){
    stop('The ebv_name does not exist. Possible ebv_name values: ', paste(ebv_names, collapse = ', '))
  }

  #check if ebv_name corresponds to ebv_class
  if(ebv_class == ebv_classes[1]){
    ebv_names <- c('Intraspecific genetic diversity', 'Genetic differentiation',
                   'Effective population size', 'Inbreeding')
  } else if(ebv_class == ebv_classes[2]){
    ebv_names <- c('Species distributions', 'Species abundances')
  }else if(ebv_class == ebv_classes[3]){
    ebv_names <- c('Morphology', 'Physiology', 'Phenology', 'Movement')
  }else if(ebv_class == ebv_classes[4]){
    ebv_names <- c('Community abundance', 'Taxonomic and phylogenetic diversity', 'Trait diversity', 'Interaction diversity')
  }else if(ebv_class == ebv_classes[5]){
    ebv_names <- c('Primary productivity', 'Ecosystem phenology', 'Ecosystem disturbances')
  }else if(ebv_class == ebv_classes[6]){
    ebv_names <- c('Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile')
  }else if(ebv_class == ebv_classes[7]){
    ebv_names <- c('Pollination')
  }
  if(! ebv_name %in% ebv_names){
    stop(paste0('The  ebv_name ', ebv_name, ' does not is not available for the ebv_class ', ebv_class,  '. Possible ebv_name values: ', paste(ebv_names, collapse = ', '), '. Change ebv_name or ebv_class.'))
  }

  #check spatial scope ----
  if(! ebv_spatial_scope %in% c('Continental/Regional', 'National', 'Sub-national/Local', 'Global')){
    stop('The ebv_spatial_scope needs to be one of the following: ‘Continental/Regional’, ‘National’, ‘Sub-national/Local’ or ‘Global’.')
  }
  if(ebv_spatial_scope != 'Global' && is.null(ebv_spatial_description)){
    stop('You chose an ebv_spatial_scope that requires an ebv_spatial_description. Please add.')
  }
  if(ebv_spatial_scope == 'Global' && !is.null(ebv_spatial_description)){
    if(verbose){
      warning('You set the ebv_spatial_scope to Global and added some spatial description. The latter is set to NA as this is the rule in the EBV Data Portal.')
    }
    ebv_spatial_description <- 'N/A'
  }

  #check ebv_scope----
  if(ebv_entity_type != 'None'){
    if(missing(ebv_entity_scope)){
      stop('You need to define the ebv_entit_scope')
    }
  }else{
    ebv_entity_scope <- 'N/A'
  }

  #check content coverage type----
  if(! ebv_i_contained(coverage_content_type, c('image', 'thematicClassification',
                                                'physicalMeasurement', 'auxiliaryInformation',
                                                'qualityInformation', 'referenceInformation',
                                                'modelResult', 'coordinate'))){
    stop("The coverage content type must be one or several of the following: 'image','thematicClassification', 'physicalMeasurement', 'auxiliaryInformation', 'qualityInformation', 'referenceInformation', 'modelResult' or 'coordinate'.")
  }

  # #OUTCOMMENTED check URLs ----
  # urls <- c(ebv_scenario_classification_url, project_url, ebv_entity_classification_url, references)
  # for (url in urls){
  #   if(!is.null(url)){
  #     if(! (stringr::str_starts(url, 'http://') | stringr::str_starts(url, 'https://'))){
  #       url <- paste0('https://', url)
  #     }
  #     if(verbose){
  #       if((ebv_i_check_url(url))){
  #         warning(paste0('Your url: ', url, ' cannot be reached. Please check.'))
  #       }
  #   }
  # }
  # }

  #check summary length ----
  if(nchar(summary)>1500){
    stop('The maximum character length of the summary is 1500. Pelase shorten your summary.')
  }

  # check if all mandatory arguments are given ----
  # mandatory_args <- c(title, summary,source,
  #                    date_created, creator_name, creator_institution,
  #                    license, ebv_class, ebv_name,
  #                    ebv_spatial_scope,ebv_domain, coverage_content_type,
  #                    ebv_entity_type, ebv_entity_scope,
  #                    time_coverage_start, time_coverage_end,
  #                    time_coverage_resolution,
  #                    metric)
  # print(mandatory_args)

  # optional_args <- c(project_name, project_url, creator_email, contributor_name,
  #                    comment, ebv_scenario_classification_name, ebv_scenario_classification_url,
  #                    ebv_scenario_classification_version, ebv_spatial_description,
  #                    ebv_entity_classification_url, ebv_entity_classification_name)
  # print(optional_args)

  # for(i in 1:length(optional_args)){
  #   if(is.null(arg)){
  #     optional_args[i] <- 'N/A'
  #   }
  # }

  #create metrics ----
  if(metric_no == 0){
    stop('You need to define at least one metric.')
  }else if(metric_no>1){
    ebv_metric <- paste0('"ebv_metric_1": {
                    ":standard_name": "', metric[[1]]$standard_name, '",
                    ":long_name": "', metric[[1]]$long_name, '",
                    ":units": "', metric[[1]]$units, '"\n\t\t\t\t}')
    if(metric_no>1){
      ebv_metric <- paste0(ebv_metric, ',')
      for(i in 2:metric_no){
        ebv_metric <- paste0(ebv_metric, '\n\t\t\t\t"ebv_metric_', i, '": {
                    ":standard_name": "', metric[[i]]$standard_name, '",
                    ":long_name": "', metric[[i]]$standard_name, '",
                    ":units": "', metric[[i]]$standard_name, '"\n\t\t\t\t}')
        if(i != metric_no){
          ebv_metric <- paste0(ebv_metric, ',')
        }
      }
    }
  }else{
    ebv_metric <- paste0('"ebv_metric_1": {
                    ":standard_name": "', metric$standard_name, '",
                    ":long_name": "', metric$long_name, '",
                    ":units": "', metric$units, '"\n\t\t\t\t}')
  }

  #create scenarios ----
  if (scenario_no > 1){
    ebv_scenario <- paste0('"ebv_scenario": {
                "ebv_scenario_classification_name": "', ebv_scenario_classification_name, '",
                "ebv_scenario_classification_version": "', ebv_scenario_classification_version, '",
                "ebv_scenario_classification_url": "', ebv_scenario_classification_url, '",')
    for(i in 1:scenario_no){
      ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t\t"ebv_scenario_', i, '": {
                    ":standard_name": "', scenario[[i]]$standard_name, '",
                    ":long_name": "', scenario[[i]]$long_name, '"
                }')
      if(i != scenario_no){
        ebv_scenario <- paste0(ebv_scenario, ',')
      }
    }
    ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t},')
  } else if (scenario_no==1){
    ebv_scenario <- paste0('"ebv_scenario": {
                "ebv_scenario_classification_name": "', ebv_scenario_classification_name, '",
                "ebv_scenario_classification_version": "', ebv_scenario_classification_version, '",
                "ebv_scenario_classification_url": "', ebv_scenario_classification_url, '",')
    ebv_scenario <- paste0(ebv_scenario, '\n\t\t\t\t"ebv_scenario_1": {
                    ":standard_name": "', scenario$standard_name, '",
                    ":long_name": "', scenario$long_name, '"
                }')
  }else{
    ebv_scenario <- '"ebv_scenario": "N/A",'
  }

  #create json with values ----
  json <- paste0('{
    "data": [
        {
            "id": "pending",
            "naming_authority": "The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig",
            "title": "', title, '",
            "date_created": "', date_created, '",
            "summary": "', summary, '",
            "references": [\n\t\t\t\t"', paste0(unlist(references), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "source": "', source, '",
            "coverage_content_type": [\n\t\t\t\t"', paste0(unlist(coverage_content_type), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "project": "', project_name, '",
            "project_url": "', project_url, '",
            "creator": {
                "creator_name": "', creator_name, '",
                "creator_email": "', creator_email, '",
                "creator_institution": "', creator_institution, '"
            },
            "contributor_name": [\n\t\t\t\t"', paste0(unlist(contributor_name), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "license": "', license, '",
            "publisher": {
                "publisher_name": "N/A",
                "publisher_email": "N/A",
                "publisher_institution": "N/A"
            },
            "ebv": {
                "ebv_class": "', ebv_class, '",
                "ebv_name": "', ebv_name, '"
            },
            "ebv_entity": {
                "ebv_entity_type": "', ebv_entity_type, '",
                "ebv_entity_scope": "', ebv_entity_scope, '",
                "ebv_entity_classification_name": "', ebv_entity_classification_name, '",
                "ebv_entity_classification_url": "', ebv_entity_classification_url, '"
            },
            "ebv_metric": {\n\t\t\t\t',
                 ebv_metric,
                 '\n\t\t\t},\n\t\t\t',
                 ebv_scenario,
                 '\n\t\t\t"ebv_spatial": {
                "ebv_spatial_scope": "', ebv_spatial_scope, '",
                "ebv_spatial_description": "', ebv_spatial_description, '"
            },
            "geospatial_lat_units": "N/A",
            "geospatial_lon_units": "N/A",
            "time_coverage": {
                "time_coverage_resolution": "', time_coverage_resolution, '",
                "time_coverage_start": "', time_coverage_start, '",
                "time_coverage_end": "', time_coverage_end, '"
            },
            "ebv_domain": [\n\t\t\t\t"', paste0(unlist(ebv_domain), collapse='",\n\t\t\t\t"'), '"\n\t\t\t],
            "comment": "', comment, '"
        }
    ]
}')

  #write json to file ----
  write(json, outputpath)
}
