#' EBV netCDF properties class (S4)
#'
#' @slot general Named list. Elements: title, description, ebv_class, ebv_name,
#'   ebv_domain, references, source, project_name, project_url, creator_name,
#'   creator_institution, creator_email, contributor_name, publisher_name,
#'   publisher_institution, publisher_email, comment, keywords, id, history,
#'   licence, conventions, naming_authority, date_created, date_issued,
#'   entity_names, entity_type, entity_scope, entity_classification_name,
#'   entity_classification_url, taxonomy, taxonomy_lsid
#' @slot spatial Named list. Elements: wkt2, epsg, extent, resolution,
#'   crs_units, dimensions, scope, description
#' @slot temporal Named list. Elements: resolution, units, timesteps, dates
#' @slot metric Named list. Elements: name, description
#' @slot scenario Named list. Elements: name, description
#' @slot ebv_cube Named list. Elements: units, coverage_content_type, fillvalue,
#'   type
#'
#' @return S4 class containing the EBV netCDF properties
#' @export
#'
#' @note If the properties class holds e.g. no scenario information this is
#'   indicated with an element called status in the list. \cr If you read an EBV
#'   netCDF based on an older standard, the properties will differ from the
#'   definition above. If the dataset does not encompass taxonomic info, the
#'   'taxonomy' is NA. Besides, even if a dataset encompasses the taxonomy
#'   information, the 'taxonomy_lsid' can be NA.
methods::setClass(
  "EBV netCDF properties",
  slots = list(
    general = "list",
    spatial = "list",
    temporal = "list",
    metric = "list",
    scenario = "list",
    ebv_cube = "list"
  )
)

#' Read properties of EBV netCDF
#'
#' @description Structured access to all attributes of the netCDF file.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#' @param scenario Character or integer. Optional. Default: NULL. Define the
#'   scenario you want to access. If the EBV netCDF has no scenarios, leave the
#'   default value (NULL). You can use an integer value defining the scenario or
#'   give the name of the scenario as a character string. To check the available
#'   scenarios and their name or number (integer), use
#'   [ebvcube::ebv_datacubepaths()].
#' @param metric Character or integer. Optional. Define the metric you want to
#'   access. You can use an integer value defining the metric or give the name
#'   of the scenario as a character string. To check the available metrics and
#'   their name or number (integer), use [ebvcube::ebv_datacubepaths()].
#'
#' @return S4 class containing information about file or file and datacube
#'   depending on input.
#' @export
#'
#' @examples
#' #define path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#'
#' #get properties only for the file
#' prop_file <- ebv_properties(file)
#' #get properties for the file and a specific datacube - use datacubepath
#' prop_dc <- ebv_properties(file, datacubepath = datacubes[1,1])
#' #get properties for the file and a specific datacube - use scenario & metric
#' #note: this dataset has no scenario -> only metric is defined
#' prop_dc <- ebv_properties(file, metric = 2)
ebv_properties <-
  function(filepath,
           datacubepath = NULL,
           scenario = NULL,
           metric = NULL,
           verbose = TRUE) {

    ####initial tests start ----
    # ensure file and all datahandles are closed on exit
    withr::defer(if (exists('hdf')) {
      if (rhdf5::H5Iis_valid(hdf) == TRUE) {
        rhdf5::H5Fclose(hdf)
      }
    })

    dids <- c('entity_list', 'did', 'entity_level.id')
    withr::defer(
      for (id in dids){
        if(exists(id)){
          id <- eval(parse(text = id))
          if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Dclose(id)}
        }
      }
    )

    #are all arguments given?
    if (missing(filepath)) {
      stop('Filepath argument is missing.')
    }

    #check verbose
    if (checkmate::checkLogical(verbose, len = 1, any.missing = FALSE) != TRUE) {
      stop('Verbose must be of type logical.')
    }

    #filepath check
    if (checkmate::checkCharacter(filepath) != TRUE) {
      stop('Filepath must be of type character.')
    }
    if (checkmate::checkFileExists(filepath) != TRUE) {
      stop(paste0('File does not exist.\n', filepath))
    }
    if (!endsWith(filepath, '.nc')) {
      stop(paste0('File ending is wrong. File cannot be processed.'))
    }

    #datacubepath check
    #1. make sure anything is defined
    if(is.null(datacubepath) && is.null(scenario) && is.null(metric)){
      if(verbose){
        print('Giving the properties for the file. For more info on a specific datacube, define the metric and scenario OR datacubepath.')
      }
      # open file
      hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
    }else if(!is.null(datacubepath)){
    #2. check datacubepath
      # open file
      hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
      if (checkmate::checkCharacter(datacubepath) != TRUE) {
        stop('Datacubepath must be of type character.')
      }
      if (rhdf5::H5Lexists(hdf, datacubepath) == FALSE ||
          !stringr::str_detect(datacubepath, 'ebv_cube')) {
        stop(paste0('The given datacubepath is not valid:\n', datacubepath))
      }
      if(verbose){
        print('Giving the properties for the file and a specified datacube.')
      }
    } else if(!is.null(metric)){
      #3. check metric&scenario
      datacubepaths <- ebv_datacubepaths(filepath, verbose)
      datacubepath <- ebv_i_datacubepath(scenario, metric,
                                         datacubepaths, verbose)
      # open file
      hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")

      if(verbose){
        print('Giving the properties for the file and a specified datacube.')
      }
    }

    ####initial tests end ----

    #get all taxonomy values----
    if(rhdf5::H5Lexists(hdf, 'entity_list')){
      #get levels
      tax_levels <- suppressWarnings(rhdf5::h5read(hdf, 'entity_levels'))
      tax_levels <- apply(tax_levels, 1, ebv_i_paste)

      #get values of all levels
      tax_list <- suppressWarnings(rhdf5::h5read(hdf, 'entity_list'))
      #create taxon table
      dims_list <- dim(tax_list)
      taxon_df <- data.frame(matrix(NA, nrow=dims_list[2], ncol=length(tax_levels)))
      colnames(taxon_df) <- tax_levels
      for (d in 1:dims_list[1]){
        taxon_df[,d] <- apply(tax_list[d,,], 1, ebv_i_paste)
      }

      #check for lsid
      if(rhdf5::H5Lexists(hdf, 'entity_lsid')){
        lsid_list <- suppressWarnings(rhdf5::h5read(hdf, 'entity_lsid'))
        taxon_lsid <- apply(lsid_list, 1, ebv_i_paste)
      } else{
        taxon_lsid <- NA
      }

    }else{
      taxon_df <- NA
      taxon_lsid <- NA
    }

    #get all entity names ----
    entity_data <- suppressWarnings(rhdf5::h5read(hdf, 'entity'))#HERE
    entity_names <- c()
    if (!is.na(ncol(entity_data))) {
      entity_names <-apply(entity_data, 2, ebv_i_paste)
    } else{
      entity_names <- entity_data
    }

    #general ----
    # add entity names to global properties
    title <- ebv_i_read_att(hdf, 'title', verbose)
    description <- ebv_i_read_att(hdf, 'summary', verbose)
    references <- ebv_i_read_att(hdf, 'references', verbose)
    source <- ebv_i_read_att(hdf, 'source', verbose)
    project_name <- ebv_i_read_att(hdf, 'project_name', verbose)
    project_url <- ebv_i_read_att(hdf, 'project_url', verbose)
    creator_name <- ebv_i_read_att(hdf, 'creator_name', verbose)
    creator_institution <-
      ebv_i_read_att(hdf, 'creator_institution', verbose)
    creator_email <- ebv_i_read_att(hdf, 'creator_email', verbose)
    contributor_name <-
      ebv_i_read_att(hdf, 'contributor_name', verbose)
    publisher_name <- ebv_i_read_att(hdf, 'publisher_name', verbose)
    publisher_institution <-
      ebv_i_read_att(hdf, 'publisher_institution', verbose)
    publisher_email <-
      ebv_i_read_att(hdf, 'publisher_email', verbose)
    comment <- ebv_i_read_att(hdf, 'comment', verbose)
    ebv_class <- ebv_i_read_att(hdf, 'ebv_class', verbose)
    ebv_name <- ebv_i_read_att(hdf, 'ebv_name', verbose)
    ebv_domain <- ebv_i_read_att(hdf, 'ebv_domain', verbose)
    conventions <- ebv_i_read_att(hdf, 'Conventions', verbose)
    naming_authority <-
      ebv_i_read_att(hdf, 'naming_authority', verbose)
    history <- ebv_i_read_att(hdf, 'history', verbose)
    keywords <- ebv_i_read_att(hdf, 'keywords', verbose)
    id <- ebv_i_read_att(hdf, 'id', verbose)
    date_created <- ebv_i_read_att(hdf, 'date_created', verbose)
    date_issued <- ebv_i_read_att(hdf, 'date_issued', verbose)
    licence <- ebv_i_read_att(hdf, 'license', verbose)

    #entities info
    did <- rhdf5::H5Dopen(hdf, 'entity')#HERE
    ebv_entity_type <-
      ebv_i_read_att(did, 'ebv_entity_type', verbose)
    ebv_entity_scope <-
      ebv_i_read_att(did, 'ebv_entity_scope', verbose)
    ebv_entity_classification_name <-
      ebv_i_read_att(did, 'ebv_entity_classification_name', verbose)
    ebv_entity_classification_url <-
      ebv_i_read_att(did, 'ebv_entity_classification_url', verbose)
    rhdf5::H5Dclose(did)

    # spatial ----
    #get resolution, units
    resolution <- c()
    crs_units <-
      stringr::str_split(ebv_i_read_att(hdf, 'geospatial_lon_units', verbose), '_')[[1]][1]
    resolution <-
      c(as.numeric(stringr::str_remove_all(
        c(
          resolution,
          ebv_i_read_att(hdf, 'geospatial_lon_resolution', verbose)
        ), '[A-Za-z _-]'
      )),
      as.numeric(stringr::str_remove_all(
        c(
          resolution,
          ebv_i_read_att(hdf, 'geospatial_lat_resolution', verbose)
        ), '[A-Za-z _-]'
      )))

    #did <- rhdf5::H5Dopen(hdf, 'lat')
    #resolution <- c(resolution, ebv_i_read_att(hdf, 'geospatial_lat_resolution'))

    #global spatial atts
    ebv_spatial_scope <-
      ebv_i_read_att(hdf, 'ebv_spatial_scope', verbose)
    ebv_spatial_description <-
      ebv_i_read_att(hdf, 'ebv_spatial_description', verbose)

    #get dims
    if (ebv_i_4D(filepath)) {
      dims <-
        c(dim(hdf$lat),
          dim(hdf$lon),
          dim(hdf$time),
          dim(hdf$entity)[2])#HERE [2]
    } else{
      dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time))
    }


    #get extent
    extent <-
      c(
        min(hdf$lon) - resolution[1] / 2,
        max(hdf$lon) + resolution[1] / 2,
        min(hdf$lat) - resolution[2] / 2,
        max(hdf$lat) + resolution[2] / 2
      )

    #get extent, epsg, crs
    did <- rhdf5::H5Dopen(hdf, 'crs')
    #extent <- ebv_i_read_att(did, 'geospatial_bounds')
    epsg <-
      stringr::str_split(ebv_i_read_att(hdf, 'geospatial_bounds_crs', verbose), ':')[[1]][2]
    crs <- ebv_i_read_att(did, 'spatial_ref', verbose)
    rhdf5::H5Dclose(did)

    # temporal ----
    time_data <- suppressWarnings(rhdf5::h5read(hdf, 'time'))
    did <- rhdf5::H5Dopen(hdf, 'time')
    t_res <-
      ebv_i_read_att(hdf, 'time_coverage_resolution', verbose)
    t_units <- ebv_i_read_att(did, 'units', verbose)
    add <- 40177
    time_natural <- as.Date(time_data - add, origin = '1970-01-01')

    rhdf5::H5Dclose(did)

    #create lists of attributes----
    general <-
      list(
        'title' = title,
        'description' = description,
        'ebv_class' = ebv_class,
        'ebv_name' = ebv_name,
        'ebv_domain' = ebv_domain,
        'references' = references,
        'source' = source,
        'project_name' = project_name,
        'project_url' = project_url,
        'creator_name' = creator_name,
        'creator_institution' = creator_institution,
        'creator_email' = creator_email,
        'contributor_name' = contributor_name,
        'publisher_name' = publisher_name,
        'publisher_institution' = publisher_institution,
        'publisher_email' = publisher_email,
        'comment' = comment,
        'keywords' = keywords,
        'id' = id,
        'history' = history,
        'licence' = licence,
        'conventions' = conventions,
        'naming_authority' = naming_authority,
        'date_created' = date_created,
        'date_issued' = date_issued,
        'entity_names' = entity_names,
        'entity_type' = ebv_entity_type,
        'entity_scope' = ebv_entity_scope,
        'entity_classification_name' = ebv_entity_classification_name,
        'entity_classification_url' = ebv_entity_classification_url,
        'taxonomy' = taxon_df,
        'taxonomy_lsid' = taxon_lsid
      )
    spatial <-
      list(
        'wkt2' = crs,
        'epsg' = epsg,
        'extent' = extent,
        'resolution' = resolution,
        'crs_units' = crs_units,
        'dimensions' = dims,
        'scope' = ebv_spatial_scope,
        'description' = ebv_spatial_description
      )
    temporal <- list(
      'resolution' = t_res,
      'units' = t_units,
      'timesteps' = time_data,
      'dates' = time_natural
    )

    # FILE AND DATACUBE ----
    if (!is.null(datacubepath)) {
      #info about scenario, metric, cube
      # 1. scenario and metric ----
      if (stringr::str_detect(datacubepath, 'scenario')) {
        # get scenario info
        path_s <- stringr::str_split(datacubepath, '/')[[1]][1]
        gid <- rhdf5::H5Gopen(hdf, path_s)
        #global info
        ebv_sce_class_name <-
          ebv_i_read_att(hdf, 'ebv_scenario_classification_name', verbose)
        ebv_scen_class_url <-
          ebv_i_read_att(hdf, 'ebv_scenario_classification_url', verbose)
        ebv_sce_class_version <-
          ebv_i_read_att(hdf, 'ebv_scenario_classification_version', verbose)
        #group info
        name_s <- ebv_i_read_att(gid, 'standard_name', verbose)
        description_s <- ebv_i_read_att(gid, 'long_name', verbose)
        rhdf5::H5Gclose(gid)
        scenario <-
          list(
            'name' = name_s,
            'description' = description_s,
            'scenario_classification_name' = ebv_sce_class_name,
            'scenario_classification_url' = ebv_scen_class_url,
            'scenario_classification_version' = ebv_sce_class_version
          )

        # get metric info
        path_m <-
          paste0(
            stringr::str_split(datacubepath, '/')[[1]][1],
            '/',
            stringr::str_split(datacubepath, '/')[[1]][2]
          )
        gid <- rhdf5::H5Gopen(hdf, path_m)
        name_m <- ebv_i_read_att(gid, 'standard_name', verbose)
        description_m <- ebv_i_read_att(gid, 'long_name', verbose)
        rhdf5::H5Gclose(gid)
        metric <- list('name' = name_m, 'description' = description_m)

      } else{
        # 2. metric only----
        scenario <- list('status' = 'This dataset has no scenario.')
        # get metric info
        path_m <- stringr::str_split(datacubepath, '/')[[1]][1]
        gid <- rhdf5::H5Gopen(hdf, path_m)
        name_m <- ebv_i_read_att(gid, 'standard_name', verbose)
        description_m <- ebv_i_read_att(gid, 'long_name', verbose)
        rhdf5::H5Gclose(gid)
        metric <- list('name' = name_m, 'description' = description_m)
      }

      #cube info ----
      # open datacube
      did <- rhdf5::H5Dopen(hdf, datacubepath)
      fillvalue <- ebv_i_read_att(did, '_FillValue', verbose)
      coverage_content_type <-
        ebv_i_read_att(did, 'coverage_content_type', verbose)
      units_d <- ebv_i_read_att(did, 'units', verbose)
      #get type
      info <- utils::capture.output(did)
      rhdf5::H5Dclose(did)
      indices <- stringr::str_locate(info, ' type')
      for (row in 1:dim(indices)[1]) {
        if (!is.na(indices[row, 1])) {
          i <- c(row, indices[row, ])
        }
      }
      type <- as.vector(info)[i[1]]
      type <- stringr::str_remove(type, 'type')
      type <-
        stringr::str_replace_all(type, stringr::fixed(" "), "")
      ebv_cube <-
        list(
          'units' = units_d,
          'coverage_content_type' = coverage_content_type,
          'fillvalue' = fillvalue,
          'type' = type
        )

    } else{
      scenario <- list('status' = 'Only available with datacube argument.')
      metric <-
        list('status' = 'Only available with datacube argument.')
      ebv_cube <-
        list('status' = 'Only available with datacube argument.')
    }

    prop <-  methods::new(
      'EBV netCDF properties',
      general = general,
      spatial = spatial,
      temporal = temporal,
      metric = metric,
      scenario = scenario,
      ebv_cube = ebv_cube
    )


    #close file
    rhdf5::H5Fclose(hdf)

    return(prop)
  }
