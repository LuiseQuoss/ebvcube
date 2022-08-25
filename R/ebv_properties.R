#' EBV netCDF properties class (S4)
#'
#' @slot general Named list. Elements: title, description, ebv_class, ebv_name,
#'   ebv_domain, references, source, project, creator_name, creator_institution,
#'   creator_email, contributor_name, publisher_name, publisher_institution,
#'   publisher_email, comment, keywords, id, history, licence, conventions,
#'   naming_authority, date_created, date_issued, entity_names, entity_type,
#'   entity_scope, entity_classification_name, entity_classification_url
#' @slot spatial Named list. Elements: wkt2, epsg, extent, resolution,
#'   crs_units, dimensions, scope, description
#' @slot temporal Named list. Elements: resolution, units, timesteps,
#'   timesteps_natural
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
#'   definition above.
methods::setClass("EBV netCDF properties", slots=list(general="list",
                                                      spatial="list",
                                                      temporal="list",
                                                      metric="list",
                                                      scenario="list",
                                                      ebv_cube="list"))

#' Read properties of EBV netCDF
#'
#' @description Structured access to all attributes of the netCDF file.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to TRUE.
#'
#' @return S4 class containing information about file or file and datacube
#'   depending on input.
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #get properties only for the file
#' prop_file <- ebv_properties(file)
#' #get properties for the file and a specific datacube
#' prop_dc <- ebv_properties(file, datacubes[1,1])
ebv_properties <- function(filepath, datacubepath = NULL, verbose = FALSE){
  ####initial tests start ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  withr::defer(
    if(exists('srs.ds')){
      if(rhdf5::H5Iis_valid(srs.ds)==TRUE){rhdf5::H5Dclose(srs.ds)}
    }
  )
  withr::defer(
    if(exists('time.ds')){
      if(rhdf5::H5Iis_valid(time.ds)==TRUE){rhdf5::H5Dclose(time.ds)}
    }
  )
  withr::defer(
    if(exists('dh')){
      if(rhdf5::H5Iis_valid(dh)==TRUE){rhdf5::H5Gclose(dh)}
    }
  )

  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #filepath check
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath)

  # open file
  hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")

  #variable check
  if (checkmate::checkCharacter(datacubepath) != TRUE & !is.null(datacubepath)){
    stop('Datacubepath must be of type character.')
  }
  if(!is.null(datacubepath)){
    if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
      stop(paste0('The given variable is not valid:\n', datacubepath))
    }
  }
  ####initial tests end ----

  #check dimensions
  ls <- rhdf5::h5ls(filepath)
  if('entity' %in% ls$name){#HERE
    new <- TRUE
  } else{
    new <- FALSE
  }

  #SUPER OLD STADARD----
  #-> make files currently at portal, work
  #aim: remove this part!!!
  if(!new){
    #get resolution ----
    lat.data <- rhdf5::h5read(filepath, 'lat', start=1, count = 2)
    lon.data <- rhdf5::h5read(filepath, 'lon', start=1, count = 2)
    res.lon <- lon.data[1]-lon.data[2]
    res.lat <- lat.data[1]-lat.data[2]
    if (res.lon<0){
      res.lon <- res.lon*-1
    }
    if (res.lat<0){
      res.lat <- res.lat*-1
    }
    resolution <- c(res.lon, res.lat)

    #get extent
    lon.data <- rhdf5::h5read(filepath, 'lon')
    lat.data <- rhdf5::h5read(filepath, 'lat')
    xmin <- min(lon.data) - resolution[1]/2
    xmax <- max(lon.data) + resolution[1]/2
    ymin <- min(lat.data) - resolution[2]/2
    ymax <- max(lat.data) + resolution[2]/2
    extent <- c(xmin, xmax, ymin, ymax)

    ####general properties of file ----
    # #get depth of nested structure
    depth <- ebv_i_read_att(hdf, 'ebv_subgroups')
    if(!depth[1]==''){
      if('metric' %in% depth){
        metric <- TRUE
      }else {metric <- FALSE}
      if('scenario' %in% depth){
        scenario <- TRUE
      } else {scenario <- FALSE}
      if('entity' %in% depth){
        entity <- TRUE
      }else {entity <- FALSE}
    } else {
      metric<-TRUE
      scenario <-FALSE
      entity<-FALSE
    }

    #get title
    title <- ebv_i_read_att(hdf, 'title')[1]

    #get description
    description.file <- ebv_i_read_att(hdf, 'description')[1]

    #get more global attributes
    ebv_name <- ebv_i_read_att(hdf, 'ebv_name')
    ebv_class <- ebv_i_read_att(hdf, 'ebv_class')
    ebv_subgroups <- ebv_i_read_att(hdf, 'ebv_subgroups')
    creator <- ebv_i_read_att(hdf, 'creator')

    #general
    general <- list(title=title, description=description.file,
                    ebv_class=ebv_class, ebv_name=ebv_name, ebv_subgroups=ebv_subgroups,
                    creator=creator)

    #get srs ----
    srs.ds <- rhdf5::H5Dopen(hdf, 'crs')
    srs <- ebv_i_read_att(srs.ds, 'spatial_ref')
    rhdf5::H5Dclose(srs.ds)

    #get epsg
    epsg <- as.numeric(ebv_i_get_epsg(srs))

    #get dims
    dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time))

    #create spatial list for S4 class
    spatial <- list(wkt2=srs, epsg=epsg, resolution=resolution, extent=extent, dimensions=dims)

    #get time info ----
    add <- 40177
    time.ds <- rhdf5::H5Dopen(hdf, 'time')
    #time units
    time <- ebv_i_read_att(time.ds, 'units')[1]
    #time delta
    t_delta <- ebv_i_read_att(time.ds, 't_delta')[1]
    #timesteps
    timesteps <- rhdf5::h5read(hdf, 'time')
    #timesteps natural language
    time_natural <- as.Date(timesteps-add, origin='1970-01-01')

    #close data handle
    rhdf5::H5Dclose(time.ds)

    #create temporal list for S4 class
    temporal <- list(units=time, t_delta=t_delta, timesteps=timesteps, timesteps_natural=time_natural)

    ####variable specific properties ----
    if (!is.null(datacubepath)){
      #### sm(e) ----
      if(scenario & metric){

        ####get entity info ----
        dh <- hdf&datacubepath

        #get type
        info <- utils::capture.output(dh)
        indices <- stringr::str_locate(info, ' type')
        for (row in 1:dim(indices)[1]){
          if (!is.na(indices[row,1])){
            i <- c(row, indices[row,])
          }
        }
        type <- as.vector(info)[i[1]]
        type <- stringr::str_remove(type, 'type')
        type <- stringr::str_replace_all(type, stringr::fixed(" "), "")

        #get fillvalue
        fillvalue <- ebv_i_read_att(dh, '_FillValue')
        if(is.null(fillvalue)){
          fillvalue <- array(NA)
        }

        #long name
        if (rhdf5::H5Aexists(dh, 'description')){
          description <- ebv_i_read_att(dh, 'description')
        }else if (rhdf5::H5Aexists(dh, 'long_name')){
          description <- ebv_i_read_att(dh, 'long_name')[1]
        } else {
          description <- 'not defined'
        }

        #standard_name
        if (rhdf5::H5Aexists(dh, 'label')){
          standard_name <- ebv_i_read_att(dh, 'label')[1]
        } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          standard_name <- ebv_i_read_att(dh, 'standard_name')[1]
        } else {
          standard_name <- 'not defined'
        }

        #unit
        unit <- ebv_i_read_att(dh, 'units')[1]
        #only because of error in globES - delete when corrected!
        if (is.null(unit)){
          unit <- ebv_i_read_att(dh, 'unit')[1]
        }

        #close data handle
        rhdf5::H5Dclose(dh)

        #create object of S4 class
        entity <- list(description=description, standard_name=standard_name, units=unit, type=type, fillvalue=fillvalue)

        ####get metric info ----
        #one level higher
        i <- length(stringr::str_split(datacubepath, '/')[[1]])
        group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
        dh <- rhdf5::H5Gopen(hdf, group)

        #standard_name
        if (rhdf5::H5Aexists(dh, 'label')){
          standard_name <- ebv_i_read_att(dh, 'label')[1]
        } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          standard_name <- ebv_i_read_att(dh, 'standard_name')[1]
        } else {
          standard_name <- 'not defined'
        }

        #description
        description <- ebv_i_read_att(dh, 'description')[1]

        #close data handle
        rhdf5::H5Gclose(dh)

        #create object of S4 class
        metric <- list(standard_name=standard_name, description=description)

        ####get scenario info ----
        #two levels higher
        i <- length(stringr::str_split(datacubepath, '/')[[1]])
        group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
        i <- length(stringr::str_split(group, '/')[[1]])-1
        group <- stringr::str_remove(group, stringr::str_split(datacubepath, '/')[[1]][i])
        group <- stringr::str_remove(group, '/')
        dh <- rhdf5::H5Gopen(hdf, group)

        #label
        if (rhdf5::H5Aexists(dh, 'label')){
          standard_name <- ebv_i_read_att(dh, 'label')[1]
        } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          standard_name <- ebv_i_read_att(dh, 'standard_name')[1]
        } else {
          standard_name <- 'not defined'
        }

        #description
        description <- ebv_i_read_att(dh, 'description')

        #close data handle
        rhdf5::H5Gclose(dh)

        #create object of S4 class
        scenario <- list(standard_name=standard_name, description=description)

        #create object of S4 class ----
        prop <- methods::new('EBV netCDF properties',
                             general = general,
                             spatial=spatial,
                             temporal=temporal,
                             metric=metric,
                             scenario=scenario,
                             ebv_cube=entity)

        # m(e) ----
      } else if (!scenario & metric){
        ####get entity info ----
        dh <- hdf&datacubepath

        #get type
        info <- utils::capture.output(dh)
        indices <- stringr::str_locate(info, ' type')
        for (row in 1:dim(indices)[1]){
          if (!is.na(indices[row,1])){
            i <- c(row, indices[row,])
          }
        }
        type <- as.vector(info)[i[1]]
        type <- stringr::str_remove(type, 'type')
        type <- stringr::str_replace_all(type, stringr::fixed(" "), "")

        #get fillvalue
        fillvalue <- ebv_i_read_att(dh, '_FillValue')
        if(is.null(fillvalue)){
          fillvalue <- array(NA)
        }

        #long name
        if (rhdf5::H5Aexists(dh, 'description')){
          description <- ebv_i_read_att(dh, 'description')
        } else if (rhdf5::H5Aexists(dh, 'long_name')){
          description <- ebv_i_read_att(dh, 'long_name')[1]
        } else {
          description <- 'not defined'
        }

        #label
        if (rhdf5::H5Aexists(dh, 'label')){
          standard_name <- ebv_i_read_att(dh, 'label')[1]
        } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          standard_name <- ebv_i_read_att(dh, 'standard_name')[1]
        } else {
          standard_name <- 'not defined'
        }

        #unit
        unit <- ebv_i_read_att(dh, 'units')[1]

        #close data handle
        rhdf5::H5Dclose(dh)

        #create object of S4 class
        entity <- list(description=description, standard_name=standard_name, units=unit, type=type, fillvalue=fillvalue)

        ####get metric info ----
        #one level higher
        i <- length(stringr::str_split(datacubepath, '/')[[1]])
        group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
        dh <- rhdf5::H5Gopen(hdf, group)

        #standard_name
        if (rhdf5::H5Aexists(dh, 'label')){
          standard_name <- ebv_i_read_att(dh, 'label')[1]
        } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          standard_name <- ebv_i_read_att(dh, 'standard_name')[1]
        } else {
          standard_name <- 'not defined'
        }

        #description
        description <- ebv_i_read_att(dh, 'description')[1]

        #close data handle
        rhdf5::H5Gclose(dh)

        #create object of S4 class
        metric <- list(standard_name=standard_name, description=description)

        #define new S4 class
        prop <- methods::new('EBV netCDF properties',
                             general = general,
                             spatial=spatial,
                             temporal=temporal,
                             metric=metric,
                             scenario=list(status='Datacube has no scenario.'),
                             ebv_cube=entity)

      }
      # define S4 for filepath only ----
    } else {
      prop <-  methods::new('EBV netCDF properties',
                            general = general,
                            spatial=spatial,
                            temporal=temporal,
                            metric=list(status='Only available with datacube argument.'),
                            scenario=list(status='Only available with datacube argument.'),
                            ebv_cube=list(status='Only available with datacube argument.')
      )
    }



  }else{
    #ACDD STANDARD----
    #get all entity names ----
    entity_data <- rhdf5::h5read(hdf, 'entity')#HERE
    entity_names <- c()
    if(!is.na(ncol(entity_data))){#HERE!
      for (col in 1:ncol(entity_data)){
        name <- paste0(entity_data[,col], collapse = '')
        entity_names <- c(entity_names, name)
      }
      #trim whitespaces
      entity_names <- gsub(pattern = "(^ +| +$)",
                           replacement = "",
                           x = entity_names)
    } else{
      entity_names <- entity_data
    }



    time_data <- rhdf5::h5read(hdf, 'time')

    #general ----
    # add entity names to global properties
    title <- ebv_i_read_att(hdf, 'title')
    description <- ebv_i_read_att(hdf, 'summary')
    references <- ebv_i_read_att(hdf, 'references')
    source <- ebv_i_read_att(hdf, 'source')
    project <- ebv_i_read_att(hdf, 'project_name')
    creator_name <- ebv_i_read_att(hdf, 'creator_name')
    creator_institution <- ebv_i_read_att(hdf, 'creator_institution')
    creator_email <- ebv_i_read_att(hdf, 'creator_email')
    contributor_name <- ebv_i_read_att(hdf, 'contributor_name')
    publisher_name <- ebv_i_read_att(hdf, 'publisher_name')
    publisher_institution <-ebv_i_read_att(hdf, 'publisher_institution')
    publisher_email <-ebv_i_read_att(hdf, 'publisher_email')
    comment <- ebv_i_read_att(hdf, 'comment')
    ebv_class <- ebv_i_read_att(hdf, 'ebv_class')
    ebv_name <- ebv_i_read_att(hdf, 'ebv_name')
    ebv_domain <- ebv_i_read_att(hdf, 'ebv_domain')
    conventions <- ebv_i_read_att(hdf, 'Conventions')
    naming_authority <- ebv_i_read_att(hdf, 'naming_authority')
    history <- ebv_i_read_att(hdf, 'history')
    keywords <- ebv_i_read_att(hdf, 'keywords')
    id <- ebv_i_read_att(hdf, 'id')
    date_created <- ebv_i_read_att(hdf, 'date_created')
    date_issued <- ebv_i_read_att(hdf, 'date_issued')
    license <- ebv_i_read_att(hdf, 'license')

    #entities info
    did <- rhdf5::H5Dopen(hdf, 'entity')#HERE
    ebv_entity_type <- ebv_i_read_att(did, 'ebv_entity_type')
    ebv_entity_scope <- ebv_i_read_att(did, 'ebv_entity_scope')
    ebv_entity_classification_name <- ebv_i_read_att(did, 'ebv_entity_classification_name')
    ebv_entity_classification_url <- ebv_i_read_att(did, 'ebv_entity_classification_url')
    rhdf5::H5Dclose(did)

    # spatial ----
    #get resolution, units
    resolution <- c()
    crs_units <- stringr::str_split(ebv_i_read_att(hdf, 'geospatial_lon_units'), '_')[[1]][1]
    resolution <- c(as.numeric(stringr::str_remove_all(c(resolution, ebv_i_read_att(hdf, 'geospatial_lon_resolution')),'[A-Za-z _-]')),
                    as.numeric(stringr::str_remove_all(c(resolution, ebv_i_read_att(hdf, 'geospatial_lat_resolution')),'[A-Za-z _-]')))

    #did <- rhdf5::H5Dopen(hdf, 'lat')
    #resolution <- c(resolution, ebv_i_read_att(hdf, 'geospatial_lat_resolution'))

    #global spatial atts
    ebv_spatial_scope <- ebv_i_read_att(hdf, 'ebv_spatial_scope')
    ebv_spatial_description <- ebv_i_read_att(hdf, 'ebv_spatial_description')

    #get dims
    if(ebv_i_4D(filepath)){
      dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time), dim(hdf$entity)[2])#HERE [2]
    } else{
      dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time))
    }


    #get extent
    extent <- c(min(hdf$lon)-resolution[1]/2, max(hdf$lon)+resolution[1]/2,
                min(hdf$lat)-resolution[2]/2, max(hdf$lat)+resolution[2]/2)

    #get extent, epsg, crs
    did <- rhdf5::H5Dopen(hdf, 'crs')
    #extent <- ebv_i_read_att(did, 'geospatial_bounds')
    epsg <- stringr::str_split(ebv_i_read_att(hdf, 'geospatial_bounds_crs'),':')[[1]][2]
    crs <-ebv_i_read_att(did, 'spatial_ref')
    rhdf5::H5Dclose(did)

    # temporal ----
    did <- rhdf5::H5Dopen(hdf, 'time')
    t_res <- ebv_i_read_att(hdf, 'time_coverage_resolution')
    t_units <- ebv_i_read_att(did, 'units')
    add <- 40177
    time_natural <- as.Date(time_data-add, origin='1970-01-01')

    rhdf5::H5Dclose(did)

    general <- list('title'=title, 'description' = description, 'ebv_class'= ebv_class,
                    'ebv_name'=ebv_name, 'ebv_domain'=ebv_domain,
                    'references'=references, 'source'=source, 'project'=project,
                    'creator_name'=creator_name, 'creator_institution'=creator_institution,
                    'creator_email'=creator_email, 'contributor_name'=contributor_name,
                    'publisher_name'=publisher_name, 'publisher_institution'=publisher_institution,
                    'publisher_email'=publisher_email, 'comment'=comment, 'keywords'=keywords,
                    'id'=id, 'history'=history, 'licence'=licence, 'conventions'=conventions,
                    'naming_authority'=naming_authority, 'date_created'=date_created,
                    'date_issued'=date_issued, 'entity_names'=entity_names,
                    'entity_type' = ebv_entity_type, 'entity_scope'=ebv_entity_scope,
                    'entity_classification_name'=ebv_entity_classification_name,
                    'entity_classification_url' =ebv_entity_classification_url)
    spatial <- list('wkt2'=crs, 'epsg'=epsg, 'extent'=extent, 'resolution'=resolution,
                    'crs_units'=crs_units, 'dimensions'=dims,'scope'=ebv_spatial_scope,
                    'description'=ebv_spatial_description)
    temporal <- list('resolution'=t_res, 'units'=t_units,
                     'timesteps'= time_data, 'timesteps_natural'=time_natural)

    # FILE AND DATACUBE ----
    if(!is.null(datacubepath)){
      #info about scenario, metric, cube
      # 1. scenario and metric
      if (stringr::str_detect(datacubepath,'scenario')){
        # get scenario info
        path_s <- stringr::str_split(datacubepath, '/')[[1]][1]
        gid <- rhdf5::H5Gopen(hdf, path_s)
        #global info
        ebv_scenario_classification_name <- ebv_i_read_att(hdf, 'ebv_scenario_classification_name')
        ebv_scenario_classification_url <- ebv_i_read_att(hdf, 'ebv_scenario_classification_url')
        ebv_scenario_classification_version <- ebv_i_read_att(hdf, 'ebv_scenario_classification_version')
        #group info
        name_s <- ebv_i_read_att(gid, 'standard_name')
        description_s <- ebv_i_read_att(gid, 'long_name')
        rhdf5::H5Gclose(gid)
        scenario <- list('name' = name_s, 'description'=description_s,
                         'scenario_classification_name' = ebv_scenario_classification_name,
                         'scenario_classification_url' = ebv_scenario_classification_url,
                         'scenario_classification_version' = ebv_scenario_classification_version)

        # get metric info
        path_m <- paste0(stringr::str_split(datacubepath, '/')[[1]][1],'/',
                         stringr::str_split(datacubepath, '/')[[1]][2])
        gid <- rhdf5::H5Gopen(hdf, path_m)
        name_m <- ebv_i_read_att(gid, 'standard_name')
        description_m <- ebv_i_read_att(gid, 'long_name')
        rhdf5::H5Gclose(gid)
        metric <- list('name' = name_m, 'description'=description_m)

      } else{
        # 2. metric only
        scenario <- list('status'='This dataset has no scenario.')
        # get metric info
        path_m <- stringr::str_split(datacubepath, '/')[[1]][1]
        gid <- rhdf5::H5Gopen(hdf, path_m)
        name_m <- ebv_i_read_att(gid, 'standard_name')
        description_m <- ebv_i_read_att(gid, 'long_name')
        rhdf5::H5Gclose(gid)
        metric <- list('name' = name_m, 'description'=description_m)
      }

      #cube info ----
      # open datacube
      did <- rhdf5::H5Dopen(hdf, datacubepath)
      fillvalue <- ebv_i_read_att(did, '_FillValue')
      coverage_content_type <- ebv_i_read_att(did, 'coverage_content_type')
      units_d <- ebv_i_read_att(did, 'units')
      #get type
      info <- utils::capture.output(did)
      rhdf5::H5Dclose(did)
      indices <- stringr::str_locate(info, ' type')
      for (row in 1:dim(indices)[1]){
        if (!is.na(indices[row,1])){
          i <- c(row, indices[row,])
        }
      }
      type <- as.vector(info)[i[1]]
      type <- stringr::str_remove(type, 'type')
      type <- stringr::str_replace_all(type, stringr::fixed(" "), "")
      ebv_cube <- list('units'=units_d, 'coverage_content_type'=coverage_content_type,
                       'fillvalue'=fillvalue, 'type'=type)

    }else{
      scenario <- list('status'='Only available with datacube argument.')
      metric <- list('status'='Only available with datacube argument.')
      ebv_cube <- list('status'='Only available with datacube argument.')
    }

    prop <-  methods::new('EBV netCDF properties',
                          general = general,
                          spatial=spatial,
                          temporal=temporal,
                          metric=metric,
                          scenario=scenario,
                          ebv_cube=ebv_cube)

  }

  #close file
  rhdf5::H5Fclose(hdf)

  return(prop)
}
