#' EBV NetCDF Properties class (S4)
#'
#' @slot general list. Elements: title, description, ebv_class, ebv_name, ebv_subgroups,
#'   creator
#' @slot spatial list.
#' @slot temporal list.
#' @slot metric list.
#' @slot scenario list.
#' @slot entity list. Elements: value_range
#'
#' @return S4 class
#' @export
#'
#' @examples
methods::setClass("EBV NetCDF properties", slots=list(general="list",
                                                      spatial="list",
                                                      temporal="list",
                                                      metric="list",
                                                      scenario="list",
                                                      entity="list"))

#' Read properties of EBV NetCDF
#'
#' @description Structured access to all attributes of the NetCDF file.
#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Optional. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return S4 class containing information about file or file and datacube
#'   depending on input.
#' @export
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # prop_file <- ebv_properties(file)
#' # prop_dc <- ebv_properties(file, datacubes[1,1])
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
  if(checkmate::checkLogical(verbose) != TRUE){
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
  hdf <- rhdf5::H5Fopen(filepath)

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
  #get depth of nested structure
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

  # #get value_range
  # value_range <- ebv_i_read_att(hdf, 'value_range')
  # if(is.null(value_range)){
  #   value_range <- array(NA)
  # }

  #get more global attributes
  ebv_name <- ebv_i_read_att(hdf, 'ebv_name')
  ebv_class <- ebv_i_read_att(hdf, 'ebv_class')
  ebv_subgroups <- ebv_i_read_att(hdf, 'ebv_subgroups')
  creator <- ebv_i_read_att(hdf, 'creator')

  #general
  general <- list(title=title, description=description.file,
                  ebv_class=ebv_class, ebv_name=ebv_name, ebv_subgroups=ebv_subgroups,
                  creator=creator)#, value_range=value_range)

  #get srs ----
  srs.ds <- rhdf5::H5Dopen(hdf, 'crs')
  srs.chr <- ebv_i_read_att(srs.ds, 'spatial_ref')
  srs <- sp::CRS(srs.chr)
  rhdf5::H5Dclose(srs.ds)

  #get epsg
  epsg <- as.integer(rgdal::showEPSG(srs.chr))

  #get dims
  dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time))

  #create spatial list for S4 class
  spatial <- list(srs=srs, epsg=epsg, resolution=resolution, extent=extent, dimensions=dims)

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
  time.natural <- as.Date(timesteps-add, origin='1970-01-01')

  #close data handle
  rhdf5::H5Dclose(time.ds)

  #create temporal list for S4 class
  temporal <- list(units=time, t_delta=t_delta, timesteps=timesteps, timesteps.natural=time.natural)

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
      long_name <- ebv_i_read_att(dh, 'long_name')[1]

      #label
      if (rhdf5::H5Aexists(dh, 'label')){
        label <- ebv_i_read_att(dh, 'label')[1]
      } else if (rhdf5::H5Aexists(dh, 'standard_name')){
          label <- ebv_i_read_att(dh, 'standard_name')[1]
      } else {
        label <- 'not defined'
      }

      #unit
      unit <- ebv_i_read_att(dh, 'units')[1]
      #only because of error in globES - delete when corrected!
      if (is.null(unit)){
        unit <- ebv_i_read_att(dh, 'unit')[1]
      }

      #get value_range
      did <- rhdf5::H5Dopen(hdf, datacubepath)
      if (rhdf5::H5Aexists(did, 'valid_range')){
        value_range <- ebv_i_read_att(dh, 'valid_range')
      } else {
        value_range <- ebv_i_read_att(hdf, 'value_range')
      }
      # catch mistakes of old netcdfs
      if(is.null(value_range)){
        value_range <- array(NA)
      }

      #close data handle
      rhdf5::H5Dclose(dh)

      #create object of S4 class
      entity <- list(long_name=long_name, label=label, unit=unit, type=type, fillvalue=fillvalue, value_range=value_range)

      ####get metric info ----
      #one level higher
      i <- length(stringr::str_split(datacubepath, '/')[[1]])
      group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
      dh <- rhdf5::H5Gopen(hdf, group)

      #label
      if (rhdf5::H5Aexists(dh, 'label')){
        label <- ebv_i_read_att(dh, 'label')[1]
      } else if (rhdf5::H5Aexists(dh, 'standard_name')){
        label <- ebv_i_read_att(dh, 'standard_name')[1]
      } else {
        label <- 'not defined'
      }

      #description
      description <- ebv_i_read_att(dh, 'description')[1]

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      metric <- list(label=label, description=description)

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
        label <- ebv_i_read_att(dh, 'label')[1]
      } else if (rhdf5::H5Aexists(dh, 'standard_name')){
        label <- ebv_i_read_att(dh, 'standard_name')[1]
      } else {
        label <- 'not defined'
      }

      #description
      description <- ebv_i_read_att(dh, 'description')

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      scenario <- list(label=label, description=description)

      #create object of S4 class ----
      prop <- methods::new('EBV NetCDF properties',
                           general = general,
                           spatial=spatial,
                           temporal=temporal,
                           metric=metric,
                           scenario=scenario,
                           entity=entity)

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
      long_name <- ebv_i_read_att(dh, 'long_name')[1]

      #label
      if (rhdf5::H5Aexists(dh, 'label')){
        label <- ebv_i_read_att(dh, 'label')[1]
      } else if (rhdf5::H5Aexists(dh, 'standard_name')){
        label <- ebv_i_read_att(dh, 'standard_name')[1]
      } else {
        label <- 'not defined'
      }

      #unit
      unit <- ebv_i_read_att(dh, 'units')[1]

      #get value_range
      if (rhdf5::H5Aexists(dh, 'valid_range')){
        value_range <- ebv_i_read_att(dh, 'valid_range')
      } else {
        value_range <- ebv_i_read_att(hdf, 'value_range')
      }
      # catch mistakes of old netcdfs
      if(is.null(value_range)){
        value_range <- array(NA)
      }

      #close data handle
      rhdf5::H5Dclose(dh)

      #create object of S4 class
      entity <- list(long_name=long_name, label=label, unit=unit, type=type, fillvalue=fillvalue, value_range=value_range)

      ####get metric info ----
      #one level higher
      i <- length(stringr::str_split(datacubepath, '/')[[1]])
      group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
      dh <- rhdf5::H5Gopen(hdf, group)

      #label
      if (rhdf5::H5Aexists(dh, 'label')){
        label <- ebv_i_read_att(dh, 'label')[1]
      } else if (rhdf5::H5Aexists(dh, 'standard_name')){
        label <- ebv_i_read_att(dh, 'standard_name')[1]
      } else {
        label <- 'not defined'
      }

      #description
      description <- ebv_i_read_att(dh, 'description')[1]

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      metric <- list(label=label, description=description)

      #define new S4 class
      prop <- methods::new('EBV NetCDF properties',
                           general = general,
                           spatial=spatial,
                           temporal=temporal,
                           metric=metric,
                           scenario=list(status='Datacube has no scenario.'),
                           entity=entity)

    }
    #else{
    #   # e ----
    #   #only for hennekens! - delete when hennekens is deleted!
    #   ####get entity info ----
    #   dh <- hdf&datacubepath
    #
    #   #get type
    #   info <- utils::capture.output(dh)
    #   indices <- stringr::str_locate(info, ' type')
    #   for (row in 1:dim(indices)[1]){
    #     if (!is.na(indices[row,1])){
    #       i <- c(row, indices[row,])
    #     }
    #   }
    #   type <- as.vector(info)[i[1]]
    #   type <- stringr::str_remove(type, 'type')
    #   type <- stringr::str_replace_all(type, stringr::fixed(" "), "")
    #
    #   #get fillvalue
    #   fillvalue <- ebv_i_read_att(dh, '_FillValue')
    #   if(is.null(fillvalue)){
    #     fillvalue <- array(NA)
    #   }
    #
    #   #long name
    #   long_name <- ebv_i_read_att(dh, 'long_name')[1]
    #
    #   #label
    #   if (rhdf5::H5Aexists(dh, 'label')){
    #     label <- ebv_i_read_att(dh, 'label')[1]
    #   } else if (rhdf5::H5Aexists(dh, 'standard_name')){
    #     label <- ebv_i_read_att(dh, 'standard_name')[1]
    #   } else {
    #     label <- 'not defined'
    #   }
    #
    #   #unit
    #   unit <- ebv_i_read_att(dh, 'units')[1]
    #
    #   #get value_range
    #   if (rhdf5::H5Aexists(dh, 'valid_range')){
    #     value_range <- ebv_i_read_att(dh, 'valid_range')
    #   } else {
    #     value_range <- ebv_i_read_att(hdf, 'value_range')
    #   }
    #   # catch mistakes of old netcdfs
    #   if(is.null(value_range)){
    #     value_range <- array(NA)
    #   }
    #
    #   #close data handle
    #   rhdf5::H5Dclose(dh)
    #
    #   #create object of S4 class
    #   entity <- list(long_name=long_name, label=label, unit=unit, type=type, fillvalue=fillvalue, value_range=value_range)
    #
    #   #define new S4 class
    #   prop <- methods::new('EBV NetCDF properties',
    #                        general = general,
    #                        spatial=spatial,
    #                        temporal=temporal,
    #                        metric=list(status='Datacube has no metric. Error in standard - will be corrected soon.'),
    #                        scenario=list(status='Datacube has no scenario.'),
    #                        entity=entity)
    #
    # }
    # define S4 for filepath only ----
  } else {
    prop <-  methods::new('EBV NetCDF properties',
                          general = general,
                          spatial=spatial,
                          temporal=temporal,
                          metric=list(status='Only available with datacube argument.'),
                          scenario=list(status='Only available with datacube argument.'),
                          entity=list(status='Only available with datacube argument.')
                          )
  }

  #close file
  rhdf5::H5Fclose(hdf)

  return(prop)
}
