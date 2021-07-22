#' Create an EBV NetCDF
#'
#' @description Create the core structure of the EBV NetCDF based on the json
#'   from the \href{https://portal.geobon.org/api-docs}{Geobon Portal API}. Data
#'   and attributes will be added afterwards. Use
#'   [ebvnetcdf::ebv_add_data()] to add the missing attributes.
#'
#' @param jsonpath Character. Path to the json file downloaded from the
#'   \href{https://portal.geobon.org/api-docs}{Geobon Portal API}.
#' @param outputpath Character. Set path where the NetCDF file should be
#'   created.
#' @param entities_no Integer. Default: 0. Indicates how many entities there are
#'   per metric.
#' @param epsg Integer. Default: 4326 (WGS84). Defines the coordinate reference
#'   system via the corresponding epsg code.
#' @param extent Numeric. Default: c(-180,180,-90,90). Defines the extent of the
#'   data: c(xmin, xmax, ymin, ymax).
#' @param fillvalue Numeric. Value of the missing data in the array. Not
#'   mandatory but should be defined!
#' @param prec Character. Default: 'double'. Precision of the data set. Valid
#'   options: 'short' 'integer' 'float' 'double' 'char' 'byte'.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note To check out the results take a look at your NetCDF file with
#'   \href{https://www.giss.nasa.gov/tools/panoply/}{Panoply} provided by the
#'   NASA.
#'
#' @return Creates the NetCDF file at the 'outputpath' location.
#' @export
#'
#' @importFrom utils capture.output
#'
#' @examples
#' json <- system.file(file.path("extdata","1.json"), package="ebvnetcdf")
#' out <- file.path(system.file(package='ebvnetcdf'),"extdata","sCAR_new.nc")
#' #ebv_create(json, out, 3, fillvalue=-3.4E38)
ebv_create <- function(jsonpath, outputpath, entities_no=0, epsg=4326,
                            extent= c(-180,180,-90,90), fillvalue = NULL,
                            prec = 'double', overwrite=FALSE,verbose=FALSE){
  # start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  gids <- c('mgid', 'sgid')
  withr::defer(
    for (id in gids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Gclose(id)}
      }
    }
  )
  dids <- c('mcrs.id', 'crs.id', 'lat.id', 'lon.id', 'time.id', 'var_entity.id','dc')
  withr::defer(
    for (id in dids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Gclose(id)}
      }
    }
  )
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  withr::defer(
    if(exists('sid')){
      if(rhdf5::H5Iis_valid(sid)==TRUE){rhdf5::H5Sclose(sid)}
    }
  )
  withr::defer(
    if(exists('nc')){
      tryCatch(l <- utils::capture.output(ncdf4::nc_close(nc)))
    }
  )

  #are all arguments given?
  if(missing(jsonpath)){
    stop('Jsonpath argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
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

  #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }

  #check if json exists
  if (checkmate::checkCharacter(jsonpath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(jsonpath) != TRUE){
    stop(paste0('Json file does not exist.\n', jsonpath))
  }
  if (!endsWith(jsonpath, '.json')){
    stop(paste0('Json file ending is wrong. File cannot be processed.'))
  }

  #check if ouputpath exists
  #outputpath check
  if (checkmate::checkCharacter(outputpath) != TRUE){
    stop('Outputpath must be of type character.')
  }
  if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  if(!endsWith(outputpath, '.nc')){
    stop('Outputpath needs to end with *.nc ')
  }
  #check if outpufile exists if overwrite is disabled
  if(!overwrite){
    if(checkmate::checkPathForOutput(outputpath) != TRUE){
      stop('Output file already exists. Change name or enable overwrite.')
    }
  }

  #check if epsg is valid
  if(checkmate::checkIntegerish(epsg) != TRUE){
    stop('epsg must be of type integer.')
  }
  epsg_list <- rgdal::make_EPSG()
  if (epsg != 4326){
    if (! epsg %in% epsg_list$code){
      stop(paste0('The given epsg is not valid or not supported by R.\n', epsg))
    }
  }

  #check extent
  if (checkmate::checkNumeric(extent, len = 4) != TRUE){
    stop('extent needs to be a list of 4 numeric values.')
  }

  #check entities_no
  if(checkmate::checkInt(entities_no) != TRUE){
    stop('entities_no must be a single integer value.')
  }

  #check prec
  if (! prec %in% c('short', 'integer', 'float', 'double', 'char', 'byte')){
    stop('prec value not valid!')
  }

  #check fillvalue
  if (! is.null(fillvalue)){
    if(checkmate::checkNumber(fillvalue) != TRUE & !is.na(fillvalue)){
      stop('The fillvalue needs to be a single numeric value or NA.')
    }
  }


  # end initial tests ----

  #overwrite --> delete file
  if (file.exists(outputpath) & overwrite==TRUE){
    file.remove(outputpath)
  }

  #read json ----
  file <- jsonlite::fromJSON(txt=jsonpath)
  #json root
  json <- file$data

  # get basic collection info ----
  collection <- json$collections
  scenarios.no <- length(grep("^scenario", names(collection)))
  metrics.no <- length(grep("^metric", names(collection)))

  # get crs information ----
  # :GeoTransform
  res <- json$spatialDomain$spatialResolution
  geo_trans <- paste0(extent[1], " ",res," 0.0 ", extent[4], " 0.0 -", res)

  # :spatial_ref
  crs <- sp::CRS(SRS_string = paste0("EPSG:", epsg))
  ref <- sp::wkt(crs)

  # unit
  parts <- stringr::str_split(ref, "[^[:print:]]")[[1]]
  row <- parts[stringr::str_detect(parts, 'CS')]
  if (stringr::str_detect(row, 'Cartesian') | stringr::str_detect(row, 'cartesian')){
    crs.unit <- 'meters'
  } else{
    crs.unit <- 'degrees'
  }

  # get dimensions ----
  # time ----
  t_delta <- json$temporalDomain$temporalResolution
  t.units <- json$temporalDomain$temporalExtentStart
  t.end <- json$temporalDomain$temporalExtentEnd

  #create timesteps
  add <- 40177
  if (mapply(grepl,'year',t_delta,ignore.case=TRUE)){
    start <- as.integer(stringr::str_split(t.units, '-')[[1]][1])
    end <- as.integer(stringr::str_split(t.end, '-')[[1]][1])
    intervall <- as.numeric(regmatches(t_delta, gregexpr("[[:digit:]]+", t_delta))[[1]][1])
    if(is.na(intervall)){ #yearly
      intervall <- 1
    }
    sequence <- seq(start, end, intervall)
    timesteps <- c()
    for (s in sequence){
      date <- as.numeric(as.Date(paste0(as.character(s),'-01-01'), format = '%Y-%m-%d'))
      timestep <- date+add
      timesteps <- c(timesteps, timestep)
    }
  } else if (mapply(grepl,'month',t_delta,ignore.case=TRUE)){
    print('only implemented for yearly timesteps - not months')
  }else if (mapply(grepl,'day',t_delta,ignore.case=TRUE)){
    print('only implemented for yearly timesteps - not days')
  } else if (mapply(grepl,'decad',t_delta,ignore.case=TRUE)){
    start <- as.integer(stringr::str_split(t.units, '-')[[1]][1])
    end <- as.integer(stringr::str_split(t.end, '-')[[1]][1])
    intervall <- 10
    sequence <- seq(start, end, intervall)
    timesteps <- c()
    for (s in sequence){
      date <- as.numeric(as.Date(paste0(as.character(s),'-01-01'), format = '%Y-%m-%d'))
      timestep <- date+add
      timesteps <- c(timesteps, timestep)
    }
  } else {
    warning('could not detect delta time. empty time dataset created')
    timesteps <- c(0)
  }

  # lat ----
  res <- as.numeric(res)
  lat.min <- extent[3]
  lat.max <- extent[4]
  lat.data <- seq((lat.min+(res/2)), (lat.max-(res/2)), res)

  # lon ----
  lon.min <- extent[1]
  lon.max <- extent[2]
  lon.data <- seq((lon.min+(res/2)),(lon.max-(res/2)), res)

  # entities ----
  if(!entities_no==0){
    if ((entities_no) < 10){
      zeros <- 1
    } else if ((entities_no) < 100){
      zeros <- 2
    } else if ((entities_no) < 1000){
      zeros <- 3
    } else if ((entities_no-1) < 10000){
      zeros <- 4
    } else if ((entities_no) < 100000){
      zeros <- 5
    }
    #create entity list
    entity.list <- c()
    for (e in 1:(entities_no)){
      reps <- rep('0', (zeros - nchar(e)))
      ent <- paste0('entity0', paste(reps, collapse = ''), as.character(e))
      entity.list <- c(entity.list, ent)
    }
  } else {
    entity.list <- c('data')
  }

  # create dimensions ----
  lat.dim <- ncdf4::ncdim_def('lat', crs.unit , vals = lat.data)
  lon.dim <- ncdf4::ncdim_def('lon', crs.unit, vals = lon.data)
  time.dim <- ncdf4::ncdim_def('time', 'days since 1860-01-01 00:00:00.0' , timesteps, unlim = T)

  # create list of vars ----
  var.list <- c()
  units <- c()
  # 1. metric, no scenario
  if(scenarios.no==0){
    len.m <- nchar(as.character(metrics.no))+1
    for (j in 1:(metrics.no)){
      #create metric group
      ending.m <- as.character(j)
      while(nchar(ending.m)<len.m){
        ending.m <- paste0('0',ending.m)
      }
      #metric units list
      units <- c(units,eval(parse(text=paste0('json$collections$metric',j-1,'$unit'))))
      #add entities
      for (ent in entity.list){
        var.list <- c(var.list, paste0('metric', ending.m, '/', ent))
      }
    }
    #2. scenario and metric (entities are not relevant)
  } else {
    len.s <- nchar(as.character(scenarios.no))+1
    len.m <- nchar(as.character(metrics.no))+1

    for (i in 1:(scenarios.no)){
      #create scenario group
      ending.s <- as.character(i)
      while(nchar(ending.s)<len.s){
        ending.s <- paste0('0',ending.s)
      }
      for (j in 1:(metrics.no)){
        #create metric group
        ending.m <- as.character(j)
        while(nchar(ending.m)<len.m){
          ending.m <- paste0('0',ending.m)
        }
        #metric units list
        units <- c(units,eval(parse(text=paste0('json$collections$metric',j-1,'$unit'))))
        #add entities
        for (ent in entity.list){
          var.list <- c(var.list, paste0('scenario', ending.s, '/metric', ending.m, '/', ent))
        }
      }
    }
  }

  var.list.nc <- list()
  enum = 1
  # create all vars ---
  if (!is.null(fillvalue)){
    for (var in var.list){
      metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
      metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
      name <- paste0('var', enum)
      assign(name, ncdf4::ncvar_def(var, units[metric.digit], dim= list(lon.dim, lat.dim, time.dim), missval=fillvalue, compression=2, prec=prec, verbose=verbose))
      var.list.nc[[enum]] <- eval(parse(text=name))
      enum = enum +1
    }
  } else {
    for (var in var.list){
      metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
      metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
      name <- paste0('var', enum)
      assign(name, ncdf4::ncvar_def(var, units[metric.digit], dim= list(lon.dim, lat.dim, time.dim), compression=2, prec=prec, verbose=verbose))
      var.list.nc[[enum]] <- eval(parse(text=name))
      enum = enum +1
    }
  }

  # add all entity vars ----
  # also creates groups
  nc <- ncdf4::nc_create(outputpath, var.list.nc, force_v4 = T, verbose = verbose)

  # close file
  ncdf4::nc_close(nc)

  # use hdf5 to add all attributes ----
  # open file
  hdf <- rhdf5::H5Fopen(outputpath)

  # global attributes ----
  #Conventions = "EBV";
  ebv_i_char_att(hdf, 'Conventions', 'EBV')

  {
    global.att <- list()
    global.att['title'] <- 'title'
    global.att['creator'] <- 'creator$creatorName'
    global.att['institution']<-'creator$creatorOrganisation'
    global.att['description']<-'abstract'
    global.att['contactname']<-'contact$contactName'
    global.att['contactemail']<-'contact$contactEmail'
    global.att['ebv_class']<-'ebv$ebvClass'
    global.att['ebv_name']<-'ebv$ebvName'
    global.att['license'] <- 'licenseName'
  }
  #add global.att to netcdf
  for (i in 1:length(global.att)){
    att.txt <- eval(parse(text = paste0('json$', global.att[i][[1]])))
    ebv_i_char_att(hdf, names(global.att[i]), att.txt)
  }

  # ebv subgroups ----
  if(scenarios.no>0 & entities_no>0){
    sid <- rhdf5::H5Screate_simple(3)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('scenario')+1))
    aid <- rhdf5::H5Acreate(hdf, 'ebv_subgroups', tid, sid)
    rhdf5::H5Awrite(aid, c('scenario', 'metric', 'entity'))
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
    #ebv_i_char_att(hdf, 'ebv_subgroups', '"scenario", "metric", "entity"')
  } else if (scenarios.no>0){
    sid <- rhdf5::H5Screate_simple(2)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('scenario')+1))
    aid <- rhdf5::H5Acreate(hdf, 'ebv_subgroups', tid, sid)
    rhdf5::H5Awrite(aid, c('scenario', 'metric'))
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  } else if (entities_no> 0){
    sid <- rhdf5::H5Screate_simple(2)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('entity')+1))
    aid <- rhdf5::H5Acreate(hdf, 'ebv_subgroups', tid, sid)
    rhdf5::H5Awrite(aid, c('metric', 'entity'))
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  } else {
    sid <- rhdf5::H5Screate_simple(1)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('metric')+1))
    aid <- rhdf5::H5Acreate(hdf, 'ebv_subgroups', tid, sid)
    rhdf5::H5Awrite(aid, 'metric')
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  }

  # add crs variable ----
  #add new ds
  sid <- rhdf5::H5Screate_simple(1)
  tid <- rhdf5::H5Tcopy("H5T_C_S1")
  crs.id <- rhdf5::H5Dcreate(hdf, 'crs', tid, sid)
  rhdf5::H5Sclose(sid)

  # :standard_name
  ebv_i_char_att(crs.id, 'standard_name', 'CRS definition')

  # :standard_name
  ebv_i_char_att(crs.id, 'description', 'CRS definition')

  # :spatial_ref
  ebv_i_char_att(crs.id, 'spatial_ref', ref)

  # :GeoTransform
  ebv_i_char_att(crs.id, 'GeoTransform', geo_trans)

  #close ds
  rhdf5::H5Dclose(crs.id)

  # change lat variable ----
  # open dataset
  lat.id <- rhdf5::H5Dopen(hdf, 'lat')

  # remove long_name
  rhdf5::H5Adelete(lat.id, 'long_name')

  # :standard_name = "latitude";
  ebv_i_char_att(lat.id, 'standard_name', 'latitude')

  # :description = "latitude coordinates";
  ebv_i_char_att(lat.id, 'description', 'latitude coordinates')

  # :units = "degrees_north";
  ebv_i_char_att(lat.id, 'units', paste0(crs.unit, '_north'))

  # :axis = "Y";
  ebv_i_char_att(lat.id, 'axis', 'Y')

  #close dataset
  rhdf5::H5Dclose(lat.id)

  # change lon variable ----
  #open dataset
  lon.id <- rhdf5::H5Dopen(hdf, 'lon')

  # remove long_name
  rhdf5::H5Adelete(lon.id, 'long_name')

  # :standard_name = "longitude";
  ebv_i_char_att(lon.id, 'standard_name', 'longitude')

  # :description = "longitude coordinates";
  ebv_i_char_att(lon.id, 'description', 'longitude coordinates')

  # :units = "degrees_east";
  ebv_i_char_att(lon.id, 'units', paste0(crs.unit, '_east'))

  # :axis = "X";
  ebv_i_char_att(lon.id, 'axis', 'X')

  #close dataset
  rhdf5::H5Dclose(lon.id)

  # change time variable ----
  # open dataset
  time.id <- rhdf5::H5Dopen(hdf, 'time')

  # remove long_name
  rhdf5::H5Adelete(time.id, 'long_name')

  # :standard_name = "time";
  ebv_i_char_att(time.id, 'standard_name', 'time')

  # :description
  ebv_i_char_att(time.id, 'description', 'time dimensions of the data')

  # :t_delta = "5 Years";
  ebv_i_char_att(time.id, 't_delta', t_delta)

  # :axis = "T";
  ebv_i_char_att(time.id, 'axis', 'T')

  # :calendar = "standard";
  ebv_i_char_att(time.id, 'calendar', 'standard')

  #close group
  rhdf5::H5Dclose(time.id)

  # add var_entity variable ----
  if(!entities_no==0){
    sid <- rhdf5::H5Screate_simple(entities_no)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('entity0')+zeros))
    var_entity.id <- rhdf5::H5Dcreate(hdf, 'var_entity', tid, sid)
    rhdf5::H5Dwrite(var_entity.id, entity.list, h5spaceMem = sid, h5spaceFile = sid)
    rhdf5::H5Sclose(sid)
  } else {
    sid <- rhdf5::H5Screate_simple(1)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('data')+1))
    var_entity.id <- rhdf5::H5Dcreate(hdf, 'var_entity', tid, sid)
    rhdf5::H5Dwrite(var_entity.id, 'data', h5spaceMem = sid, h5spaceFile = sid)
    rhdf5::H5Sclose(sid)
  }

  # :standard_name = "variable entity";
  ebv_i_char_att(var_entity.id, 'standard_name', 'variable entity')

  # :standard_name = "variable entity";
  ebv_i_char_att(var_entity.id, 'units', 'name of entity')

  # :description = "list of all entities / species / species groups";
  ebv_i_char_att(var_entity.id, 'description', 'list of all entities / species / species groups')

  # :description = "list of all entities / species / species groups";
  ebv_i_uint_att(var_entity.id, '_ChunkSizes', 512)

  # # :long_name = "variable entity";
  # ebv_i_char_att(var_entity.id, 'long_name', 'variable entity')

  #close group
  rhdf5::H5Dclose(var_entity.id)

  # add metric and scenario attributes ----

  # 1. metric, no scenario (entities are not relevant)
  if(scenarios.no==0){
    j = 0
    for (i in 1:(metrics.no)){
      #create scenario group
      ending.m <- as.character(i)
      while(nchar(ending.m)<len.m){
        ending.m <- paste0('0',ending.m)
      }
      mgid <- rhdf5::H5Gopen(hdf, paste0('metric', ending.m))
      #add metric attributes
      label <- eval(parse(text=paste0('json$collections$metric',j,'$name')))
      description <- eval(parse(text=paste0('json$collections$metric',j,'$description')))
      ebv_i_char_att(mgid, 'standard_name', label)
      ebv_i_char_att(mgid, 'description', description)
      #close data handle
      rhdf5::H5Gclose(mgid)
      j = j +1
    }
    #2. scenario and metric (entities are not relevant)
  }else{
    j = 0
    for (i in 1:(scenarios.no)){
      #open scenario group
      ending.s <- as.character(i)
      while(nchar(ending.s)<len.s){
        ending.s <- paste0('0',ending.s)
      }
      #scenario path
      sgid <- rhdf5::H5Gopen(hdf, paste0('scenario', ending.s))
      #add attributes
      label <- eval(parse(text=paste0('json$collections$scenario',j,'$name')))
      description <- eval(parse(text=paste0('json$collections$scenario',j,'$description')))
      ebv_i_char_att(sgid, 'standard_name', label)
      ebv_i_char_att(sgid, 'description', description)
      rhdf5::H5Gclose(sgid)
      j=j+1
      for (i in 1:(metrics.no)){
        k = 0
        #create scenario group
        ending.m <- as.character(i)
        while(nchar(ending.m)<len.m){
          ending.m <- paste0('0',ending.m)
        }
        #open metric group
        mgid <- rhdf5::H5Gopen(hdf, paste0('scenario', ending.s, '/metric', ending.m))
        #add metric attributes
        label <- eval(parse(text=paste0('json$collections$metric',k,'$name')))
        description <- eval(parse(text=paste0('json$collections$metric',k,'$description')))
        ebv_i_char_att(mgid, 'standard_name', label)
        ebv_i_char_att(mgid, 'description', description)
        #close datahandle
        rhdf5::H5Gclose(mgid)
        k = k +1
      }
    }
  }

  #add entity attributes ----
  datacubepaths <- ebv_datacubepaths(outputpath)
  for (cube in datacubepaths[,1]){
    dc <- rhdf5::H5Dopen(hdf, cube)
    #grid_mapping
    ebv_i_char_att(dc, 'grid_mapping', '/crs')
    #description
    ebv_i_char_att(dc, 'description', 'default')
    # standard_name
    ebv_i_char_att(dc, 'standard_name', 'default')
    #valid_range
    sid <- rhdf5::H5Screate_simple(2)
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
    aid <- rhdf5::H5Acreate(dc,'valid_range', tid,sid)
    rhdf5::H5Awrite(aid, c(0,0))
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
    #close datahandle
    rhdf5::H5Dclose(dc)
  }

  # close file  ----
  rhdf5::H5Fclose(hdf)

}
