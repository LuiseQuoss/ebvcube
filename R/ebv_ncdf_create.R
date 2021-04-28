#' Create an EBV NetCDF
#'
#' @description Create the core structure of the EBV NetCDF based on the json
#'   from the \href{https://portal.geobon.org/api-docs}{Geobon Portal API}. Data
#'   and attributes will be added afterwards. See
#'   [ebvnetcdf::ebv_ncdf_add_data()] and
#'   [ebvnetcdf::ebv_ncdf_entity_attributes()] for the next steps.
#'
#' @param jsonpath Path to the json file downloaded from the
#'   \href{https://portal.geobon.org/api-docs}{Geobon Portal API}.
#' @param outputpath Set path where the NetCDF file should be created.
#' @param entities.no Default: 0. Indicates how many entities there are be per
#'   metric.
#' @param epsg Default: 4326 (WGS84). Defines the coordinate reference system
#'   via the corresponding epsg code.
#' @param extent Default: c(-180,180,-90,90). Defines the extent of the data:
#'   c(xmin, xmax, ymin, ymax).
#' @param overwrite Default: FALSE. Set to TRUE to overwrite the outputfile
#'   defined by 'outputpath'.
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @note To check out the results take a look at your NetCDF file with
#'   \href{https://www.giss.nasa.gov/tools/panoply/}{Panoply}.
#'
#' @return Creates the NetCDF at the 'outputpath' location.
#' @export
#'
#' @examples
#' # json <- 'path/to/json/file.json'
#' # out <- 'path/to/create/new/netcdf/file.nc'
#' # ebv_ncdf_create(json, out, 5)
ebv_ncdf_create <- function(jsonpath, outputpath, entities.no=0, epsg=4326, extent= c(-180,180,-90,90), overwrite=FALSE,verbose=FALSE){
  #turn off local warnings if verbose=TRUE ----
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  # ensure file and all datahandles are closed on exit ----
  defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  gids <- c('mgid', 'sgid')
  defer(
    for (id in gids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Gclose(id)}
      }
    }
  )
  dids <- c('mcrs.id', 'crs.id', 'lat.id', 'lon.id', 'time.id', 'var_entity.id')
  defer(
    for (id in dids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Gclose(id)}
      }
    }
  )
  defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  defer(
    if(exists('sid')){
      if(rhdf5::H5Iis_valid(sid)==TRUE){rhdf5::H5Sclose(sid)}
    }
  )

  # start initial tests ----
  #are all arguments given?
  if(missing(jsonpath)){
    stop('Jsonpath argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }

  #check if json exists
  if (!file.exists(jsonpath)){
    stop(paste0('Json file does not exist.\n', jsonpath))
  }
  if (!endsWith(jsonpath, '.json')){
    stop(paste0('Json file ending is wrong. File cannot be processed.'))
  }

  #check if ouputpath exists
  if(!dir.exists(dirname(outputpath))){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  if (file.exists(outputpath) & overwrite==FALSE){
    stop('Output file already exists. Enable overwrite or delete file.')
  }

  #check if epsg is valid
  #valid epsg check
  epsg_list <- rgdal::make_EPSG()
  if (epsg != 4326){
    if (! epsg %in% epsg_list$code){
      stop(paste0('The given epsg is not valid or not supported by R.\n', epsg))
    }
  }

  # end initial tests -------------------------------------------------------

  #overwrite --> delete file
  if (file.exists(outputpath) & overwrite==TRUE){
    file.remove(outputpath)
  }

  # create empty hdf5 file -------------------------------------------------------
  rhdf5::h5createFile(outputpath)
  hdf <- rhdf5::H5Fopen(outputpath)

  #read json
  file <- jsonlite::fromJSON(txt=jsonpath)
  #json root
  json <- file$data

  # get basic collection info
  collection <- json$collections
  scenarios.no <- length(grep("^scenario", names(collection)))
  metrics.no <- length(grep("^metric", names(collection)))

  # mandatory global attributes -------------------------------------------------------
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
  }
  #add global.att to netcdf
  for (i in 1:length(global.att)){
    att.txt <- eval(parse(text = paste0('json$', global.att[i][[1]])))
    ebv_i_char_att(hdf, names(global.att[i]), att.txt)
  }

  if(!scenarios.no==0){
    scenarios.list <- c()
    scenarios.ebv <- c()
    scenarios.name <- c()

    for (i in 0:(scenarios.no-1)){
      txt <- paste0('json$collections$scenario', i, '$description')
      value <- eval(parse(text = txt))
      scenarios.list <- c(scenarios.list, value)
      value.ebv <- stringr::str_split(value, '-')[[1]][1]
      scenarios.ebv = c(scenarios.ebv, value.ebv)
      txt <- paste0('json$collections$scenario', i, '$name')
      value.name <- eval(parse(text = txt))
      scenarios.name <- c(scenarios.name, value.name)
    }
  }

  # additional global attributes -------------------------------------------------------
  # ebv var scenario
  # ebv var scenatio desc
  # ebv var estimate
  # ebv var estimate desc
  # ebv var metric
  # ebv var metric desc
  # ebv subgroups ----
  if(scenarios.no>0 & entities.no>0){
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
  } else if (entities.no> 0){
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

  # crs ---------------------------------------------------------------------
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
  crs <- sp::CRS(SRS_string = paste0("EPSG:", epsg))
  ref <- sp::wkt(crs) #WKT2 --> GEOGCRS
  #ref <- "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AXIS[\"Latitude\",NORTH],AXIS[\"Longitude\",EAST],AUTHORITY[\"EPSG\",\"4326\"]]"
  ebv_i_char_att(crs.id, 'spatial_ref', ref)

  # :GeoTransform
  res <- json$spatialDomain$spatialResolution
  geo_trans <- paste0(extent[1], " ",res," 0.0 ", extent[4], " 0.0 -", res)
  ebv_i_char_att(crs.id, 'GeoTransform', geo_trans)

  # :grid_mapping_name
  ebv_i_char_att(crs.id, 'grid_mapping_name', 'crs')

  # :longitude_of_prime_meridian
  ebv_i_num_att(crs.id, 'longitude_of_prime_meridian', '0.0')

  # :semi_major_axis
  ebv_i_num_att(crs.id, 'semi_major_axis', '6378137.0')

  # :inverse_flattening
  ebv_i_num_att(crs.id, 'inverse_flattening', '298.257223563')

  #close ds
  rhdf5::H5Dclose(crs.id)

  # lat ---------------------------------------------------------------------
  # create dataset
  res <- as.numeric(res)
  lat.min <- extent[3]
  lat.max <- extent[4]
  lat.data <- seq((lat.min+(res/2)), (lat.max-(res/2)), res)
  sid <- rhdf5::H5Screate_simple(length(lat.data))
  tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
  lat.id <- rhdf5::H5Dcreate(hdf, 'lat', tid, sid)
  rhdf5::H5Dwrite(lat.id, lat.data, h5spaceMem = sid, h5spaceFile = sid)
  rhdf5::H5Sclose(sid)

  # :standard_name = "latitude";
  ebv_i_char_att(lat.id, 'standard_name', 'latitude')

  # :description = "latitude coordinates";
  ebv_i_char_att(lat.id, 'description', 'latitude coordinates')

  # :units = "degrees_north";
  ebv_i_char_att(lat.id, 'units', 'degrees_north')

  # :axis = "Y";
  ebv_i_char_att(lat.id, 'axis', 'Y')

  # :long_name = "latitude";
  # ebv_i_char_att(lat.id, 'long_name', 'latitude')

  #close dataset
  rhdf5::H5Dclose(lat.id)

  # lon ---------------------------------------------------------------------
  #create dataset
  lon.min <- extent[1]
  lon.max <- extent[2]
  lon.data <- seq((lon.min+(res/2)),(lon.max-(res/2)), res)
  sid <- rhdf5::H5Screate_simple(length(lon.data))
  tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
  lon.id <- rhdf5::H5Dcreate(hdf, 'lon', tid, sid)
  rhdf5::H5Dwrite(lon.id, lon.data, h5spaceMem = sid, h5spaceFile = sid)
  rhdf5::H5Sclose(sid)

  # :standard_name = "longitude";
  ebv_i_char_att(lon.id, 'standard_name', 'longitude')

  # :description = "longitude coordinates";
  ebv_i_char_att(lon.id, 'description', 'longitude coordinates')

  # :units = "degrees_east";
  ebv_i_char_att(lon.id, 'units', 'degrees_east')

  # :axis = "X";
  ebv_i_char_att(lon.id, 'axis', 'X')

  # # :long_name = "longitude";
  # ebv_i_char_att(lon.id, 'long_name', 'longitude')

  #close dataset
  rhdf5::H5Dclose(lon.id)

  # create hierachy -------------------------------------------------------
  # 1. metric, no scenario (entities are not relevant)
  if(scenarios.no==0){
    len.m <- nchar(as.character(metrics.no))+1
    for (j in 0:(metrics.no-1)){
      #create metric group
      ending.m <- as.character(j)
      while(nchar(ending.m)<len.m){
        ending.m <- paste0('0',ending.m)
      }
      rhdf5::h5createGroup(hdf, paste0('metric', ending.m))
      mgid <- rhdf5::H5Gopen(hdf, paste0('metric', ending.m))
      #add metric attributes
      label <- eval(parse(text=paste0('json$collections$metric',j,'$unit')))
      description <- eval(parse(text=paste0('json$collections$metric',j,'$description')))
      ebv_i_char_att(mgid, 'label', label)
      ebv_i_char_att(mgid, 'description', description)
      #add subgroup 'crs' to metric
      sid <- rhdf5::H5Screate_simple(1)
      tid <- rhdf5::H5Tcopy("H5T_C_S1")
      mcrs.id <- rhdf5::H5Dcreate(mgid, 'crs', tid, sid)
      rhdf5::H5Sclose(sid)
      # :spatial_ref
      ebv_i_char_att(mcrs.id, 'spatial_ref', ref)
      # :GeoTransform = "-180.0 0.25 0.0 90.0 0.0 -0.25";
      ebv_i_char_att(mcrs.id, 'GeoTransform', geo_trans)
      #close datahandle
      rhdf5::H5Gclose(mgid)
      rhdf5::H5Dclose(mcrs.id)
    }
    #2. scenario and metric (entities are not relevant)
  } else {
    len.s <- nchar(as.character(scenarios.no))+1
    len.m <- nchar(as.character(metrics.no))+1

    for (i in 0:(scenarios.no-1)){
      #create scenario group
      ending.s <- as.character(i)
      while(nchar(ending.s)<len.s){
        ending.s <- paste0('0',ending.s)
      }
      rhdf5::h5createGroup(hdf, paste0('scenario', ending.s))
      sgid <- rhdf5::H5Gopen(hdf, paste0('scenario', ending.s))
      #add scenario attributes
      label <- scenarios.list[i+1]
      description <- scenarios.name[i+1]
      ebv_i_char_att(sgid, 'label', label)
      ebv_i_char_att(sgid, 'description', description)

      for (j in 0:(metrics.no-1)){
        #create metric group
        ending.m <- as.character(j)
        while(nchar(ending.m)<len.m){
          ending.m <- paste0('0',ending.m)
        }
        rhdf5::h5createGroup(sgid, paste0('metric', ending.m))
        mgid <- rhdf5::H5Gopen(sgid, paste0('metric', ending.m))
        #add metric attributes
        label <- eval(parse(text=paste0('json$collections$metric',j,'$unit')))
        description <- eval(parse(text=paste0('json$collections$metric',j,'$description')))
        ebv_i_char_att(mgid, 'label', label)
        ebv_i_char_att(mgid, 'description', description)
        #add subgroup 'crs' to metric
        sid <- rhdf5::H5Screate_simple(1)
        tid <- rhdf5::H5Tcopy("H5T_C_S1")
        mcrs.id <- rhdf5::H5Dcreate(mgid, 'crs', tid, sid)
        rhdf5::H5Sclose(sid)
        # :spatial_ref
        ebv_i_char_att(mcrs.id, 'spatial_ref', ref)
        # :GeoTransform
        ebv_i_char_att(mcrs.id, 'GeoTransform', geo_trans)
        #close grouphandles
        rhdf5::H5Gclose(mgid)
        rhdf5::H5Dclose(mcrs.id)
      }
      rhdf5::H5Gclose(sgid)
    }

  }
  # something went wrong
  # else{
  #   stop('Cannot create inner EBV NetCDF hierarchy. Please contact the developer of the R package.')
  # }

  # time --------------------------------------------------------------------
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

  #create DS
  sid <- rhdf5::H5Screate_simple(length(timesteps))
  tid <- rhdf5::H5Tcopy("H5T_NATIVE_UINT")
  time.id <- rhdf5::H5Dcreate(hdf, 'time', tid, sid)
  rhdf5::H5Dwrite(time.id, timesteps, h5spaceMem = sid, h5spaceFile = sid)
  rhdf5::H5Sclose(sid)

  # :standard_name = "time";
  ebv_i_char_att(time.id, 'standard_name', 'time')

  # :description
  ebv_i_char_att(time.id, 'description', 'time dimensions of the data')

  # :units = "days since 1860-01-01 00:00:00.0";
  ebv_i_char_att(time.id, 'units', "days since 1860-01-01 00:00:00.0")

  # :t_delta = "5 Years";
  ebv_i_char_att(time.id, 't_delta', t_delta)

  # :axis = "T";
  ebv_i_char_att(time.id, 'axis', 'T')

  # :calendar = "standard";
  ebv_i_char_att(time.id, 'calendar', 'standard')

  # :_ChunkSizes = 1024U; // uint
  ebv_i_uint_att(time.id, '_ChunkSizes', '1024')

  # # :long_name = "time";
  # ebv_i_char_att(time.id, 'long_name', 'time')

  #close group
  rhdf5::H5Dclose(time.id)

  # var_entity --------------------------------------------------------------
  #entity list if entites exist
  if(!entities.no==0){
    sid <- rhdf5::H5Screate_simple(entities.no)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    if ((entities.no-1) < 10){
      zeros <- 1
    } else if ((entities.no-1) < 100){
      zeros <- 2
    } else if ((entities.no-1) < 1000){
      zeros <- 3
    } else if ((entities.no-1) < 10000){
      zeros <- 4
    } else if ((entities.no-1) < 100000){
      zeros <- 5
    }
    rhdf5::H5Tset_size(tid, (nchar('entity0')+zeros))
    var_entity.id <- rhdf5::H5Dcreate(hdf, 'var_entity', tid, sid)
    #create entity list
    entity.list <- c()
    for (e in 0:(entities.no-1)){
      reps <- rep('0', (zeros - nchar(e)))
      ent <- paste0('entity0', paste(reps, collapse = ''), as.character(e))
      entity.list <- c(entity.list, ent)
    }
    #write entity list
    rhdf5::H5Dwrite(var_entity.id, entity.list, h5spaceMem = sid, h5spaceFile = sid)
    rhdf5::H5Sclose(sid)
  } else{
    sid <- rhdf5::H5Screate_simple(1)
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, (nchar('data')+1))
    var_entity.id <- rhdf5::H5Dcreate(hdf, 'var_entity', tid, sid)
    rhdf5::H5Dwrite(var_entity.id, 'data', h5spaceMem = sid, h5spaceFile = sid)
    rhdf5::H5Sclose(sid)
  }

  # :standard_name = "variable entity";
  ebv_i_char_att(var_entity.id, 'standard_name', 'variable entity')

  # :description = "list of all entities / species / species groups";
  ebv_i_char_att(var_entity.id, 'description', 'list of all entities / species / species groups')

  # :unit = "name of entity";
  ebv_i_char_att(var_entity.id, 'unit', 'name of entity')

  # :_ChunkSizes = 512U; // uint
  ebv_i_uint_att(var_entity.id, '_ChunkSizes', '512')

  # # :long_name = "variable entity";
  # ebv_i_char_att(var_entity.id, 'long_name', 'variable entity')

  #close group
  rhdf5::H5Dclose(var_entity.id)

  # close file  ----------------------------------------------------------------
  rhdf5::H5Fclose(hdf)

}
