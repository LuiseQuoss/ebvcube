#' Add data to self-created EBV NetCDF
#'
#' @description Add data to the self-created EBV NetCDF from GeoTiffs.
#'
#' @param filepath_nc Path to the self-created NetCDF file.
#' @param filepath_tif Path to the GeoTiff file containing the data.
#' @param metric Default: 1. Indicates to which metric the data should be added.
#' @param scenario Default: NULL. Indicates to which scenario the data should be
#'   added.
#' @param entity Default: NULL. Indicates to which entity the data should be
#'   added.
#' @param timestep Default: 1. Define to which timestep or timesteps the data
#'   should be added. If several timesteps are given they have to be in a
#'   continuous order. Meaning c(4,5,6) is right but c(2,5,6) is wrong.
#' @param band Default: 1. Define which band(s) to read from GeoTiff. Can be
#'   several. Don't have to be in order as the timesteps definition requires.
#' @param ignore.RAM Checks if there is enough space in your memory to read the
#'   data. Can be switched off (set to TRUE).
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @note If the data exceeds your memory the RAM check will throw an error. No
#'   block-processing or other method implemented so far. Move to a machine with
#'   more capacities for the moment.
#'
#' @return Adds data to the EBV NetCDF. Check your results using
#'   [ebvnetcdf::ebv_datacubepaths()], [ebvnetcdf::ebv_data_read()] and/or
#'   [ebvnetcdf::ebv_data_analyse()].
#' @export
#'
#' @examples
#' # file <- 'path/to/created/netcdf/file.nc'
#' # tif <- 'path/to/geotiff/containing/data.tif'
#' # scenario <- 1
#' # metric <- 1
#' # entity <- 1
#' # ts <- c(1:6)
#' # band <- c(1:6)
#' # ebv_ncdf_add_data(file, tif, metric, scenario, entity, ts, band)
ebv_ncdf_add_data <- function(filepath_nc, filepath_tif, datacubepath,
                                  timestep=1, band=1, ignore.RAM=FALSE, verbose=FALSE){
  ### start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
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
    if(exists('gid')){
      if(rhdf5::H5Iis_valid(gid)==TRUE){rhdf5::H5Gclose(gid)}
    }
  )
  withr::defer(
    if(exists('did')){
      if(rhdf5::H5Iis_valid(did)==TRUE){rhdf5::H5Dclose(did)}
    }
  )
  withr::defer(
    tryCatch(ncdf4::nc_close(nc), error=function(e) print('file closed'))
  )

  #are all arguments given?
  if(missing(filepath_nc)){
    stop('Filepath_nc argument is missing.')
  }
  if(missing(filepath_tif)){
    stop('Filepath_tif argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
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

  #check logical arguments
  if(checkmate::checkLogical(ignore.RAM) != TRUE){
    stop('ignore.RAM must be of type logical.')
  }

  #check if nc file exists
  if (checkmate::checkCharacter(filepath_nc) != TRUE){
    stop('NetCDF Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath_nc) != TRUE){
    stop(paste0('NetCDF File does not exist.\n', filepath_nc))
  }
  if (!endsWith(filepath_nc, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
  }

  #check if tif file exists
  if (checkmate::checkCharacter(filepath_tif) != TRUE){
    stop('GeoTiff Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath_tif) != TRUE){
    stop(paste0('GeoTiff does not exist.\n', filepath_nc))
  }
  if (!endsWith(filepath_tif, '.tif')){
    stop(paste0('GeoTiff file ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath_nc)

  # open file
  hdf <- rhdf5::H5Fopen(filepath_nc)

  #check datacubepath
  if (checkmate::checkCharacter(datacubepath) != TRUE & !is.null(datacubepath)){
    stop('Datacubepath must be of type character.')
  }
  if(!is.null(datacubepath)){
    if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
      stop(paste0('The given variable is not valid:\n', datacubepath))
    }
  }
  rhdf5::H5Fclose(hdf)

  #check timesteps
  #check if timestep is valid type
  if(checkmate::checkIntegerish(timestep) != TRUE){
    stop('Timestep has to be an integer or a list of integers.')
  }

  #check timestep range
  max_time <- length(rhdf5::h5read(filepath_nc,'time'))
  min_time <- 1
  if(checkmate::checkIntegerish(timestep, lower=min_time, upper=max_time) != TRUE){
    stop(paste0('Chosen timestep ', paste(timestep, collapse = ' '), ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
  }

  #check if band is valid type
  if(checkmate::checkIntegerish(band) != TRUE){
    stop('Band has to be an integer or a list of integers.')
  }

  #check if timesteps and bands have the same length
  if (length(band) != length(timestep)){
    stop('The amount of bands to read from Tiff and the amount of timesteps to write to NetCDF differ. Have to be the same.')
  }
  #check if timesteps and tif have the same length/amount of bands
  tif_info <- gdalUtils::gdalinfo(filepath_tif)
  b.count <- stringr::str_count(tif_info, 'Band')
  b.sum <- sum(b.count, na.rm=T)
  if (b.sum < length(timestep)){
    stop('The amount of timesteps to write to NetCDF is longer than the available bands in Tiff.')
  }
  #check if band available in Tif
  if (max(band) > b.sum){
    stop('The highest band that should be used exceeds the amount of bands in GeoTiff File.')
  }

  #check needed RAM to read tif info
  #get dims
  index <- stringr::str_detect(tif_info, 'Size is')
  size.chr <- tif_info[index]
  size.int <- as.integer(regmatches(size.chr, gregexpr("[[:digit:]]+", size.chr))[[1]])
  #get type
  index <- stringr::str_detect(tif_info, 'Type=')
  type.chr <- tif_info[index][1]
  if (stringr::str_detect(type.chr, 'Float')){
    type.long <- 'xx_xx_Float'
  } else if (stringr::str_detect(type.chr, 'CFloat')){
    type.long <- 'xx_xx_Float'
  } else if (stringr::str_detect(type.chr, 'Int')){
    type.long <- 'xx_xx_Int'
  } else if (stringr::str_detect(type.chr, 'UInt')){
    type.long <- 'xx_xx_Int'
  } else if (stringr::str_detect(type.chr, 'CInt')){
    type.long <- 'xx_xx_Int'
  }

  if (!ignore.RAM){
    ebv_i_check_ram(size.int,timestep,type.long)
  } else{
    message('RAM capacities are ignored.')
  }

  #check if dims of tif data correspond to lat and lon in netcdf
  lat.len <- length(rhdf5::h5read(filepath_nc, 'lat'))
  lon.len <- length(rhdf5::h5read(filepath_nc, 'lon'))
  if ((size.int[1] != lon.len) & (size.int[2] != lat.len)){
    stop(paste0('The size of your GeoTiff does not correspond to the latitude and longitude coordinates.
  Size should be: ', lon.len, ', ', lat.len, '. But is: ', size.int[1], ', ', size.int[2]))
  }
  if (size.int[1] != lon.len) {
    stop(paste0('The size of your GeoTiff doesn not match the longitudinal coordinates.
  Size sould be: ', lon.len, '. But size is: ', size.int[1]))
  } else if (size.int[2] != lat.len){
    stop(paste0('The size of your GeoTiff doesn not match the longitudinal coordinates.
  Size sould be: ', lat.len, '. But size is: ', size.int[2]))
  }

  ### end initial test ----

  #open hdf file ----
  #hdf <- rhdf5::H5Fopen(filepath_nc)

  #get data from tif ----
  if (length(timestep) > 1){
    raster <- raster::brick(filepath_tif)[[band]] #RAM PROBLEM FOR HUGE DATA!!! read into delayed array?
    raster <- raster::brick(raster) #convert raster stack to raster brick
  } else{
    raster <- raster::raster(filepath_tif, band) #RAM PROBLEM FOR HUGE DATA!!! read into delayed array?
  }

  #get value range from tif
  if (raster@data@haveminmax){
    min <- min(raster@data@min)
    max <- max(raster@data@max)
    value_range <- c(min, max)
  }else{
    value_range <- 'None'
  }

  #get fill value from tif
  nodata <- raster@file@nodatavalue
  if (nodata == '-Inf'){
    nodata = 'None'
  }
  #get data type from tif
  type.hdf<- ebv_i_type_raster(raster@file@datanotation, raster@file@byteorder)

  #read amount of entities from hdf file
  #len.e <- length(rhdf5::h5read(hdf, 'var_entity'))

  #rotate data
  if (length(timestep) > 1){
    data <- array(NA, dim=c(dim(raster)[2], dim(raster)[1], dim(raster)[3]))
    for (i in 1:length(timestep)){
      temp <- as.matrix(raster::as.array(raster)[,,i])
      temp <- t(temp[nrow(temp):1,])
      temp <- temp[,ncol(temp):1]
      data[,,i] <- temp
    }
  } else {
    data <- raster::as.matrix(raster)
    data <- t(data[nrow(data):1,])
    data <- data[,ncol(data):1]
  }

  nc<- ncdf4::nc_open(outputpath, write=T)
  ncdf4::ncvar_put(nc = nc, varid = datacubepath, vals = data, start=c(1,1,min(timestep)), count=c(dim(data)[1:2], length(timestep)), verbose=verbose)
  ncdf4::nc_close(nc)

  #add entity attributes----
  #open file
  hdf <- rhdf5::H5Fopen(filepath_nc)
  #open DS
  did <- rhdf5::H5Dopen(hdf, datacubepath)

  #grid_mapping
  ebv_i_char_att(did, 'grid_mapping', 'crs')

  # :valid_range
  if (! rhdf5::H5Aexists(did, 'valid_range')){
    sid <- rhdf5::H5Screate_simple(length(value_range))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
    #H5Tset_size(tid, max(nchar(ebv_subgroups_desc))+1)
    aid <- rhdf5::H5Acreate(did,'valid_range', tid,sid)
    rhdf5::H5Awrite(aid, value_range)
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  } else{
    old.vr <- ebv_i_read_att(did, 'valid_range')
    old.min <- old.vr[1]
    old.max <- old.vr[2]
    new.vr <- old.vr
    if (old.min > value_range[1]){
      new.vr[1] <- value_range[1]
    }
    if (old.max < value_range[2]){
      new.vr[2] <- value_range[2]
    }
    if ((old.min != new.vr[1])|(old.max != new.vr[2])){
      value_range <- new.vr
      rhdf5::H5Adelete(did, 'valid_range')
      sid <- rhdf5::H5Screate_simple(length(value_range))
      tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
      aid <- rhdf5::H5Acreate(did, name = 'valid_range', tid,sid)
      rhdf5::H5Awrite(aid, value_range)
      rhdf5::H5Aclose(aid)
      rhdf5::H5Sclose(sid)
    }
  }

  # :description
  ebv_i_char_att(did, 'description', 'default')

  # :least_significant_digit = 4; // int
  ebv_i_int_att(did, 'least_significant_digit', 4)

  #attributes that are filled by user - created empty
  # :label = "forest bird species";
  ebv_i_char_att(did, 'standard_name', 'default')

  #delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}
