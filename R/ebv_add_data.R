#' Add data to a self-created EBV netCDF
#'
#' @description Add data to the self-created EBV netCDF from GeoTiffs.
#'
#' @param filepath_nc Character. Path to the self-created netCDF file.
#' @param filepath_tif Character. Path to the GeoTiff file containing the data.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. (As if the structure
#'   were 3D. Then no entity argument is needed.) Character string or single
#'   integer value indicating the entity of the 4D structure of the EBV netCDFs.
#' @param timestep Integer. Default: 1. Define to which timestep or timesteps
#'   the data should be added. If several timesteps are given they have to be in
#'   a continuous order. Meaning c(4,5,6) is right but c(2,5,6) is wrong.
#' @param band Integer. Default: 1. Define which band(s) to read from GeoTiff.
#'   Can be several. Don't have to be in order as the timesteps definition
#'   requires.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note If the data exceeds your memory the RAM check will throw an error. No
#'   block-processing or other method implemented so far. Move to a machine with
#'   more capacities for the moment if needed.
#'
#' @return Adds data to the EBV netCDF. Check your results using
#'   [ebvnetcdf::ebv_read()] and/or [ebvnetcdf::ebv_analyse()].
#' @export
#'
#' @importFrom utils capture.output
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_new.nc"), package="ebvnetcdf")
#' tif <- system.file(file.path("extdata","cSAR_write_ts234.tif"), package="ebvnetcdf")
#' # datacubes <- ebv_datacubepaths(file)
#' ts <- c(2:4)
#' band <- c(1:3)
#' #ebv_add_data(file, tif, datacubepaths[1,1], ts, band)
ebv_add_data <- function(filepath_nc, filepath_tif, datacubepath,entity=NULL,
                              timestep=1, band=1, ignore_RAM=FALSE,
                              verbose=FALSE){
  ### start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  withr::defer(
    if(exists('file_space')){
      if(rhdf5::H5Iis_valid(file_space)==TRUE){rhdf5::H5Sclose(file_space)}
    }
  )
  withr::defer(
    if(exists('did')){
      if(rhdf5::H5Iis_valid(did)==TRUE){rhdf5::H5Dclose(did)}
    }
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
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #check logical arguments
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=F) != TRUE){
    stop('ignore_RAM must be of type logical.')
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

  # get properties
  prop <- ebv_properties(filepath_nc, datacubepath)
  fillvalue <- prop@ebv_cube$fillvalue
  dims <- prop@spatial$dimensions
  entity_names <- prop@general$entity_names

  #check file structure
  is_4D <- ebv_i_4D(filepath_nc)
  if(is_4D){
    if(is.null(entity)){
      stop('Your working with a 4D cube based EBV netCDF. Please specify the entity-argument.')
    }
    #check entity ----
    # check entity name
    ebv_i_entity(entity, entity_names)

    #get entity index
    if(checkmate::checkIntegerish(entity, len=1) == TRUE){
      entity_index <- entity
    } else if (checkmate::checkCharacter(entity)==TRUE){
      entity_index <- which(entity_names==entity)
    } else{
      entity <- 1 #set entity to 1 (for ebv_i_check_ram)
    }
  }

  #check timesteps
  #check if timestep is valid type
  if(checkmate::checkIntegerish(timestep) != TRUE){
    stop('Timestep has to be an integer or a list of integers.')
  }

  #check timestep range
  max_time <- dims[3] #length(rhdf5::h5read(filepath_nc,'time'))
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

  if (!ignore_RAM){
    ebv_i_check_ram(size.int,timestep,entity,type.long)
  } else{
    message('RAM capacities are ignored.')
  }

  #check if dims of tif data correspond to lat and lon in netcdf
  lat.len <- dims[1]
  lon.len <- dims[2]
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

  #get data from tif ----
  if (length(timestep) > 1){
    raster <- raster::brick(filepath_tif)[[band]]
    raster <- raster::brick(raster) #convert raster stack to raster brick
  } else{
    raster <- raster::raster(filepath_tif, band)
  }

  #get fill value from tif
  nodata <- raster@file@nodatavalue
  if(!is.na(nodata)&!is.na(fillvalue)){
    if (nodata != fillvalue){
      message(paste0('The fillvalue of the GeoTiff (value: ',nodata,') differs from
                   the fillvalue of the datacube: ', fillvalue, '.'))
    }
  }


  #rotate data ----
  if (length(timestep) > 1){
    data <- array(raster, dim=c(dim(raster)[2], dim(raster)[1], dim(raster)[3]))
  } else {
    data <- matrix(raster, nrow=dim(raster)[2], ncol=dim(raster)[1])
  }


  #open file
  hdf <- rhdf5::H5Fopen(filepath_nc)

  #set dim of dataset ----
  did <- rhdf5::H5Dopen(hdf, datacubepath)
  file_space <- rhdf5::H5Dget_space(did)
  if (rhdf5::H5Sget_simple_extent_dims(file_space)$size[3] != dims[3]){
    #set new dimension of dataset
    rhdf5::H5Dset_extent(did, c(dims[2],dims[1],dims[3:length(dims)]))
    rhdf5::H5Dclose(did)
    rhdf5::H5Sclose(file_space)
  }else{
    rhdf5::H5Dclose(did)
    rhdf5::H5Sclose(file_space)
  }

  #write data ----

  if(is_4D){
    #write data 4D
    rhdf5::h5write(data, hdf, datacubepath, start=c(1,1,min(timestep),entity_index),
                   count=c(lon.len,lat.len,length(timestep),length(entity)))
  } else{
    #write data 3D
    rhdf5::h5write(data, hdf, datacubepath, start=c(1,1,min(timestep)),
                   count=c(lon.len,lat.len,length(timestep)))
  }

  #open DS
  did <- rhdf5::H5Dopen(hdf, datacubepath)

  #delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}