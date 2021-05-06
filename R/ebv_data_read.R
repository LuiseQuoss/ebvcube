#' Read datasube from an EBV NetCDF
#'
#' @description Read one or more layers from one datacube of the NetCDF file.
#'   Decide between in-memory array, in-memory raster or an array-like object
#'   (DelayedMatrix) pointing to the on-disk NetCDF file. Latter is useful for
#'   data that exceeds your memory.
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param timestep Integer. Choose one or several timesteps (vector).
#' @param delayed Logical. Default: TRUE. Returns data as DelayedMatrix object.
#'   More timesteps are not returned as a 3D array but as a list of the
#'   DelayedMatrix (one matrix per band).
#' @param sparse Logical. Default: FALSE. Set to TRUE if the data contains a lot
#'   empty raster cells. Only relevant for DelayedMatrix. No further
#'   implementation by now.
#' @param raster Logical. Default: FALSE. Set to TRUE and 'delayed' to FALSE to
#'   get a raster. If both arguments are set to FALSE the function returns an
#'   array.
#' @param ignore.RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note For working with the DelayedMatrix take a look at
#'   [DelayedArray::DelayedArray()] and the
#'   \href{https://www.rdocumentation.org/packages/HDF5Array/versions/1.0.2/topics/DelayedArray-utils}{DelayedArray-utils}.
#'
#' @return Array, Raster or DelayedMatrix object containing the data of the
#'   corresponding datacube and timestep(s).
#' @export
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' #cSAR.delayedarray <- ebv_data_read(file, datacubes[1,1], c(1,6), delayed=T, sparse=T)
#' #cSAR.raster <- ebv_data_read(file, datacubes[1,1], 1, delayed = F, raster = T)
#' #cSAR.array <- ebv_data_read(file, datacubes[1,1], c(1,1,3), delayed = F, raster = F)
ebv_data_read <- function(filepath, datacubepath, timestep, delayed=TRUE, sparse=FALSE, raster=FALSE, ignore.RAM = FALSE, verbose = FALSE){
  ####initial tests start ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }
  if(missing(timestep)){
    stop('Timestep argument is missing.')
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
  if(checkmate::checkLogical(delayed, len=1, any.missing=F) != TRUE){
    stop('delayed must be of type logical.')
  }
  if(checkmate::checkLogical(sparse, len=1, any.missing=F) != TRUE){
    stop('sparse must be of type logical.')
  }
  if(checkmate::checkLogical(raster, len=1, any.missing=F) != TRUE){
    stop('raster must be of type logical.')
  }
  if(checkmate::checkLogical(ignore.RAM, len=1, any.missing=F) != TRUE){
    stop('ignore.RAM must be of type logical.')
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

  #variable check
  if (checkmate::checkCharacter(datacubepath) != TRUE){
    stop('Datacubepath must be of type character.')
  }
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given variable is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

  #get properties
  prop <- ebv_properties(filepath, datacubepath, verbose)

  #timestep check
  #check if timestep is valid type
  if(checkmate::checkIntegerish(timestep) != TRUE){
    stop('Timestep has to be an integer or a list of integers.')
  }

  #check timestep range
  max_time <- prop@spatial$dimensions[3]
  min_time <- 1
  if(checkmate::checkIntegerish(timestep, lower=min_time, upper=max_time) != TRUE){
    stop(paste0('Chosen timestep ', paste(timestep, collapse = ' '), ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
  }

  #warning that raster output will be ignored
  if(delayed==TRUE & raster==TRUE){
    message('raster=TRUE will be ignored as delayed = TRUE.')
  }

  #######initial test end ----

  #get fillvalue
  fillvalue <- prop@entity$fillvalue

  if (delayed==TRUE){
    #return delayed array ----
    #get type
    type.long <- prop@entity$type
    #get numeric type
    type.short <- ebv_i_type_r(type.long)

    #read as H5Array
    all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath, as.sparse = sparse, type = type.short)

    if (length(timestep)>1){
      part <- all[,,timestep]
      h5array <- c()
      #rotate array
      for (i in 1:length(timestep)){
        temp <- t(part[nrow(part):1,,i]) #,drop=FALSE
        temp <- temp[,ncol(temp):1] #,drop=FALSE
        temp <- replace(temp, temp==fillvalue, c(NA))
        h5array <- c(h5array, temp)
      }

    }
    else{
      h5array <- all[,,timestep]
      #rotate matrix
      h5array <- t(h5array[nrow(h5array):1,,drop=FALSE])
      h5array <- h5array[,ncol(h5array):1,drop=FALSE]
      h5array <- replace(h5array, h5array==fillvalue, c(NA))
    }

    # return any in-memory object ----
  } else{
    #check needed RAM
    if (!ignore.RAM){
      type.long <- prop@entity$type
      ebv_i_check_ram(prop@spatial$dimensions,timestep,type.long)
    } else{
      message('RAM capacities are ignored.')
    }

    #return in memory array ----
    h5array <- array(dim=c(prop@spatial$dimensions[1],prop@spatial$dimensions[2], length(timestep)))
    for (i in 1:length(timestep)){
      part <- rhdf5::h5read(filepath, datacubepath, start=c(1,1,timestep[i]),
                     count=c(prop@spatial$dimensions[2],prop@spatial$dimensions[1],1))
      #rotate matrix
      mat <- matrix(part, c(prop@spatial$dimensions[2], prop@spatial$dimensions[1]))
      mat <- t(mat[nrow(mat):1,,drop=FALSE])
      mat <- mat[,ncol(mat):1,drop=FALSE]
      mat <- replace(mat, which(base::match(mat, fillvalue)==1), c(NA))
      #fill array
      h5array[,,i] <- mat
    }

    if(delayed==FALSE & raster==TRUE){
      # return raster object ----
      if(length(timestep)==1){
        #matrix to raster
        h5array <-raster::raster(
          matrix(h5array, prop@spatial$dimensions[1], prop@spatial$dimensions[2]),
          xmn=prop@spatial$extent[1], xmx=prop@spatial$extent[2],
          ymn=prop@spatial$extent[3], ymx=prop@spatial$extent[4],
          crs=prop@spatial$srs
        )
      }else{
        h5array <-raster::brick(
          h5array,
          xmn=prop@spatial$extent[1], xmx=prop@spatial$extent[2],
          ymn=prop@spatial$extent[3], ymx=prop@spatial$extent[4],
          crs=prop@spatial$srs
        )
      }

      #set nodata value
      h5array <- raster::reclassify(h5array, cbind(fillvalue, NA))
    }

  }

  return(h5array)
}
