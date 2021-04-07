#' Read data from EBV NetCDF
#'
#' @description Read one or more layers from one datacube of the NetCDF file. Decide between in-memory array, in-memory raster or an array-like object (DelayedMatrix) pointing to the on-disk NetCDF file. Latter is useful for data that exceeds your memory.
#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use [ebvnetcdf::ebv_datacubepaths()]).
#' @param timestep Choose one or several timesteps (vector).
#' @param delayed Default. Returns data as DelayedMatrix object. More timesteps are not returned as a 3D array but as a list of the DelayedMatrix (one matrix per band).
#' @param sparse Set to TRUE if the data contains a lot emtpy raster cells. Only relevant for DelayedMatrix. No further implementation by now.
#' @param raster Set to TRUE and 'delayed' to FALSE to get a raster. If both arguments are set to FALSE the function returns an array.
#' @param ignore.RAM Checks if there is enough space in your memory to read the data. Can be switched off (set to TRUE).
#'
#' @note For working with the DelayedMatrix take a look at the \href{https://www.rdocumentation.org/packages/HDF5Array/versions/1.0.2/topics/DelayedArray-utils}{DelayedArray-class}.
#'
#' @return Array, Raster or DelayedMatrix object containing the data of the corresponding datacube and timestep(s).
#' @export
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # cSAR.delayedarray <- ebv_data_read(file, datacubes[1,1], c(1,6))
#' # cSAR.raster <- ebv_data_read(file, datacubes[1,1], 1, delayed = F, raster = T)
#' # cSAR.array <- ebv_data_read(file, datacubes[1,1], c(1,1,3), delayed = F, raster = F)
ebv_data_read <- function(filepath, datacubepath, timestep, delayed=TRUE, sparse=TRUE, raster=FALSE, ignore.RAM = FALSE){
  ####initial tests start
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

  #filepath check
  if (!file.exists(filepath)){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath)

  #variable check
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    rhdf5::H5Fclose(hdf)
    stop(paste0('The given variable is not valid:\n', datacubepath))
  } else {
    rhdf5::H5Fclose(hdf)
  }

  #get properties
  prop <- ebv_properties(filepath, datacubepath)

  #timestep check
  #check if timestep is valid type
  if (class(timestep)=='numeric'){
    for (t in timestep){
      if (! as.integer(t)==t){
        stop('Timestep has to be an integer or a list of integers.')
      }
    }
  } else {
    stop('Timestep has to be of class numeric.')
  }

  #check timestep range
  for (t in timestep){
    max_time <- prop@spatial_information@dimensions[3]
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  #warning that raster output will be ignored
  if(delayed==TRUE & raster==TRUE){
    message('raster=TRUE will be ignored as delayed = TRUE.')
  }

  #######initial test end

  #get fillvalue
  fillvalue <- prop@entity_information@fillvalue

  if (delayed==TRUE){
    #return delayed array
    #get type
    type.long <- prop@entity_information@type
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

  } else{
    #check needed RAM
    if (!ignore.RAM){
      type.long <- prop@entity_information@type
      ebv_i_check_ram(prop@spatial_information@dimensions,timestep,type.long)
    } else{
      message('RAM capacities are ignored.')
    }

    #return in memory array
    h5array <- array(dim=c(prop@spatial_information@dimensions[1],prop@spatial_information@dimensions[2], length(timestep)))
    for (i in 1:length(timestep)){
      part <- rhdf5::h5read(filepath, datacubepath, start=c(1,1,timestep[i]),
                     count=c(prop@spatial_information@dimensions[2],prop@spatial_information@dimensions[1],1))
      #rotate matrix
      mat <- matrix(part, c(prop@spatial_information@dimensions[2], prop@spatial_information@dimensions[1]))
      mat <- t(mat[nrow(mat):1,,drop=FALSE])
      mat <- mat[,ncol(mat):1,drop=FALSE]
      mat <- replace(mat, which(base::match(mat, fillvalue)==1), c(NA))
      #fill array
      h5array[,,i] <- mat
    }

    if(delayed==FALSE & raster==TRUE){
      if(length(timestep)==1){
        #matrix to raster
        h5array <-raster::raster(
          matrix(h5array, prop@spatial_information@dimensions[1], prop@spatial_information@dimensions[2]),
          xmn=prop@spatial_information@extent[1], xmx=prop@spatial_information@extent[2],
          ymn=prop@spatial_information@extent[3], ymx=prop@spatial_information@extent[4],
          crs=prop@spatial_information@srs
        )
      }else{
        #array to raster
        h5array <-raster::brick(
          h5array,
          xmn=prop@spatial_information@extent[1], xmx=prop@spatial_information@extent[2],
          ymn=prop@spatial_information@extent[3], ymx=prop@spatial_information@extent[4],
          crs=prop@spatial_information@srs
        )
      }

      #set nodata value
      h5array <- raster::reclassify(h5array, cbind(fillvalue, NA))
    }

  }

  return(h5array)
}
