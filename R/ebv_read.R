#' Read data from an EBV netCDF
#'
#' @description Read one or more layers from one datacube of the netCDF file.
#'   Decide between in-memory array, in-memory SpatRaster or an array-like
#'   object (DelayedMatrix) pointing to the on-disk netCDF file. The latter is
#'   useful for data that exceeds your memory.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param timestep Integer or character. Select one or several timestep(s).
#'   Either provide an integer value or list of values that refer(s) to the
#'   index of the timestep(s) (minimum value: 1) or provide a date or list of
#'   dates in ISO format, such as '2015-01-01'.
#' @param type Character. Choose between 'a', 'r' and 'da'. The first returns an
#'   array or matrix object. The 'r' indicates that a SpatRaster object from the
#'   terra package will be returned (default). The latter ('da') returns a
#'   DelayedArray or DelayedMatrix object.
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
#' @param sparse Logical. Default: FALSE. Set to TRUE if the data contains a lot
#'   empty raster cells. Only relevant for DelayedArray return value.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @note For working with the DelayedMatrix take a look at
#'   [DelayedArray::DelayedArray()] and the
#'   \href{https://www.rdocumentation.org/packages/HDF5Array/versions/1.0.2/topics/DelayedArray-utils}{DelayedArray-utils}.
#'
#'
#'
#' @return Array, SpatRaster or DelayedArray object containing the data of the
#'   corresponding datacube and timestep(s).
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#'
#' \donttest{
#' #read data as DelayedArray
#' cSAR.delayedarray <- ebv_read(filepath = file, datacubepath = datacubes[1,1],
#'                               entity = 1, timestep = c(1,3), type='da',
#'                               sparse = TRUE)
#' #read data as SpatRaster
#' cSAR.raster <- ebv_read(filepath = file,entity = 1, timestep = "2000-01-01",
#'                         type='r', metric = 1)
#' #read data as Array
#' cSAR.array <- ebv_read(filepath = file, datacubepath = datacubes[1,1],
#'                               entity = 1, timestep = 1, type='r')
#' }
ebv_read <- function(filepath, datacubepath = NULL,  entity=NULL, timestep=1,
                     type='r', scenario = NULL, metric = NULL,
                     sparse=FALSE, ignore_RAM = FALSE, verbose = FALSE){
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

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(sparse, len=1, any.missing=FALSE) != TRUE){
    stop('sparse must be of type logical.')
  }
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=FALSE) != TRUE){
    stop('ignore_RAM must be of type logical.')
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

  # type test
  if (checkmate::checkCharacter(type) != TRUE){
    stop('Type must be of type character.')
  }
  if (! type %in% c('da', 'r', 'a')){
    stop('Type must be "da", "r" or "a". Check help page for more information.')
  }

  #file closed?
  # ebv_i_file_opened(filepath, verbose)

  #datacubepath check
  #1. make sure anything is defined
  if(is.null(datacubepath) && is.null(scenario) && is.null(metric)){
    stop('You need to define the datacubepath or the scenario and metric.
       Regarding the second option: If your EBV netCDF has no scenario,
       leave the argument empty.')
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
    #close file
    rhdf5::H5Fclose(hdf)
  } else if(!is.null(metric)){
    #3. check metric&scenario
    datacubepaths <- ebv_datacubepaths(filepath, verbose=verbose)
    datacubepath <- ebv_i_datacubepath(scenario, metric,
                                       datacubepaths, verbose=verbose)
  }

  #get properties
  prop <- ebv_properties(filepath, datacubepath, verbose=verbose)

  #check file structure
  is_4D <- ebv_i_4D(filepath)
  if(is_4D){
    if(is.null(entity)){
      stop('Your working with a 4D cube based EBV netCDF. Please specify the entity-argument.')
    }
    #check entity
    entity_names <- prop@general$entity_names
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

  #timestep check -> in case of ISO, get index
  timestep <- ebv_i_date(timestep, prop@temporal$dates)

  #######initial test end ----

  #get fillvalue
  fillvalue <- prop@ebv_cube$fillvalue

  if (type=='da'){
    #return delayed array ----
    #get type
    type.long <- prop@ebv_cube$type
    #get numeric type
    type.short <- ebv_i_type_r(type.long)

    #read as H5Array
    all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath, as.sparse = sparse, type = type.short)

    #rotate data
    if(is_4D){
      # 4D structure
      if (length(timestep)>1){
        #select entity cube
        entity_part <- all[,,timestep,entity_index]
        #rotate matrix
        h5array <- DelayedArray::aperm(DelayedArray::aperm(entity_part, 3:1), c(2,3,1))
        #replace NA value
        h5array <- replace(h5array, h5array==fillvalue, c(NA))
      }
      else{
        #select slice
        h5array <- all[,,timestep,entity_index]
        #rotate matrix
        h5array <- t(h5array[,,drop=FALSE])
        #replace NA value
        h5array <- replace(h5array, h5array==fillvalue, c(NA))
      }
    } else{
      # 3D strucuture
      if (length(timestep)>1){
        #rotate matrix
        h5array <- DelayedArray::aperm(DelayedArray::aperm(all, 3:1), c(2,3,1))
        #replace NA value
        h5array <- replace(h5array, h5array==fillvalue, c(NA))
      }
      else{
        h5array <- all[,,timestep]
        #rotate matrix
        h5array <- t(h5array[,,drop=FALSE])
        #replace NA value
        h5array <- replace(h5array, h5array==fillvalue, c(NA))
      }

    }

    # return any in-memory object ----
  } else if (type=='a' || type=='r') {
    #check needed RAM
    if (!ignore_RAM){
      type.long <- prop@ebv_cube$type
      ebv_i_check_ram(prop@spatial$dimensions,timestep,entity,type.long)
    } else{
      if(verbose){
        print('RAM capacities are ignored.')
      }
    }

    #return in memory array ----
    h5array <- array(dim=c(prop@spatial$dimensions[1],prop@spatial$dimensions[2], length(timestep)))
    for (i in 1:length(timestep)){
      if(is_4D){
        part <- rhdf5::h5read(filepath, datacubepath, start=c(1,1,timestep[i], entity_index),
                              count=c(prop@spatial$dimensions[2],prop@spatial$dimensions[1],1,1))
      }else{
        part <- rhdf5::h5read(filepath, datacubepath, start=c(1,1,timestep[i]),
                              count=c(prop@spatial$dimensions[2],prop@spatial$dimensions[1],1))

      }

      #rotate matrix
      mat <- matrix(part, c(prop@spatial$dimensions[2], prop@spatial$dimensions[1]))
      mat <- t(mat[,,drop=FALSE])
      mat <- replace(mat, which(base::match(mat, fillvalue)==1), c(NA))
      #fill array
      h5array[,,i] <- mat

    }

    if(type=='r'){
      # return SpatRaster object ----

        extent <- terra::ext(c(prop@spatial$extent[1], prop@spatial$extent[2],
                    prop@spatial$extent[3], prop@spatial$extent[4]))
        h5array <- terra::rast(h5array, crs=prop@spatial$wkt2, extent=extent)


      #set nodata value
        h5array <- terra::classify(h5array, cbind(fillvalue, NA))
    }

  } else {
    stop('Something went terribly wrong. Check your type argument.')
  }

  return(h5array)
}
