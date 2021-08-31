#' Get a simple explorative analysis of an EBV NetCDF datacube
#'
#' @description Get basic measurements of the data, including min, max, mean,
#'   sd, n, #NAs, q25, q50, q75 (no mean for categorical data).
#' @param filepath Character. Path to the NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param subset Optional if you want measurements on a smaller subset. Possible
#'   via the path to a shapefile (character) or the indication of a bounding box
#'   (vector of four numeric values) defining the subset. Else the whole area is
#'   analysed.
#' @param timestep Integer. Choose one or several timesteps (vector).
#' @param at Logical. Optional. Default: TRUE. Only relevant if the subset is
#'   indicated by a shapefile. See [ebvnetcdf::ebv_read_shp()].
#' @param epsg Numeric. Optional. Only relevant if the subset is indicated by a
#'   bounding box and the coordinate reference system differs from WGS84. See
#'   [ebvnetcdf::ebv_read_bb()].
#' @param numerical Logical. Default: TRUE. Change to FALSE if the data covered
#'   by the NetCDF contains categorical data.
#' @param na_rm Logical. Default: TRUE. NA values are removed in the analysis.
#'   Change to FALSE to include NAs.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Returns a named list containing the measurements.
#' @export
#' @seealso [ebvnetcdf::ebv_read_bb()] and [ebvnetcdf::ebv_read_shp()]
#'   for the usage of subsets.
#'
#' @importFrom stats quantile
#' @examples
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' data_global_year <- ebv_analyse(file, datacubes[1,1], timestep=c(1:12))
#' #data_germany_1900 <- ebv_analyse(file, datacubes[1,1], c(5,15,47,55), timestep=1)
ebv_analyse <- function(filepath, datacubepath, subset=NULL, timestep=1,
                             at=TRUE, epsg = 4326, numerical=TRUE, na_rm=TRUE, verbose=FALSE){
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

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #filepath check - nc
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
  hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
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

  #check logical arguments
  if(checkmate::checkLogical(na_rm, len=1, any.missing=F) != TRUE){
    stop('na_rm must be of type logical.')
  }
  if(checkmate::checkLogical(numerical, len=1, any.missing=F) != TRUE){
    stop('numerical must be of type logical.')
  }
  if(checkmate::checkLogical(at, len=1, any.missing=F) != TRUE){
    stop('at must be of type logical.')
  }

  #more checks are included in subset bb and subset shp

  ####initial tests end ----

  #process global scale ----
  if (is.null(subset)){
    #process whole file + variable+ timestep
    type.short <- ebv_i_type_r(prop@ebv_cube$type)
    all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath, as.sparse = T, type = type.short)
    subset.array <- all[,,timestep]
    #give fillvalue as nodata value
    subset.array <- replace(subset.array, subset.array==prop@ebv_cube$fillvalue[1], c(NA))
  }  else if(class(subset) == "numeric"){
    #process bb subset ----
    subset.raster <- ebv_read_bb(filepath, datacubepath, bb=subset, timestep=timestep, epsg=epsg, verbose=verbose)
    #raster to array
    subset.array <- raster::as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else if(endsWith(subset, '.shp')){
    #process shp subset ----
    subset.raster <- ebv_read_shp(filepath, datacubepath, shp=subset, timestep=timestep, at=at, verbose=verbose)
    #raster to array
    subset.array <- raster::as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else {
    stop('Not processed. Check your given subset-indication.')
  }

  if(numerical){
    #numerical stats ----
    n <- length(subset.array)
    temp <- as.numeric(summary(array(subset.array)))
    sd <- sd(subset.array, na.rm=na_rm)
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], mean =temp[4], q75 =temp[5], max=temp[6], std =sd, n =n, NAs =temp[7])
  }else{
    #categorical stats ----
    n <- length(subset.array)
    temp <- as.numeric(stats::quantile(subset.array, na.rm=na_rm))
    NAs <- sum(is.na(subset.array))
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], q75 =temp[4], max=temp[5], n =n, NAs =NAs)
  }

  return(stats)

}
