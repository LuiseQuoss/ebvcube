#' Get a simple explorative analysis of a EBV NetCDF
#'
#' @description Get basic measurements of the data, including min, max, mean,
#'   sd, n, #NAs, q25, q50, q75 (no mean for categorical data).
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param subset Optional if you want measurements on a smaller subset. Possible
#'   via the path to a shapefile or the indication of a bounding box defining
#'   the subset. Else the whole area is analysed.
#' @param timestep Choose one or several timesteps (vector).
#' @param at Optional. Only relevant if the subset is indicated by a shapefile.
#'   See [ebvnetcdf::ebv_data_read_shp()].
#' @param epsg Optional. Only relevant if the subset is indicated by a boudning
#'   box and the coordinate reference system differes from WGS84. See
#'   [ebvnetcdf::ebv_data_read_bb()].
#' @param numerical Default: TRUE. Change to FALSE if the data covered by the
#'   NetCDF contains categorical data.
#' @param na.rm Default: TRUE. NA values are removed in the analysis. Change to
#'   FALSE to include NAs.
#'
#' @return Returns a S4 object containing the basic measurements.
#' @export
#' @seealso [ebvnetcdf::ebv_data_read_bb()] and [ebvnetcdf::ebv_data_read_shp()]
#'   for the usage of subsets.
#'
#' @importFrom stats quantile
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # data.global.year <- ebv_data_analyse(file, datacubes[1,1], timestep=c(1:12))
#' # data.germany.jan <- ebv_data_analyse(file, datacubes[1,1], c(5,15,47,55), timestep=1)
ebv_data_analyse <- function(filepath, datacubepath, subset=NULL, timestep=1, at=TRUE, epsg = 4326, numerical=TRUE, na.rm=TRUE){
  ####initial tests start ----
  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }

  #filepath check - nc
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
    max_time <- prop@spatial$dimensions[3]
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  #more checks are included in subset bb and subset shp

  ####initial tests end ----

  #process shp subset
  if (is.null(subset)){
    #process whole file + variable+ timestep
    type.short <- ebv_i_type_r(prop@entity$type)
    all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath, as.sparse = T, type = type.short)
    subset.array <- all[,,timestep]
    #give fillvalue as nodata value
    subset.array <- replace(subset.array, subset.array==prop@entity$fillvalue[1], c(NA))
  }  else if(class(subset) == "numeric"){
    #process bb subset
    subset.raster <- ebv_data_read_bb(filepath, datacubepath, bb=subset, timestep=timestep, epsg=epsg)
    #raster to array
    subset.array <- raster::as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else if(endsWith(subset, '.shp')){
    #process shp subset
    subset.raster <- ebv_data_read_shp(filepath, datacubepath, shp=subset, timestep=timestep, at=at)
    #raster to array
    subset.array <- as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else {
    stop('Not processed. Check your given subset-indication.')
  }

  if(numerical){
    #numerical stats
    n <- length(subset.array)
    temp <- as.numeric(summary(array(subset.array)))
    sd <- sd(subset.array, na.rm=na.rm)
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], mean =temp[4], q75 =temp[5], max=temp[6], std =sd, n =n, NAs =temp[7])
  }else{
    #categorical stats
    n <- length(subset.array)
    temp <- as.numeric(stats::quantile(subset.array, na.rm=na.rm))
    NAs <- sum(is.na(subset.array))
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], q75 =temp[4], max=temp[5], n =n, NAs =NAs)
  }

  return(stats)

}
