#' Read subset (shapefile) of the data from EBV NetCDF
#'
#' @description Read a subset of one or more layers from one datacube of the NetCDF file. Subset definition by a shapefile. This functions writes temporary files on your disk. Speficy a directory for these setting via options('temp_directory'='/path/to/temp/directory').

#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param shp Path to the shapefile defining the subset.
#' @param outputpath Defaul: NULL, returns the data as a raster object in
#'   memory. Optional: set path to write subset as GeoTiff on disk, returns
#'   outputpath.
#' @param timestep Choose one or several timesteps (vector).
#' @param at Default: TRUE, all pixels touched by the polygon(s) will be
#'   updated. Set to FALSE to only include pixels that are on the line render
#'   path or have center points inside the polygon(s).
#' @param overwrite Default: FALSE. Set to TRUE to overwrite the outputfile
#'   defined by 'outputpath'.
#' @param ignore.RAM Checks if there is enough space in your memory to read the
#'   data. Can be switched off (set to TRUE).
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return Returns a raster object if no outputpath is given. Otherwise the
#'   subset is written onto the disk and the ouputpath is returned.
#' @export
#' @seealso [ebvnetcdf::ebv_data_read_bb()] for more examples
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # shp <- 'path/to/subset.shp'
#' # out <- 'path/to/write/subset.tif'
#' # cSAR.germany <- ebv_data_read_bb(file, datacubes[1], shp)
#'
ebv_data_read_shp <- function(filepath, datacubepath, shp, outputpath=NULL, timestep = 1, at = TRUE, overwrite=FALSE, ignore.RAM=FALSE, verbose = FALSE){
  ####start initial checks ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  #ensure that all tempfiles are deleted on exit
  withr::defer(
    if(exists('tempshp')){
      unlink(tempshp, recursive = TRUE)
    }
  )
  withr::defer(
    if(exists('tempraster')){
      if(file.exists(tempraster)){
        file.remove(tempraster)
      }
    }
  )

  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }
  #are all arguments given?
  if(missing(shp)){
    stop('Shapefile (shp) argument for subsetting is missing.')
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

  #nc filepath check
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #shp filepath check
  if (checkmate::checkCharacter(shp) != TRUE){
    stop('Shapefilepath must be of type character.')
  }
  if (checkmate::checkFileExists(shp) != TRUE){
    stop(paste0('Shapefile does not exist.\n', shp))
  }
  if (!endsWith(shp, '.shp')){
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
    stop(paste0('Chosen timestep ', timestep, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
  }

  #outputpath check
  if (!is.null(outputpath)){
    if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
      stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
    }
    #check if outpufile exists if overwrite is disabled
    if(!overwrite){
      if(checkmate::checkPathForOutput(outputpath) != TRUE){
        stop('Output file already exists. Change name or enable overwrite.')
      }
    }
  }

  #get temp directory
  temp_path <- getOption('temp_directory')[[1]]
  if (is.null(temp_path)){
    stop('This function creates a temporary file. Please specify a temporary directory via options.')
  } else if (checkmate::checkDirectoryExists(temp_path) != TRUE){
    stop('The temporary directory given by you does not exist. Please change!\n', temp_path)
  }

  ####end initial checks ----

  #read shapefile
  subset <- rgdal::readOGR(shp, verbose=FALSE)

  #get_epsg of shp
  temp_epsg <- rgdal::showEPSG(sp::proj4string(subset))
  if(is.na(as.integer(temp_epsg))){
    stop(paste0('The given srs of the shapefile is not supported.\n', temp_epsg))
  } else {
    epsg.shp <- as.integer(temp_epsg)
  }

  #get epsg of ncdf
  epsg.nc <- prop@spatial$epsg

  #original extent
  extent.org <- raster::extent(subset)

  #reproject shp if necessary to epsg of ncdf
  if (epsg.shp != epsg.nc){
    subset <- sp::spTransform(subset, sp::CRS(paste0('EPSG:',epsg.nc)))
    tempshp <- file.path(temp_path, 'temp_EBV_shp_subset')
    if (dir.exists(tempshp)){
      unlink(tempshp, recursive = TRUE)
    }
    rgdal::writeOGR(subset, tempshp, layer = 'temp', driver='ESRI Shapefile')
    shp <- file.path(tempshp, 'temp.shp')
  }

  #get extent of shp
  extent.shp <- raster::extent(subset)

  #get extent of ncdf file
  ext <- prop@spatial$extent

  #get subset of ncdf #checks for RAM
  subset.nc <- ebv_data_read_bb(filepath, datacubepath, c(extent.shp@xmin, extent.shp@xmax, extent.shp@ymin, extent.shp@ymax), timestep=timestep, epsg=epsg.nc, ignore.RAM = ignore.RAM, verbose=verbose)

  #get extent of raster
  extent.raster <- raster::extent(subset.nc)

  #get resolution of ncdf
  resolution.nc <- raster::res(subset.nc)

  #rasterize shp - with resoultion of ncdf, burn value 1 (temp rasterlayer --> mask)
  #check ram
  dim.subset <- dim(subset.nc)
  #check needed RAM
  if (!ignore.RAM){
    ebv_i_check_ram(dim.subset, timestep, 'H5T_STD_I8BE') #type=placeholder for integer
  } else{
    message('RAM capacities are ignored.')
  }


  #define output
  tempraster <- file.path(temp_path, 'temp_EBV_shp_subset.tif')
  #remove file in case it exists, as gdal_rasterize has no overwrite option
  if(file.exists(tempraster)){
    file.remove(tempraster)
  }
  temp.raster <- gdalUtils::gdal_rasterize(shp, tempraster, at = at, burn = 1,
                                te = c(extent.raster@xmin, extent.raster@ymin,
                                       extent.raster@xmax, extent.raster@ymax),
                                tr = resolution.nc, ot ='Byte',
                                output_Raster = TRUE)

  #mask the subset
  #check needed RAM
  if (!ignore.RAM){
    ebv_i_check_ram(dim.subset, timestep, 'H5T_NATIVE_FLOAT') #type=placeholder for double
  } else{
    message('RAM capacities are ignored.')
  }
  subset.raster <- raster::mask(subset.nc, temp.raster, maskvalue=0, overwrite=TRUE)

  #set nodata value
  subset.raster <- raster::reclassify(subset.raster, cbind(prop@entity$fillvalue, NA))

  #remove temp.raster
  file.remove(tempraster)

  #remove temp shp
  if (epsg.shp != epsg.nc){
    unlink(tempshp, recursive = TRUE)
  }

  #return raster or tif
  if(!is.null(outputpath)){
    raster::writeRaster(subset.raster, outputpath, format = "GTiff", overwrite = overwrite)
    return(outputpath)
  } else {
    return(subset.raster)
  }

}
