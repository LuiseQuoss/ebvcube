#' Read subset (shapefile) of one datacube of an EBV netCDF
#'
#' @description Read a subset of one or more layers from one datacube of the
#'   netCDF file. Subset definition by a shapefile.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param shp Character. Path to the shapefile defining the subset. Ending needs
#'   to be *.shp.
#' @param outputpath Character. Default: NULL, returns the data as a raster
#'   object in memory. Optional: set path to write subset as GeoTiff on disk.
#' @param timestep Integer. Choose one or several timesteps (vector).
#' @param touches Logical. Default: TRUE, all pixels touched by the polygon(s) will
#'   be updated. Set to FALSE to only include pixels that are on the line render
#'   path or have center points inside the polygon(s).
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Returns a raster object if no outputpath is given. Otherwise the
#'   subset is written onto the disk and the ouputpath is returned.
#' @export
#' @seealso [ebvcube::ebv_read_bb()] for subsetting via bounding box.
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #set path to shp file
#' shp_path <- system.file(file.path("extdata","subset_germany.shp"), package="ebvcube")
#'
#' #read subset - return Raster
#' # cSAR.germany <- ebv_read_shp(filepath = file, datacubepath = datacubes[1],
#' #                              entity = NULL, timestep = 1, shp = shp_path,
#' #                              outputpath = NULL)
ebv_read_shp <- function(filepath, datacubepath, entity=NULL, timestep = 1,
                         shp, outputpath=NULL, touches = TRUE, overwrite=FALSE,
                         ignore_RAM=FALSE, verbose = FALSE){
  ####start initial checks ----
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
  #are all arguments given?
  if(missing(shp)){
    stop('Shapefile (shp) argument for subsetting is missing.')
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
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(touches, len=1, any.missing=F) != TRUE){
    stop('touches must be of type logical.')
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

  #outputpath check
  if (!is.null(outputpath)){
    if (checkmate::checkCharacter(outputpath) != TRUE){
      stop('Outputpath must be of type character.')
    }
    if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
      stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
    }
    if(!endsWith(outputpath, '.tif')){
      stop('Outputpath needs to end with *.tif. Other datatypes are not yet implemented.')
    }
    #check if outpufile exists if overwrite is disabled
    if(!overwrite){
      if(checkmate::checkPathForOutput(outputpath) != TRUE){
        stop('Output file already exists. Change name or enable overwrite.')
      }
    }
  }

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

  ####end initial checks ----

  #read shapefile
  subset <- terra::vect(shp)

  #get_epsg of shp ----
  temp_epsg <- terra::crs(subset)
  if(temp_epsg==''){
    stop('The crs of the shapefile could not be detected. Did you assign a CRS to your shapefile? (Or are you incorrectly connected to GDAL and PROJ LIB?)')
  }
  temp_epsg <- ebv_i_get_epsg(temp_epsg)

  #check if empty -> error
  if(is.na(as.integer(temp_epsg))){
    stop(paste0('The crs of the shapefile could not be processed. Did you assign a CRS to your shapefile? Or are you incorrectly connected to GDAL and PROJ LIB?'))
  } else {
    #if not empty: check if valid
    ebv_i_eval_epsg(temp_epsg)
  }

  #get epsg of ncdf
  epsg.nc <- as.integer(prop@spatial$epsg)
  crs.nc <- prop@spatial$wkt2

  #original extent
  extent.org <- terra::ext(subset)

  #reproject shp if necessary to epsg of ncdf ----
  if (epsg.shp != epsg.nc){
    subset <- terra::project(subset, crs.nc)
  }

  #get extent of shp
  extent.shp <- terra::ext(subset)

  #get extent of ncdf file
  ext <- prop@spatial$extent

  #get subset of ncdf #checks for RAM
  subset.nc <- ebv_read_bb(filepath, datacubepath, entity=entity,
                           bb=c(extent.shp[1], extent.shp[2], extent.shp[3], extent.shp[4]),
                           timestep=timestep, epsg=epsg.nc, ignore_RAM = ignore_RAM, verbose=verbose)

  #get extent of raster
  extent.raster <- terra::ext(subset.nc)

  #get resolution of ncdf
  resolution.nc <- terra::res(subset.nc)

  #rasterize shp ----
  #with resoultion of ncdf, burn value 1 (temp.raster --> mask)
  #rasterize shapefile and align to netCDF resolution
  temp.raster <- terra::rasterize(subset, subset.nc, touches=touches)

  #mask the subset ----
  subset.raster <- terra::mask(subset.nc, temp.raster)

  #set nodata value
  subset.raster <- terra::classify(subset.raster, cbind(prop@ebv_cube$fillvalue, NA))

  #return raster or tif
  if(!is.null(outputpath)){
    terra::writeRaster(subset.raster, outputpath,overwrite = overwrite, filetype = "GTiff")
    return(outputpath)
  } else {
    return(subset.raster)
  }

}
