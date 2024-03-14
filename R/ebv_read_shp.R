#' Read subset (shapefile) of one datacube of an EBV netCDF
#'
#' @description Read a subset of one or more layers from one datacube of the
#'   netCDF file. Subset definition by a shapefile.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param shp Character. Path to the shapefile defining the subset. Ending needs
#'   to be *.shp.
#' @param outputpath Character. Default: NULL, returns the data as a SpatRaster
#'   object in memory. Optional: set path to write subset as GeoTiff on disk.
#' @param timestep Integer or character. Select one or several timestep(s).
#'   Either provide an integer value or list of values that refer(s) to the
#'   index of the timestep(s) (minimum value: 1) or provide a date or list of
#'   dates in ISO format, such as '2015-01-01'.
#' @param touches Logical. Default: TRUE, all pixels touched by the polygon(s)
#'   will be updated. Set to FALSE to only include pixels that are on the line
#'   render path or have center points inside the polygon(s).
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
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Returns a SpatRaster object if no output path is given. Otherwise the
#'   subset is written onto the disk and the ouput path is returned.
#' @export
#' @seealso [ebvcube::ebv_read_bb()] for subsetting via bounding box.
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#'
#' #set path to shp file - cameroon country borders
#' shp_path <- system.file(file.path("extdata","cameroon.shp"), package="ebvcube")
#'
#' \donttest{
#' #read subset - return SpatRaster
#' cSAR_cameroon <- ebv_read_shp(filepath = file, datacubepath = datacubes[1,1],
#'                              entity = 1, timestep = 1, shp = shp_path,
#'                              outputpath = NULL, ignore_RAM = TRUE)
#' }
ebv_read_shp <- function(filepath, datacubepath = NULL, entity = NULL,
                         timestep = 1, shp, outputpath=NULL, touches = TRUE,
                         scenario = NULL, metric = NULL,
                         overwrite=FALSE, ignore_RAM=FALSE, verbose = TRUE){
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
  if(missing(shp)){
    stop('Shapefile (shp) argument for subsetting is missing.')
  }

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=FALSE) != TRUE){
    stop('ignore_RAM must be of type logical.')
  }
  if(checkmate::checkLogical(overwrite, len=1, any.missing=FALSE) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(touches, len=1, any.missing=FALSE) != TRUE){
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
  prop <- ebv_properties(filepath, datacubepath, verbose = verbose)

  #timestep check -> in case of ISO, get index
  timestep <- ebv_i_date(timestep, prop@temporal$dates)

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
    epsg.shp <- temp_epsg
  }

  #get epsg of ncdf
  epsg.nc <- as.integer(prop@spatial$epsg)
  crs.nc <- prop@spatial$wkt2

  #reproject shp if necessary to epsg of ncdf ----
  if (epsg.shp != epsg.nc){
    subset <- terra::project(subset, crs.nc)
  }

  #get extent of shp
  extent.shp <- terra::ext(subset)

  #get subset of ncdf #checks for RAM
  subset.nc <- ebv_read_bb(filepath, datacubepath, entity=entity,
                           bb=c(extent.shp[1], extent.shp[2], extent.shp[3], extent.shp[4]),
                           timestep=timestep, epsg=epsg.nc, ignore_RAM = ignore_RAM, verbose=verbose)

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
