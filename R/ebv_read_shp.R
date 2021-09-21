#' Read subset (shapefile) of one datacube of an EBV netCDF
#'
#' @description Read a subset of one or more layers from one datacube of the
#'   netCDF file. Subset definition by a shapefile. This functions writes
#'   temporary files on your disk. Specify a directory for these setting via
#'   options('ebv_temp'='/path/to/temp/directory').
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param shp Character. Path to the shapefile defining the subset. Ending needs
#'   to be *.shp.
#' @param outputpath Character. Default: NULL, returns the data as a raster
#'   object in memory. Optional: set path to write subset as GeoTiff on disk.
#' @param timestep Integer. Choose one or several timesteps (vector).
#' @param at Logical. Default: TRUE, all pixels touched by the polygon(s) will
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
#' @seealso [ebvnetcdf::ebv_read_bb()] for subsetting via bounding box.
#'
#' @examples
#' #define temp directory
#' options('ebv_temp'=system.file("extdata/", package="ebvnetcdf"))
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#' #set path to shp file
#' shp_path <- system.file(file.path("extdata","subset_germany.shp"), package="ebvnetcdf")
#'
#' #read subset - return Raster
#' # cSAR.germany <- ebv_read_shp(filepath = file, datacubepath = datacubes[1],
#' #                              entity = NULL, timestep = 1, shp = shp_path,
#' #                              outputpath = NULL)
ebv_read_shp <- function(filepath, datacubepath, entity=NULL, timestep = 1,
                         shp, outputpath=NULL, at = TRUE, overwrite=FALSE,
                         ignore_RAM=FALSE, verbose = FALSE){
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
  if(checkmate::checkLogical(at, len=1, any.missing=F) != TRUE){
    stop('at must be of type logical.')
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

  #get temp directory
  temp_path <- getOption('ebv_temp')[[1]]
  if (is.null(temp_path)){
    stop('This function creates a temporary file. Please specify a temporary directory via options.')
  } else {
    if (checkmate::checkCharacter(temp_path) != TRUE){
      stop('The temporary directory must be of type character.')
    }
    if (checkmate::checkDirectoryExists(temp_path) != TRUE){
      stop('The temporary directory given by you does not exist. Please change!\n', temp_path)
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
  subset <- rgdal::readOGR(shp, verbose=FALSE)

  #get_epsg of shp ----
  temp_epsg <- gdalUtils::gdalsrsinfo(shp)
  temp_epsg <- rgdal::showEPSG(paste0(temp_epsg[5:length(temp_epsg)],collapse=' '))

  if(is.na(as.integer(temp_epsg))){
    stop(paste0('The given crs of the shapefile is not supported.\n',
                'See error from gdalUtils:\n',
                as.character(paste0(temp_epsg, collapse = '\n'))))
  } else {
    epsg.shp <- as.integer(temp_epsg)
  }

  #get epsg of ncdf
  epsg.nc <- as.integer(prop@spatial$epsg)
  crs <- gdalUtils::gdalsrsinfo(paste0("EPSG:", epsg.nc))
  crs.nc <- sp::CRS(stringr::str_remove(paste(crs[2], collapse = ' '), 'PROJ.4 : '))

  #original extent
  extent.org <- raster::extent(subset)

  #reproject shp if necessary to epsg of ncdf ----
  if (epsg.shp != epsg.nc){
    subset <- sp::spTransform(subset, crs.nc)
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
  subset.nc <- ebv_read_bb(filepath, datacubepath, entity=entity,
                           bb=c(extent.shp@xmin, extent.shp@xmax, extent.shp@ymin, extent.shp@ymax),
                           timestep=timestep, epsg=epsg.nc, ignore_RAM = ignore_RAM, verbose=verbose)

  #get extent of raster
  extent.raster <- raster::extent(subset.nc)

  #get resolution of ncdf
  resolution.nc <- raster::res(subset.nc)

  #rasterize shp ----
  #with resoultion of ncdf, burn value 1 (temp rasterlayer --> mask)
  #check ram
  dim.subset <- dim(subset.nc)
  #check needed RAM
  if (!ignore_RAM){
    ebv_i_check_ram(dim.subset, timestep, entity, 'H5T_STD_I8BE') #type=placeholder for integer
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
                                co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                                output_Raster = TRUE)

  #mask the subset ----
  #check needed RAM
  if (!ignore_RAM){
    ebv_i_check_ram(dim.subset, timestep, entity, 'H5T_NATIVE_FLOAT') #type=placeholder for double
  } else{
    message('RAM capacities are ignored.')
  }
  subset.raster <- raster::mask(subset.nc, temp.raster, maskvalue=0, overwrite=TRUE)

  #set nodata value
  subset.raster <- raster::reclassify(subset.raster, cbind(prop@ebv_cube$fillvalue, NA))
  raster::crs(subset.raster) <- crs.nc

  #remove temp shp
  if (epsg.shp != epsg.nc){
    unlink(tempshp, recursive = TRUE)
  }

  #return raster or tif
  if(!is.null(outputpath)){
    raster::writeRaster(subset.raster, outputpath, format = "GTiff",
                        overwrite = overwrite)
    return(outputpath)
  } else {
    return(subset.raster)
  }

}
