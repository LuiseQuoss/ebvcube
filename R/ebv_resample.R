#' Change the resolution of the data of an EBV netCDF
#'
#' @description Change the resolution of one datacube of a EBV netCDF based on
#'   another EBV netCDF or a given resolution.
#'
#' @param filepath_src Character. Path to the netCDF file whose resolution
#'   should be changed.
#' @param datacubepath_src Character. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]) whose resolution should be changed.
#' @param entity_src Character or Integer. Default is NULL. If the structure is
#'   3D, the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param resolution Either the path to an EBV netCDF file that determines the
#'   resolution (character) or the resolution defined directly (numeric). The
#'   vector defining the resolution directly must contain three elements: the
#'   x-resolution, the y-resolution and the corresponding EPSG code, e.g.
#'   c(0.25, 0.25, 4326).
#' @param outputpath Character. Set path to write data as GeoTiff on disk.
#' @param timestep_src Integer. Choose one or several timesteps (vector).
#' @param method Character. Default: bilinear. Define resampling method. Choose
#'   from: "near","bilinear","cubic" and "cubicspline". For categorical data,
#'   use 'near'. Based on [terra::project()].
#' @param return_raster Logical. Default: FALSE. Set to TRUE to directly get the
#'   corresponding SpatRast object.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   output file defined by 'outputpath'.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Default: returns the outputpath of the GeoTiff with the new
#'   resolution. Optional: return the raster object with the new resolution.
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #define different resolutions
#' res1 <- system.file(file.path("extdata","rodinini_001.nc"), package="ebvcube")
#' res2 <- c(1,1,4326)
#' #define output path
#' out <- file.path(system.file(package='ebvcube'),"extdata","changeRes.tif")
#'
#' \donttest{
#' #resample using a netCDF file - return GeoTiff
#' ebv_resample(filepath_src = file, datacubepath_src = datacubes[1,1],
#'              entity_src=1, timestep_src = 1, resolution = res1,
#'              outputpath = out)
#'
#' #resample defining the resolution and EPSG code by hand - return Raster
#' data_raster <- ebv_resample(filepath_src = file, datacubepath_src = datacubes[1,1],
#'                             entity_src=NULL, timestep_src = 1, resolution = res1,
#'                             outputpath = out, method='near', return_raster=TRUE)
#' }
ebv_resample <- function(filepath_src, datacubepath_src, entity_src=NULL, timestep_src = 1,
                         resolution, outputpath, method='bilinear', return_raster=FALSE,
                         overwrite = FALSE, ignore_RAM=FALSE, verbose=FALSE){
  ####initial tests start ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  #are all arguments given?
  if(missing(filepath_src)){
    stop('Filepath_src argument is missing.')
  }
  if(missing(datacubepath_src)){
    stop('Datacubepath_src argument is missing.')
  }
  if(missing(resolution)){
    stop('Resolution argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
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
  if(checkmate::checkLogical(return_raster, len=1, any.missing=F) != TRUE){
    stop('return_raster must be of type logical.')
  }
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=F) != TRUE){
    stop('ignore_RAM must be of type logical.')
  }

  #filepath src check
  if (checkmate::checkCharacter(filepath_src) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath_src) != TRUE){
    stop(paste0('File does not exist.\n', filepath_src))
  }
  if (!endsWith(filepath_src, '.nc')){
    stop(paste0('File ending of filepath_src is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath_src)

  #check if res is given or filepath to destination
  if(checkmate::checkNumeric(resolution)==TRUE){
    if(checkmate::checkNumeric(resolution, len=3)==TRUE){
      epsg_dest <- resolution[3]
      res <- resolution[1:2]
      filepath_dest <- NULL

      #check epsg code
      wkt_dest <- ebv_i_eval_epsg(epsg_dest)

    } else {
      stop('Resolution must be a vector of length 3 containing numerics.')
    }
  } else {
    filepath_dest <- resolution
  }

  if(!is.null(filepath_dest)){
    #filepath dest check
    if (checkmate::checkCharacter(filepath_dest) != TRUE){
      stop('Filepath_dest must be of type character.')
    }
    if (checkmate::checkFileExists(filepath_dest) != TRUE){
      stop(paste0('Filepath_dest does not exist.\n', filepath_dest))
    }
    if (!endsWith(filepath_dest, '.nc')){
      stop(paste0('File ending of filepath_dest is wrong. File cannot be processed.'))
    }

    #file closed
    ebv_i_file_opened(filepath_dest)

    #get properties
    prop_dest <- ebv_properties(filepath_dest, verbose=verbose)

    #get target resolution
    res <- prop_dest@spatial$resolution
    res <- c(res[1], res[2]) #c(res[1]*-1, res[2])

    #get target epsg
    epsg_dest <- prop_dest@spatial$epsg

  }

  #source variable check
  if (checkmate::checkCharacter(datacubepath_src) != TRUE){
    stop('Datacubepath must be of type character.')
  }
  hdf <- rhdf5::H5Fopen(filepath_src, flags = "H5F_ACC_RDONLY")
  if (rhdf5::H5Lexists(hdf, datacubepath_src)==FALSE | !stringr::str_detect(datacubepath_src, 'ebv_cube')){
    stop(paste0('The given variable is not valid:\n', datacubepath_src))
  }
  rhdf5::H5Fclose(hdf)

  #get properties source
  prop_src <- ebv_properties(filepath_src, datacubepath_src, verbose)
  type.long <- prop_src@ebv_cube$type
  entity_names <- prop_src@general$entity_names
  extent_src <- prop_src@spatial$extent

  #check file structure
  is_4D <- ebv_i_4D(filepath_src)
  if(is_4D){
    if(is.null(entity_src)){
      stop('Your working with a 4D cube based EBV netCDF. Please specify the entity_src-argument.')
    }
    #check entity_src ----
    ebv_i_entity(entity_src, entity_names)

    #get entity index
    if(checkmate::checkIntegerish(entity_src, len=1) == TRUE){
      entity_index <- entity_src
    } else if (checkmate::checkCharacter(entity_src)==TRUE){
      entity_index <- which(entity_names==entity_src)
    } else{
      entity_src <- 1 #set entity to 1 (for ebv_i_check_ram)
    }
  }

  #source timestep check
  #check if timestep is valid type
  if(checkmate::checkIntegerish(timestep_src) != TRUE){
    stop('timestep_src has to be an integer or a list of integers.')
  }

  #check timestep_src range
  max_time <- prop_src@spatial$dimensions[3]
  min_time <- 1
  if(checkmate::checkIntegerish(timestep_src, lower=min_time, upper=max_time) != TRUE){
    stop(paste0('Chosen timestep_src ', paste(timestep_src, collapse = ' '), ' is out of bounds. timestep_src range is ', min_time, ' to ', max_time, '.'))
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
    #check if outputfile exists if overwrite is disabled
    if(!overwrite){
      if(checkmate::checkPathForOutput(outputpath) != TRUE){
        stop('Output file already exists. Change name or enable overwrite.')
      }
    }
  }

  #check if method is valid
  if (checkmate::checkCharacter(method) != TRUE){
    stop('Method must be of type character.')
  }
  methods <- c("near","bilinear","cubic","cubicspline")
  if (! method %in% methods){
    stop('Given method is not valid.\n', method)
  }

  #check ram, if raster should be returned
  if (return_raster){
    #check needed RAM
    if (!ignore_RAM){
      ebv_i_check_ram(res,timestep_src,entity_src, type.long)
    } else{
      message('RAM capacities are ignored.')
    }
  }

  #get epsg
  epsg_src <- prop_src@spatial$epsg

  #######initial test end ----

  #srs defintion from epsg
  srs_src <- paste0('EPSG:',epsg_src)
  srs_dest <- paste0('EPSG:',epsg_dest)

  #get output type
  type_ot <- ebv_i_type_ot(type.long)
  type_terra <- ebv_i_type_terra(type_ot)

  #get data ----
  #open netCDF with terra
  data_raw <- terra::rast(filepath_src, subds = paste0('/', datacubepath_src))

  #get the index depending on the amount of entities and timesteps
  terra_index <- (entity_index-1)*max_time + timestep_src
  data_ts <- data_raw[[terra_index]]

  #create dummy terra SpatRast for projection
  dummy <- terra::rast()
  terra::crs(dummy) <- paste0('EPSG:', epsg_dest)
  #set extent
  if(!is.null(filepath_dest)){
    #BASED ON EBV NETCDF FILE
    extent <- terra::ext(terra::rast(filepath_dest))
    terra::ext(dummy) <- extent
  } else{
    #BASED ON RES and CRS
    #if epsg differ, transform src_ext
    #else just assign src extent
    if(epsg_src != epsg_dest){
      dummy2 <- terra::rast()
      terra::crs(dummy2) <- paste0('EPSG:', epsg_src)
      terra::ext(dummy2) <-  extent_src
      dummy_proj <- terra::project(dummy2, wkt_dest)
      extent_src <-terra::ext(dummy_proj)
    }
    terra::ext(dummy) <- extent_src
  }
  #set resolution
  terra::res(dummy) <- res

  #align to origin of the destination file
  data_proj <- tryCatch(
    {
      data_proj <- terra::project(data_ts, y = dummy, align=T, method=method, gdal=T)
    },
    error=function(e){
      # if (!stringr::str_detect(e, 'cannot create dataset from source')){
      #   stop(e)
      # }
      message(paste0('Slower algorithm needs to be used. Please be patient.'))
      data_proj <- terra::project(data_ts, y = dummy, align=T, method=method, gdal=F)
    }
  )


  #write data to file
  terra::writeRaster(data_proj, outputpath, filetype = "GTiff", overwrite = overwrite,
                     datatype=type_terra)

  #return array ----
  if (return_raster){
    return(data_proj)
  } else{
    return(outputpath)
  }
}

