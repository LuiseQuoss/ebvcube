#' Change the resolution of the data of an EBV netCDF
#'
#' @description Change the resolution of one datacube of a EBV netCDF based on
#'   another EBV netCDF or a given resolution.
#'
#' @param filepath_src Character. Path to the netCDF file whose resolution
#'   should be changed.
#' @param datacubepath_src Character. Optional. Default: NULL. Path to the
#'   datacube (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use
#'   the scenario and metric argument to define which cube you want to access.
#' @param entity_src Character or Integer. Default is NULL. If the structure is
#'   3D, the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param resolution Character or Numeric. Either the path to an EBV netCDF file
#'   that determines the resolution (character) or the resolution defined
#'   directly (numeric). The vector defining the resolution directly must
#'   contain three elements: the x-resolution, the y-resolution and the
#'   corresponding EPSG code, e.g. c(0.25, 0.25, 4326).
#' @param outputpath Character. Set path to write data as GeoTiff on disk.
#' @param timestep_src Integer or character. Select one or several timestep(s).
#'   Either provide an integer value or list of values that refer(s) to the
#'   index of the timestep(s) (minimum value: 1) or provide a date or list of
#'   dates in ISO format, such as '2015-01-01'.
#' @param method Character. Default: bilinear. Define resampling method. Choose
#'   from: "near","bilinear","cubic", "cubicspline", "lanczos", "sum", "min",
#'   "q1", "med", "q3", "max", "average", "mode" and "rms". For categorical
#'   data, use 'near'. Based on [terra::project()].
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
#' @param return_raster Logical. Default: FALSE. Set to TRUE to directly get the
#'   corresponding SpatRaster object.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the output
#'   file defined by 'outputpath'.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Default: returns the output path of the GeoTiff with the new
#'   resolution. Optional: return the SpatRaster object with the new resolution.
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"),
#'                     package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#'
#' #define different resolutions
#' res1 <- system.file(file.path("extdata",
#'         "baisero_spepop_id5_20220405_v1_empty.nc"), package="ebvcube")
#' res2 <- c(0.5,0.5,4326)
#' #define output path
#' out <- file.path(system.file(package='ebvcube'),"extdata","changeRes.tif")
#'
#' \dontrun{
#' #resample defining the resolution and EPSG code by hand - return SpatRaster
#' data_raster <- ebv_resample(filepath_src = file, datacubepath_src = datacubes[1,1],
#'                             entity_src=1, timestep_src = 1, resolution = res2,
#'                             outputpath = out, method='near', return_raster=TRUE,
#'                             overwrite=TRUE)
#' #resample using a netCDF file - return GeoTiff
#' ebv_resample(filepath_src = file, datacubepath_src = datacubes[1,1],
#'              entity_src=1, timestep_src = 1, resolution = res1,
#'              outputpath = out, overwrite=TRUE)
#'
#' }
ebv_resample <- function(filepath_src, datacubepath_src = NULL, entity_src=NULL,
                         timestep_src = 1, resolution, outputpath, method='bilinear',
                         scenario = NULL, metric = NULL, return_raster=FALSE,
                         overwrite = FALSE, ignore_RAM=FALSE, verbose=TRUE){
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
  if(missing(resolution)){
    stop('Resolution argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(return_raster, len=1, any.missing=FALSE) != TRUE){
    stop('return_raster must be of type logical.')
  }
  if(checkmate::checkLogical(overwrite, len=1, any.missing=FALSE) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=FALSE) != TRUE){
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
      stop('Resolution must be of type integer or character.')
    }
    if (checkmate::checkFileExists(filepath_dest) != TRUE){
      stop(paste0('The file for the resolution does not exist.\n', filepath_dest))
    }
    if (!endsWith(filepath_dest, '.nc')){
      stop(paste0('File ending of the resolution-file is wrong - must be .nc'))
    }

    #get properties
    prop_dest <- ebv_properties(filepath_dest, verbose=verbose)

    #get target resolution
    res <- prop_dest@spatial$resolution
    res <- c(res[1], res[2]) #c(res[1]*-1, res[2])

    #get target epsg
    epsg_dest <- prop_dest@spatial$epsg

  }

  #source datacubepath check
  #1. make sure anything is defined
  if(is.null(datacubepath_src) && is.null(scenario) && is.null(metric)){
    stop('You need to define the datacubepath_src or the scenario and metric.
       Regarding the second option: If your EBV netCDF has no scenario,
       leave the argument empty.')
  }else if(!is.null(datacubepath_src)){
    #2. check datacubepath_src
    # open file
    hdf <- rhdf5::H5Fopen(filepath_src, flags = "H5F_ACC_RDONLY")
    if (checkmate::checkCharacter(datacubepath_src) != TRUE) {
      stop('Datacubepath must be of type character.')
    }
    if (rhdf5::H5Lexists(hdf, datacubepath_src) == FALSE ||
        !stringr::str_detect(datacubepath_src, 'ebv_cube')) {
      stop(paste0('The given datacubepath_src is not valid:\n', datacubepath_src))
    }
    #close file
    rhdf5::H5Fclose(hdf)
  } else if(!is.null(metric)){
    #3. check metric&scenario
    datacubepaths <- ebv_datacubepaths(filepath_src, verbose=verbose)
    datacubepath_src <- ebv_i_datacubepath(scenario, metric,
                                       datacubepaths, verbose=verbose)
  }

  #get properties source
  prop_src <- ebv_properties(filepath_src, datacubepath_src,  verbose=verbose)
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

  #timestep check -> in case of ISO, get index
  timestep_src <- ebv_i_date(timestep_src, prop_src@temporal$dates)

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
  methods <- c("near","bilinear","cubic", "cubicspline", "lanczos", "sum", "min","q1", "med", "q3", "max", "average", "mode", "rms")
  if (! method %in% methods){
    stop('Given method is not valid.\n', method)
  }

  #check ram, if raster should be returned
  if (return_raster){
    #check needed RAM
    if (!ignore_RAM){
      ebv_i_check_ram(res,timestep_src,entity_src, type.long)
    } else{
      if(verbose){
        print('RAM capacities are ignored.')
      }
    }
  }

  #get epsg
  epsg_src <- prop_src@spatial$epsg

  #######initial test end ----

  #get output type
  type_ot <- ebv_i_type_ot(type.long)
  type_terra <- ebv_i_type_terra(type_ot)

  #get data ----
  #open netCDF with terra
  data_raw <- terra::rast(filepath_src, subds = paste0('/', datacubepath_src))

  #get the index depending on the amount of entities and timesteps
  max_time <- prop_src@spatial$dimensions[3]
  terra_index <- (entity_index-1)*max_time + timestep_src
  data_ts <- data_raw[[terra_index]]

  #create dummy terra SpatRast for projection
  dummy <- terra::rast()
  terra::crs(dummy) <- paste0('EPSG:', epsg_dest)
  #set extent
  if(!is.null(filepath_dest)){
    #BASED ON EBV NETCDF FILE
    extent <- terra::ext(terra::rast(filepath_dest,2))
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
      data_proj <- terra::project(data_ts, y = dummy, align=TRUE, method=method, gdal=TRUE)
    },
    error=function(e){
      # if (!stringr::str_detect(e, 'cannot create dataset from source')){
      #   stop(e)
      # }
      if(verbose){
        message('Slower algorithm needs to be used. Please be patient.')
      }

      data_proj <- terra::project(data_ts, y = dummy, align=TRUE, method=method, gdal=FALSE)
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
