#' Change the resolution of the data of an EBV NetCDF
#'
#' @description Change the resolution of one datacube of a EBV NetCDF based on
#'   another EBV NetCDF or a given resolution. This functions writes temporary
#'   files on your disk. Specify a directory for these setting via
#'   options('ebv_temp'='/path/to/temp/directory').
#'
#' @param filepath_src Character. Path to the NetCDF file whose resolution
#'   should be changed.
#' @param datacubepath_src Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]) whose resolution should be changed..
#' @param resolution Either the path to an EBV NetCDF file that determines the
#'   resolution (character) or the resolution defined directly (numeric). The
#'   vector defining the resolution directly must contain three elements: the
#'   x-resolution, the y-resolution and the corresponding epsg.
#' @param outputpath Character. Set path to write data as GeoTiff on disk.
#' @param timestep Integer. Choose one or several timesteps (vector).
#' @param method Character. Default: Average. Define resampling method. Choose
#'   from:
#'   "near","bilinear","cubic","cubicspline","lanczos","average","mode","max","min","med","q1"
#'    and "q3". For detailed information see:
#'   \href{https://gdal.org/programs/gdalwarp.html}{gdalwarp}.
#' @param return_raster Logical. Default: FALSE. Set to TRUE to directly get the
#'   corresponding raster object.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
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
#' #define temp directory
#' options('ebv_temp'=system.file("extdata/", package="ebvnetcdf"))
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' res1 <- system.file(file.path("extdata","rodinini_001.nc"), package="ebvnetcdf")
#' res2 <- c(1,1,4326)
#' out <- file.path(system.file(package='ebvnetcdf'),"extdata","changeRes.tif")
#' #ebv_resample(file, datacubes[1,1], res1,  out, c(1,6))
#' #d <- ebv_resample(file, datacubes[1,1], res2, NULL, 3, method='max', return_raster=TRUE)
ebv_resample <- function(filepath_src, datacubepath_src, resolution, outputpath, timestep = 1,
                                method='average', return_raster=FALSE, overwrite = FALSE, ignore_RAM=FALSE, verbose=FALSE){
  ####initial tests start ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  #ensure that all tempfiles are deleted on exit
  withr::defer(
    if(exists('temp')){
      if(file.exists(temp)){
        file.remove(temp)
      }
    }
  )
  withr::defer(
    if(exists('temp_2')){
      if(file.exists(temp_2)){
        file.remove(temp_2)
      }
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
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath_src)

  #check if res is given or filepath to destination
  if(checkmate::checkNumeric(resolution)==TRUE){
    if(checkmate::checkNumeric(resolution, len=3)==TRUE){
      epsg_dest <- resolution[3]
      res <- resolution[1:2]
      filepath_dest <- NULL
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
      stop(paste0('File ending is wrong. File cannot be processed.'))
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
  if (rhdf5::H5Lexists(hdf, datacubepath_src)==FALSE){
    stop(paste0('The given variable is not valid:\n', datacubepath_src))
  }
  rhdf5::H5Fclose(hdf)

  #get properties source
  prop_src <- ebv_properties(filepath_src, datacubepath_src, verbose)
  type.long <- prop_src@ebv_cube$type

  #source timestep check
  #check if timestep is valid type
  if(checkmate::checkIntegerish(timestep) != TRUE){
    stop('Timestep has to be an integer or a list of integers.')
  }

  #check timestep range
  max_time <- prop_src@spatial$dimensions[3]
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
  methods <- c("near","bilinear","cubic","cubicspline","lanczos","average","mode","max","min","med","q1","q3")
  if (! method %in% methods){
    stop('Given method is not valid.\n', method)
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

  #check ram, if raster should be returned
  if (return_raster){
    #check needed RAM
    if (!ignore_RAM){
      ebv_i_check_ram(res,timestep,type.long)
    } else{
      message('RAM capacities are ignored.')
    }
  }

  #######initial test end ----

  #get epsg
  epsg_src <- prop_src@spatial$epsg

  #check if both epsg are valid
  epsg_list <- rgdal::make_EPSG()
  if (! epsg_dest %in% epsg_list$code){
    stop(paste0('The given target epsg is not valid or not supported by R.\n', epsg_dest))
  }
  if (! epsg_src %in% epsg_list$code){
    stop(paste0('The given source epsg is not valid or not supported by R.\n', epsg_src))
  }

  #srs defintion from epsg
  srs_src <- sp::CRS(SRS_string = paste0('EPSG:',epsg_src))
  srs_dest <- sp::CRS(SRS_string = paste0('EPSG:',epsg_dest))

  #get output type ot for gdal
  #type.long <- prop_src@ebv_cube_information@type
  ot <- ebv_i_type_ot(type.long)

  #set path to variable in netcdf for gdal
  filepath <- paste0('NETCDF:', filepath_src,':',datacubepath_src)

  #check if src dataset has several timesteps ----
  if (prop_src@spatial$dimensions[3] > 1){
    #define output parameters
    name <- 'temp_EBV_change_res_time.tif'
    temp <- file.path(temp_path, name)
    #select given timesteps, write tempfile
    if (!is.null(ot)){
      gt <- gdalUtils::gdal_translate(filepath, temp, b = timestep,
                                      ot = ot,
                                      co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                                      overwrite=TRUE,
                                      a_srs = srs_src)
    } else {
      gt <- gdalUtils::gdal_translate(filepath, temp, b = timestep,
                                      co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                                      overwrite=TRUE,
                                      a_srs = srs_src)
    }
    #change filepath
    filepath <- temp
  }

  if(!file.exists(filepath)){
    stop('GDAL did not work properly. Did you install GDAL correctly? You can
    check and set the correct GDAL paths with the following lines of code:
    #check out which GDAL installation is used currently by gdalUtils:
    getOption("gdalUtils_gdalPath")[[1]]$python_utilities
    #your paths may differ! check your GDAL installation
    #add GDAL path to the existing paths
    Sys.getenv("PATH")
    Sys.setenv(PATH = paste0("C:\\OSGeo4W64\\bin;",Sys.getenv("PATH")))
    #check and change path for proj_lib, gdal_data and gdal_driver_path
    Sys.getenv("PROJ_LIB")
    Sys.getenv("GDAL_DATA")
    Sys.getenv("GDAL_DRIVER_PATH")
    Sys.setenv(PROJ_LIB = "C:\\OSGeo4W64\\share\\proj")
    Sys.setenv(GDAL_DATA = "C:\\OSGeo4W64\\share\\gdal")
    Sys.setenv(GDAL_DRIVER_PATH = "C:\\OSGeo4W64\\bin\\gdalplugins")
        ')
  }

  #check if epsgs differ ----
  if(epsg_src != epsg_dest){
    #define output parameters
    name <- 'temp_EBV_change_res_epsg.tif'
    temp_2 <- file.path(temp_path, name)
    if (!is.null(ot)){
      gw <- gdalUtils::gdalwarp(srcfile = filepath,
                                dstfile = temp_2,
                                s_srs=srs_src,
                                t_srs=srs_dest,
                                ot = ot,
                                co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                                overwrite=TRUE)
    } else {
      gw <- gdalUtils::gdalwarp(filepath, temp_2,
                                s_srs=srs_src,
                                t_srs=srs_dest,
                                co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                                overwrite=TRUE)
    }
    filepath <- temp_2
  }

  if(!file.exists(filepath)){
    stop('GDAL did not work properly. Did you install GDAL correctly? You can
    check and set the correct GDAL paths with the following lines of code:
    #check out which GDAL installation is used currently by gdalUtils
    getOption("gdalUtils_gdalPath")[[1]]$python_utilities
    #your paths may differ! check your GDAL installation
    #add GDAL path to the existing paths
    Sys.getenv("PATH")
    Sys.setenv(PATH = paste0("C:\\OSGeo4W64\\bin;",Sys.getenv("PATH")))
    #check and change path for proj_lib, gdal_data and gdal_driver_path
    Sys.getenv("PROJ_LIB")
    Sys.getenv("GDAL_DATA")
    Sys.getenv("GDAL_DRIVER_PATH")
    Sys.setenv(PROJ_LIB = "C:\\OSGeo4W64\\share\\proj")
    Sys.setenv(GDAL_DATA = "C:\\OSGeo4W64\\share\\gdal")
    Sys.setenv(GDAL_DRIVER_PATH = "C:\\OSGeo4W64\\bin\\gdalplugins")
        ')
  }

  #get extent to align pixels ----
  if(!is.null(filepath_dest) & (epsg_src != epsg_dest)){
    extent <- raster::extent(raster::raster(filepath))
    lat.data <- rhdf5::h5read(filepath_dest, 'lat')
    lon.data <- rhdf5::h5read(filepath_dest, 'lon')
    lat.data <- sort(lat.data - res[1]/2)
    lon.data <- sort(lon.data - res[2]/2)
    xmin <-lat.data[min(which(extent@xmin < lat.data))]
    xmax <-lat.data[min(which(extent@xmax < lat.data))]
    ymin <-lon.data[min(which(extent@ymin < lon.data))]
    ymax <-lon.data[max(which(extent@xmax < lon.data))]
    te <- c(xmin, ymin, xmax, ymax)
  } else {
    te <- NULL
  }

  #write tif with new resolution ----
  if (!is.null(ot) & !is.null(te)){
    r <- gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@ebv_cube$fillvalue,
                  ot=ot,r = method, te = te,
                  overwrite=overwrite,
                  co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                  t_srs = srs_dest,
                  output_Raster = return_raster)
  } else if (is.null(ot) & !is.null(te)) {
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@ebv_cube$fillvalue,
                  r = method, te = te,
                  overwrite=overwrite,
                  co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                  t_srs = srs_dest,
                  output_Raster = return_raster)
  } else if(!is.null(ot) & is.null(te)){
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@ebv_cube$fillvalue,
                  r = method, ot = ot,
                  overwrite=overwrite,
                  co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                  t_srs = srs_dest,
                  output_Raster = return_raster)
  } else {
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@ebv_cube$fillvalue,
                  r = method,
                  overwrite=overwrite,
                  co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                  t_srs = srs_dest,
                  output_Raster = return_raster)
  }

  #remove tempfiles ----
  if (exists('temp')){
    if (file.exists(temp)){
      file.remove(temp)
    }
  }
  if (exists('temp_2')){
    if (file.exists(temp_2)){
      file.remove(temp_2)
    }
  }

  #return array ----
  if (return_raster){
    r <- raster::reclassify(r, cbind(prop_src@ebv_cube$fillvalue, NA))
    return(r)
  } else{
    return(outputpath)
  }
}
