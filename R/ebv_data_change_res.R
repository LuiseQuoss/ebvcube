#' Change the resoultion of the data from EBV NetCDF
#'
#' @description Change the resolution of the data of one datacube of a EBV NetCDF based on another EBV NetCDF or a given resolution.
#'
#' @param filepath_src Path to the NetCDF file whose resolution should be changed.
#' @param datacubepath_src Path to the datacube (use [ebvnetcdf::ebv_datacubepaths()]) whose resolution should be changed..
#' @param resolution Either the path to a NetCDF file that determines the resolution or the resultion defined directly. The vector defining the resolution directly must contain three elements: the x-resoltion, the y-resolution and the corresponding epsg.
#' @param outputpath Set path to write subset as GeoTiff on disk.
#' @param timestep Choose one or several timesteps (vector).
#' @param method Resampling method, default: Average. Choose from: "near","bilinear","cubic","cubicspline","lanczos","average","mode","max","min","med","q1" and "q3". For detailed information see: \href{https://gdal.org/programs/gdalwarp.html}{gdalwarp}.
#' @param return.raster Default: FALSE. Set to TRUE to directly get the corresponting raster object.
#' @param overwrite Default: FALSE. Set to TRUE to overwrite the outputfile defined by 'outputpath'.
#' @param ignore.RAM Checks if there is enough space in your memory to read the data. Can be switched off (set to TRUE).
#'
#' @return Default: returns the outputpath of the GeoTiff with the new resolution. Optional: return the raster object with the new resolution.
#' @export
#'
#' @examples
#' file <- paste0(path.package("ebvnetcdf"),"/extdata/cSAR_idiv_v1.nc")
#' datacubes <- ebv_datacubepaths(file)
#' res1 <- 'path/to/NetCDF/with/different/resolution.nc'
#' res2 <- c(1,1,4326)
#' out <- 'path/to/save/tif/with/new/res.tif'
#' # ebv_data_change_res(file, datacubes[1,1], res1,  out, c(1,6))
#' # data <- ebv_data_change_res(file, datacubes[1,1], res2,  out, 3, method='max', return.raster=T, overwrite=T)
ebv_data_change_res <- function(filepath_src, datacubepath_src, resolution, outputpath, timestep = 1,
                                method='average', return.raster=FALSE, overwrite = FALSE, ignore.RAM=FALSE){
  ####initial tests start
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

  #filepath src check
  if (!file.exists(filepath_src)){
    stop(paste0('File does not exist.\n', filepath_src))
  }
  if (!endsWith(filepath_src, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath_src)

  #check if res is given or filepath to destination
  if(class(resolution)=='numeric'){
    epsg_dest <- resolution[3]
    res <- resolution[1:2]
    filepath_dest = NULL
  } else {
    filepath_dest <- resolution
  }

  if(!is.null(filepath_dest)){
    #filepath dest check
    if (!file.exists(filepath_dest)){
      stop(paste0('File does not exist.\n', filepath_dest))
    }
    if (!endsWith(filepath_dest, '.nc')){
      stop(paste0('File ending is wrong. File cannot be processed.'))
    }

    #file closed
    ebv_i_file_opened(filepath_dest)

    #get properties
    prop_dest <- ebv_properties(filepath_dest)

    #get target resolution
    res <- prop_dest@spatial_information@resolution
    res <- c(res[1], res[2]) #c(res[1]*-1, res[2])

    #get target epsg
    epsg_dest <- prop_dest@spatial_information@epsg

  }

  #source variable check
  hdf <- rhdf5::H5Fopen(filepath_src)
  if (rhdf5::H5Lexists(hdf, datacubepath_src)==FALSE){
    rhdf5::H5Fclose(hdf)
    stop(paste0('The given variable is not valid:\n', datacubepath_src))
  } else {
    rhdf5::H5Fclose(hdf)
  }

  #get properties source
  prop_src <- ebv_properties(filepath_src, datacubepath_src)
  type.long <- prop_src@entity_information@type

  #source timestep check
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
    max_time <- prop_src@spatial_information@dimensions[3]
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  #outputpath check
  if(!dir.exists(dirname(outputpath))){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  #check if outpufile exists if overwrite is disabled
  if(!overwrite){
    if(file.exists(outputpath)){
      stop('Output file already exists. Change name or enable overwrite.')
    }
  }

  #check if method is valid
  methods <- c("near","bilinear","cubic","cubicspline","lanczos","average","mode","max","min","med","q1","q3")
  if (! method %in% methods){
    stop('Given method is not valid.\n', method)
  }

  #get temp directory
  temp_path <- getOption('temp_directory')[[1]]
  if (is.null(temp_path)){
    stop('This function creates a temporary file. Please specify a temporary directory via options.')
  } else if (!dir.exists(temp_path)){
    stop('The temporary directory given by you does not exist. Please change!\n', temp_path)
  }

  #check ram, if raster should be returned
  if (return.raster){
    #check needed RAM
    if (!ignore.RAM){
      ebv_i_check_ram(res,timestep,type.long)
    } else{
      message('RAM capacities are ignored.')
    }
  }

  #######initial test end

  #get epsg
  epsg_src <- prop_src@spatial_information@epsg

  #check if both epsg are valid
  epsg_list <- rgdal::make_EPSG()
  if (! epsg_dest %in% epsg_list$code){
    stop(paste0('The given target epsg is not valid or not supported by R.\n', epsg_dest))
  }
  if (! epsg_src %in% epsg_list$code){
    stop(paste0('The given source epsg is not valid or not supported by R.\n', epsg_src))
  }

  #get output type ot for gdal
  #type.long <- prop_src@entity_information@type
  ot <- ebv_i_type_r(type.long)

  #set path to variable in netcdf for gdal
  filepath <- paste0('NETCDF:', filepath_src,':',datacubepath_src)

  #check if src dataset has several timesteps
  if (prop_src@spatial_information@dimensions[3] > 1){
    #define output parameters
    name <- 'temp_EBV_change_res_time.tif'
    temp <- paste0(temp_path, '/', name)
    #select given timesteps, write tempfile
    if (!is.null(ot)){
      gt <- gdalUtils::gdal_translate(filepath, temp, b = timestep, ot = ot, overwrite=TRUE)
    } else {
      gt <- gdalUtils::gdal_translate(filepath, temp, b = timestep, overwrite=TRUE)
    }
    #change filepath
    filepath <- temp
  }

  #check if epsgs differ
  if(epsg_src != epsg_dest){
    #define output parameters
    name <- 'temp_EBV_change_res_epsg.tif'
    temp_2 <- paste0(temp_path, '/', name)
    if (!is.null(ot)){
      gw <- gdalUtils::gdalwarp(filepath, temp_2, ot = ot, t_srs=paste0('EPSG:',epsg_dest), overwrite=TRUE)
    } else {
      gw <- gdalUtils::gdalwarp(filepath, temp_2, t_srs=paste0('EPSG:',epsg_dest), overwrite=TRUE)
    }
    filepath <- temp_2
  }

  #get extent to align pixels
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

  #write tif with new resolution
  if (!is.null(ot) & !is.null(te)){
    r <- gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@entity_information@fillvalue,
                  ot=ot,r = method, te = te,
                  overwrite=overwrite,
                  output_Raster = return.raster)
  } else if (is.null(ot) & !is.null(te)) {
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@entity_information@fillvalue,
                  r = method, te = te,
                  overwrite=overwrite,
                  output_Raster = return.raster)
  } else if(!is.null(ot) & is.null(te)){
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@entity_information@fillvalue,
                  r = method, ot = ot,
                  overwrite=overwrite,
                  output_Raster = return.raster)
  } else {
    r <-  gdalUtils::gdalwarp(filepath, outputpath,
                  tr=res,srcnodata=prop_src@entity_information@fillvalue,
                  r = method,
                  overwrite=overwrite,
                  output_Raster = return.raster)
  }

  #remove tempfiles
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

  #return array
  if (return.raster){
    r <- raster::reclassify(r, cbind(prop_src@entity_information@fillvalue, NA))
    return(r)
  } else{
    return(outputpath)
  }
}

