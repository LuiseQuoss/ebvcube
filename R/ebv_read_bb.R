#' Read subset (bounding box) of one datacube of an EBV netCDF
#'
#' @description Read a subset of one or more layers from one datacube of the
#'   NetCDF file. Subset definition by a bounding box.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param bb Integer Vector. Definition of subset by bounding box: c(xmin,
#'   xmax, ymin, ymax).
#' @param outputpath Character. Default: NULL, returns the data as a SpatRaster
#'   object in memory. Optional: set path to write subset as GeoTiff on disk.
#' @param timestep Integer or character. Select one or several timestep(s).
#'   Either provide an integer value or list of values that refer(s) to the
#'   index of the timestep(s) (minimum value: 1) or provide a date or list of
#'   dates in ISO format, such as '2015-01-01'.
#' @param epsg Integer. Default: 4326 (WGS84). Change accordingly if your
#'   bounding box coordinates are based on a different coordinate reference
#'   system.
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
#' @return Returns a SpatRaster object if no outputpath is given. Otherwise the
#'   subset is written onto the disk and the outputpath is returned.
#' @export
#'
#' @note In case the epsg of the Bounding Box and the netCDF differ, the data is
#'   returned based on the epsg of the netCDF Dataset.
#' @seealso [ebvcube::ebv_read_shp()] for subsetting via shapefile.
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#'
#' #set outputpath
#' out <- file.path(system.file(package='ebvcube'),"extdata","subset_bb.tif")
#' #define two different bounding boxes based on different EPSG codes
#' bb_wgs84 <- c(-26, 64, 30, 38)
#' bb_utm32 <- c(-2383703, 5532302, 3643854, 4564646)
#'
#'
#' \dontrun{
#' #read bb (based on EPSG 4326) - return SpatRaster
#' cSAR_subset <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
#'                              entity = 1, timestep = 1:3, bb = bb_wgs84)
#'
#' #read bb (based on EPSG 4326) - write to GeoTiff
#' path <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
#'                     entity = 1, timestep = 1, bb = bb_wgs84,
#'                     outputpath = out, overwrite = TRUE)
#'
#' #read bb (based on ESRI 54009) - write to GeoTiff
#' path  <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
#'                      entity = 1, timestep = 1:2, bb = bb_utm32,
#'                      epsg = 32632, outputpath = out, overwrite = TRUE)
#' }
ebv_read_bb <- function(filepath, datacubepath = NULL, entity=NULL, timestep = 1, bb,
                        outputpath=NULL, epsg = 4326, scenario = NULL, metric = NULL,
                        overwrite=FALSE, ignore_RAM = FALSE, verbose = TRUE){
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
  if(missing(bb)){
    stop('Bounding box (bb) argument is missing.')
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

  #filepath check
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

  #valid epsg check
  if(checkmate::checkIntegerish(epsg) != TRUE){
    stop('epsg must be of type integer.')
  }

  #check whether the epsg can be processed
  ebv_i_eval_epsg(epsg)

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


  #######initial test end ----

  #get basic information of file ----
  crs <- prop@spatial$wkt2
  epsg_file <- prop@spatial$epsg
  resolution <- prop@spatial$resolution
  ext <- prop@spatial$extent

  #transform bb if necessary ----
  if (epsg_file != epsg){
    bb <- ebv_i_transform_bb(bb, epsg, epsg_file)
  }

  #get lon&lat data ----
  lat.data <- rhdf5::h5read(filepath, 'lat')
  lon.data <- rhdf5::h5read(filepath, 'lon')

  #check if bb is in spatial extent
  lat_check <- ext[1]>bb[1] | ext[2]<bb[2]
  lon_check <- ext[3]>bb[3] | ext[4]<bb[4]
  if(lon_check || lat_check){
    warning(paste0('The extent of the bounding box is bigger than the area covered by the dataset.
                   Data output encompasses a smaller extent.\nGiven bb: ', paste(bb, collapse = ' '),
                   '\nDataset bb: ', paste(ext, collapse = ' ')), immediate. = TRUE)
  }

  #indices INCLUDING all pixels in specficied bb - bigger than original bb
  #lat indices
  lat.indices <- which(lat.data >= bb[3] & lat.data <= bb[4])
  if (ebv_i_empty(lat.indices)){
    stop('The bounding box given by you does not include any data. Have you given the bounding box with the corresponding EPSG in case it differs from WGS84 (EPSG 4326)?')
  }
  if (min(lat.data[lat.indices]) != min(lat.data)){ #lat.data[max(lat.indices)]
    if (min(lat.data[lat.indices])-resolution[2]/2 > bb[3]){
      lat.indices <- sort(append(lat.indices, (max(lat.indices)+1)))
    }
  }
  if (max(lat.data[lat.indices]) != max(lat.data)){ #lat.data[min(lat.indices)]
    if(max(lat.data[lat.indices]) + resolution[2]/2 < bb[4]){
      lat.indices <- sort(append(lat.indices, (min(lat.indices)-1)))
    }
  }
  #lon indices
  lon.indices <- which(lon.data >= bb[1] & lon.data <= bb[2])
  if (ebv_i_empty(lon.indices)){
    stop('The bounding box given by you does not include any data. Have you given the bounding box with the corresponding EPSG in case it differs from WGS84 (EPSG 4326)?')
  }
  if (max(lon.data[lon.indices]) != max(lon.data)){
    if (max(lon.data[lon.indices]) + resolution[1]/2 < bb[2]){
      lon.indices <- sort(append(lon.indices, (max(lon.indices)+1)))
    }
  }
  if (min(lon.data[lon.indices]) != min(lon.data)){
    if(min(lon.data[lon.indices]) - resolution[1]/2 > bb[1]){
      lon.indices <- sort(append(lon.indices, (min(lon.indices)-1)))
    }
  }

  #extent for raster creation ----
  xmin <- min(lon.data[lon.indices])-resolution[1]/2
  xmax <- max(lon.data[lon.indices])+resolution[1]/2
  ymin <- min(lat.data[lat.indices])-resolution[2]/2
  ymax <- max(lat.data[lat.indices])+resolution[2]/2

  ncol <- length(lat.indices)
  nrow <- length(lon.indices)

  #check needed RAM ----
  if (!ignore_RAM){
    ebv_i_check_ram(c(ncol, nrow), timestep, entity, prop@ebv_cube$type)
  } else{
    if(verbose){
      print('RAM capacities are ignored.')
    }
  }

  #get data ----
  #get multiple timesteps
  if (length(timestep)>1){
    array3d <- array(dim=c(ncol,nrow, length(timestep)))

    if(is_4D){
      #read from 4D structure
      for (i in 1:length(timestep)){
        #get subset
        part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep[i], entity_index), count = c(length(lon.indices),length(lat.indices),1,length(entity_index)))
        #create and rotate array
        mat <- matrix(part, c(nrow, ncol,1))
        mat <- t(mat)
        #mat <- mat[nrow(mat):1,,drop=FALSE]
        array3d[,,i] <- array(mat, c(ncol, nrow))
      }
    } else{
      # read from 3D structure
      for (i in 1:length(timestep)){
        #get subset
        part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep[i]), count = c(length(lon.indices),length(lat.indices),1))
        #create and rotate array
        mat <- matrix(part, c(nrow, ncol))
        mat <- t(mat[,,drop=FALSE])
        #mat <- mat[nrow(mat):1,]

        array3d[,,i] <- array(mat, c(ncol, nrow))
      }

    }

    #array to raster
    extent <- terra::ext(c(xmin, xmax,ymin,ymax))
    r <- terra::rast(array3d, crs=crs, extent=extent)

  }else{
    #get one timestep

    if(is_4D){
    #read from 4D structure
      part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep, entity_index), count = c(length(lon.indices),length(lat.indices),1,length(entity_index)))
      #create and rotate array
      mat <- matrix(part, c(nrow, ncol))
      mat <- t(mat)
      #mat <- mat[nrow(mat):1,,drop=FALSE]

    }else{
    #read from 3D structure
      part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep), count = c(length(lon.indices),length(lat.indices),1))
      #create and rotate array
      mat <- matrix(part, c(nrow, ncol))
      mat <- t(mat[,,drop=FALSE])
      #mat <- mat[nrow(mat):1,]

    }

    #array to raster
    extent <- terra::ext(c(xmin, xmax,ymin,ymax))
    r <- terra::rast(mat, crs=crs, extent=extent)

  }

  #set nodata value
  r <- terra::classify(r, cbind(prop@ebv_cube$fillvalue, NA))

  #return data ----
  if (!is.null(outputpath)){
    #write raster
    terra::writeRaster(r, outputpath,  overwrite = overwrite, filetype="GTiff")
    return(outputpath)
  }
  else{
    return(r)
  }

}
