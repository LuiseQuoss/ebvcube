#' Read subset (bounding box) of one datacube of an EBV netCDF
#'
#' @description Read a subset of one or more layers from one datacube of the
#'   NetCDF file. Subset definition by a bounding box.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. (As if the structure
#'   were 3D. Then no entity argument is needed.) Character string or single
#'   integer value indicating the entity of the 4D structure of the EBV netCDFs.
#' @param bb Integer Vector. Definition of subset by bounding box: c(xmin,
#'   xmax, ymin, ymax).
#' @param outputpath Character. Default: NULL, returns the data as a raster
#'   object in memory. Optional: set path to write subset as GeoTiff on disk.
#' @param timestep Integer. Choose one or several timesteps.
#' @param epsg Integer. Default: 4326 (WGS84). Change accordingly if your
#'   bounding box coordinates are based on a different coordinate reference
#'   system.
#' @param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Returns a raster object if no outputpath is given. Otherwise the
#'   subset is written onto the disk and the outputpath is returned.
#' @export
#'
#' @note In case the epsg of the Bounding Box and the netCDF differ, the data is
#'   returned based on the epsg of the netCDF Dataset.
#' @seealso [ebvnetcdf::ebv_read_shp()] for subsetting via shapefile.
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' out <- file.path(system.file(package='ebvnetcdf'),"extdata","subset_bb.tif")
#' bb_wgs84 <- c(5,15,47,55)
#' bb_utm32 <- c(271985, 941837, 5232640, 6101151)
#' #cSAR.germany <- ebv_read_bb(file, datacubes[1], bb_wgs84, timestep = c(1,4,12))
#' #path <- ebv_read_bb(file, datacubes[1], bb_wgs84, out, timestep = c(2,3))
#' #path  <- ebv_read_bb(file, datacubes[1], bb_utm32, out, timestep=1, epsg=32632, overwrite=T)
ebv_read_bb <- function(filepath, datacubepath, entity=NULL, timestep = 1, bb,
                        outputpath=NULL, epsg = 4326, overwrite=FALSE,
                        ignore_RAM = FALSE, verbose = FALSE){
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
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }
  #are all arguments given?
  if(missing(bb)){
    stop('Bounding box (bb) argument is missing.')
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

  #valid epsg check
  if(checkmate::checkIntegerish(epsg) != TRUE){
    stop('epsg must be of type integer.')
  }

  crs <- gdalUtils::gdalsrsinfo(paste0('EPSG:',epsg))
  if(any(stringr::str_detect(as.character(crs), 'crs not found'))){
    stop('Given EPSG code is not in PROJ library. Did you give a wrong EPSG code?')
  } else if (any(stringr::str_detect(as.character(crs), '(?i)error'))){
    stop(paste0('Could not process EPSG. See error from gdalUtils:\n', as.character(paste0(crs, collapse = '\n'))))
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
  if(lon_check | lat_check){
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
    message('RAM capacities are ignored.')
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
        mat <- mat[nrow(mat):1,,drop=FALSE]
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
    r <-raster::brick(
      array3d,
      xmn=xmin, xmx=xmax,
      ymn=ymin, ymx=ymax,
      crs=crs
    )

  }else{
    #get one timestep

    if(is_4D){
    #read from 4D structure
      part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep, entity_index), count = c(length(lon.indices),length(lat.indices),1,length(entity_index)))
      #create and rotate array
      mat <- matrix(part, c(nrow, ncol))
      mat <- t(mat)
      mat <- mat[nrow(mat):1,,drop=FALSE]

    }else{
    #read from 3D structure
      part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep), count = c(length(lon.indices),length(lat.indices),1))
      #create and rotate array
      mat <- matrix(part, c(nrow, ncol))
      mat <- t(mat[,,drop=FALSE])
      #mat <- mat[nrow(mat):1,]

    }

    #array to raster
    r <- raster::raster(
      mat,
      xmn=xmin, xmx=xmax,
      ymn=ymin, ymx=ymax,
      crs=crs
    )
  }

  #set nodata value
  r <- raster::reclassify(r, cbind(prop@ebv_cube$fillvalue, NA))

  #return data ----
  if (!is.null(outputpath)){
    #write raster
    raster::writeRaster(r, outputpath, format = "GTiff", overwrite = overwrite)
    return(outputpath)
  }
  else{
    return(r)
  }

}
