#' Read subset (bounding box) of the data from EBV NetCDF
#'
#' @description Read a subset of one or more layers from one datacube of the NetCDF file. Subset definition by a bounding box.
#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use [ebvnetcdf::ebv_datacubepaths()]).
#' @param bb Definition of subsset by bounding box: c(xmin, xmax, ymin, ymax).
#' @param outputpath Defaul: NULL, returns the data as a raster object in memory. Optional: set path to write subset as GeoTiff on disk, returns outputpath.
#' @param timestep Choose one or several timesteps (vector).
#' @param epsg Default: 4326 (WGS84). Change accordingly if your bounding box coordinates are based on a different coordinate reference system.
#' @param overwrite Default: FALSE. Set to TRUE to overwrite the outputfile defined by 'outputpath'.
#' @param ignore.RAM Checks if there is enough space in your memory to read the data. Can be switched off (set to TRUE).
#'
#' @return Returns a raster object if no outputpath is given. Otherwise the subset is written onto the disk and the ouputpath is returned.
#' @export
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # out <- 'path/to/write/subset.tif'
#' # cSAR.germany <- ebv_data_read_bb(file, datacubes[1], c(5,15,47,55), timestep = c(1,4,12))
#' # out.path <- ebv_data_read_bb(file, datacubes[1], c(5,15,47,55), out, c(2,3))
#' # out.path <- ebv_data_read_bb(file, datacubes[1], c(271985, 941837, 5232640, 6101151), out, 1, epsg=32632, overwrite=T)
ebv_data_read_bb <- function(filepath, datacubepath, bb, outputpath=NULL, timestep = 1, epsg = 4326, overwrite=FALSE, ignore.RAM = FALSE){
  ####initial tests start
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

  #filepath check
  if (!file.exists(filepath)){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath)

  #variable check
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    rhdf5::H5Fclose(hdf)
    stop(paste0('The given variable is not valid:\n', datacubepath))
  } else {
    rhdf5::H5Fclose(hdf)
  }

  #get properties
  prop <- ebv_properties(filepath, datacubepath)

  #timestep check
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
    max_time <- prop@spatial_information@dimensions[3]
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  #outputpath check
  if (!is.null(outputpath)){
    if(!dir.exists(dirname(outputpath))){
      stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
    }
    #check if outpufile exists if overwrite is disabled
    if(!overwrite){
      if(file.exists(outputpath)){
        stop('Output file already exists. Change name or enable overwrite.')
      }
    }
  }

  #valid epsg check
  epsg_list <- rgdal::make_EPSG()
  if (! epsg %in% epsg_list$code){
    stop(paste0('The given epsg is not valid or not supported by R.\n', epsg))
  }

  #######initial test end

  #get basic information of file
  crs <- prop@spatial_information@srs@projargs
  epsg_file <- prop@spatial_information@epsg
  resolution <- prop@spatial_information@resolution
  ext <- prop@spatial_information@extent

  #transform bb if necessary
  if (epsg_file != epsg){
    bb <- ebv_i_transform_bb(bb, epsg, epsg_file)
  }

  #get lon&lat data
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
  if (is.empty(lat.indices)){
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
  if (is.empty(lon.indices)){
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

  #extent for raster creation
  xmin <- min(lon.data[lon.indices])-resolution[1]/2
  xmax <- max(lon.data[lon.indices])+resolution[1]/2
  ymin <- min(lat.data[lat.indices])-resolution[2]/2
  ymax <- max(lat.data[lat.indices])+resolution[2]/2

  ncol <- length(lat.indices)
  nrow <- length(lon.indices)

  #check needed RAM
  if (!ignore.RAM){
    ebv_i_check_ram(c(ncol, nrow), timestep, prop@entity_information@type)
  } else{
    message('RAM capacities are ignored.')
  }

  #get multiple timesteps - 3D
  if (length(timestep)>1){
    array3d <- array(dim=c(ncol,nrow, length(timestep)))

    for (i in 1:length(timestep)){
      #get subset
      part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep[i]), count = c(length(lon.indices),length(lat.indices),1))
      #create and rotate array
      mat <- matrix(part, c(nrow, ncol))
      mat <- t(mat[nrow(mat):1,,drop=FALSE])
      mat <- mat[,ncol(mat):1,drop=FALSE]

      array3d[,,i] <- array(mat, c(ncol, nrow))
    }

    #array to raster
    r <-raster::brick(
      array3d,
      xmn=xmin, xmx=xmax,
      ymn=ymin, ymx=ymax,
      crs=crs
    )

  }else{
    #get one timestep 2D
    part <- rhdf5::h5read(filepath, datacubepath, start=c(min(lon.indices),min(lat.indices),timestep), count = c(length(lon.indices),length(lat.indices),1))
    #create and rotate array
    mat <- matrix(part, c(nrow, ncol))
    mat <- t(mat[nrow(mat):1,,drop=FALSE])
    mat <- mat[,ncol(mat):1,drop=FALSE]

    #array to raster
    r <- raster::raster(
      mat,
      xmn=xmin, xmx=xmax,
      ymn=ymin, ymx=ymax,
      crs=crs
    )
  }

  #set nodata value
  r <- raster::reclassify(r, cbind(prop@entity_information@fillvalue, NA))

  if (!is.null(outputpath)){
    #write raster
    raster::writeRaster(r, outputpath, format = "GTiff", overwrite = overwrite)
    return(outputpath)
  }
  else{
    return(r)
  }

}
