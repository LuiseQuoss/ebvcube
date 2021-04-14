#' Add data to self-created EBV NetCDF
#'
#' @description Add data to the self-created EBV NetCDF from GeoTiffs.
#'
#' @param filepath_nc Path to the self-created NetCDF file.
#' @param filepath_tif Path to the GeoTiff file containing the data.
#' @param metric Default: 1. Indicates to which metric the data should be added.
#' @param scenario Default: NULL. Indicates to which scenario the data should be
#'   added.
#' @param entity Default: NULL. Indicates to which entity the data should be
#'   added.
#' @param timestep Default: 1. Define to which timestep or timesteps the data
#'   should be added. If several timesteps are given they have to be in a
#'   continuous order. Meaning c(4,5,6) is right but c(2,5,6) is wrong.
#' @param band Default: 1. Define which band(s) to read from GeoTiff. Can be
#'   several. Don't have to be in order as the timesteps definition requires.
#' @param ignore.RAM Checks if there is enough space in your memory to read the
#'   data. Can be switched off (set to TRUE).
#'
#' @note If the data exceeds your memory the RAM check will throw an error. No
#'   block-processing or other method implemented so far. Move to a machine with
#'   more capacities for the moment.
#'
#' @return Adds data to the EBV NetCDF. Check your results using
#'   [ebvnetcdf::ebv_datacubepaths()], [ebvnetcdf::ebv_data_read()] and/or
#'   [ebvnetcdf::ebv_data_analyse()].
#' @export
#'
#' @examples
#' # file <- 'path/to/created/netcdf/file.nc'
#' # tif <- 'path/to/geotiff/containing/data.tif'
#' # scenario <- 1
#' # metric <- 1
#' # entity <- 1
#' # ts <- c(1:6)
#' # band <- c(1:6)
#' # ebv_ncdf_add_data(file, tif, metric, scenario, entity, ts, band)
ebv_ncdf_add_data <- function(filepath_nc, filepath_tif, metric=1, scenario=NULL, entity=NULL, timestep=1, band=1, ignore.RAM=FALSE){
  ### start initial tests ----
  #are all arguments given?
  if(missing(filepath_nc)){
    stop('Filepath_nc argument is missing.')
  }
  if(missing(filepath_tif)){
    stop('Filepath_tif argument is missing.')
  }

  #check if nc file exists
  if (!file.exists(filepath_nc)){
    stop(paste0('NetCDF file does not exist.\n', filepath_nc))
  }
  if (!endsWith(filepath_nc, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
  }

  #check if tif file exists
  if (!file.exists(filepath_tif)){
    stop(paste0('Tif file does not exist.\n', filepath_tif))
  }
  if (!endsWith(filepath_tif, '.tif')){
    stop(paste0('Tif file ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath_nc)

  #check if a scenario is given, when nc has scenarios
  ls <- rhdf5::h5ls(filepath_nc)
  scenario.len <- length(grep("^scenario", ls[,2]))
  if (scenario.len>0 & is.null(scenario)){
    stop('The given NetCDF contains scenarios. You must define the scenario argument.')
  }

  #check if an entity is given, when nc has entities
  entity.len <- length(rhdf5::h5read(filepath_nc, 'var_entity'))
  if (entity.len>1 & is.null(entity)){
    stop('The given NetCDF contains entities. You must define the entity argument.')
  } else if (entity.len==1){
    if (rhdf5::h5read(filepath_nc, 'var_entity')!= 'data'){
      stop('The given NetCDF contains entities. You must define the entity argument.')
    }
  }

  #check timesteps
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
  max_time <- length(rhdf5::h5read(filepath_nc,'time'))
  for (t in timestep){
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  #check if band is valid type
  if (class(band)=='numeric'){
    for (b in band){
      if (! as.integer(b)==b){
        stop('Band has to be an integer or a list of integers.')
      }
    }
  } else {
    stop('Band has to be of class numeric.')
  }

  #check if timesteps and bands have the same length
  if (length(band) != length(timestep)){
    stop('The amount of bands to read from Tiff and the amount of timesteps to write to NetCDF differ. Have to be the same.')
  }
  #check if timesteps and tif have the same length/amount of bands
  tif_info <- gdalUtils::gdalinfo(filepath_tif)
  b.count <- stringr::str_count(tif_info, 'Band')
  b.sum <- sum(b.count, na.rm=T)
  if (b.sum < length(timestep)){
    stop('The amount of timesteps to write to NetCDF is longer than the available bands in Tiff.')
  }
  #check if band available in Tif
  if (max(band) > b.sum){
    stop('The highest band that should be used exceeds the amount of bands in Tiff.')
  }

  #check needed RAM to read tif info
  #get dims
  index <- stringr::str_detect(tif_info, 'Size is')
  size.chr <- tif_info[index]
  size.int <- as.integer(regmatches(size.chr, gregexpr("[[:digit:]]+", size.chr))[[1]])
  #get type
  index <- stringr::str_detect(tif_info, 'Type=')
  type.chr <- tif_info[index][1]
  if (stringr::str_detect(type.chr, 'Float')){
    type.long <- 'xx_xx_Float'
  } else if (stringr::str_detect(type.chr, 'CFloat')){
    type.long <- 'xx_xx_Float'
  } else if (stringr::str_detect(type.chr, 'Int')){
    type.long <- 'xx_xx_Int'
  } else if (stringr::str_detect(type.chr, 'UInt')){
    type.long <- 'xx_xx_Int'
  } else if (stringr::str_detect(type.chr, 'CInt')){
    type.long <- 'xx_xx_Int'
  }

  if (!ignore.RAM){
    ebv_i_check_ram(size.int,timestep,type.long)
  } else{
    message('RAM capacities are ignored.')
  }

  #check if dims of tif data correspond to lat and lon in netcdf
  lat.len <- length(rhdf5::h5read(filepath_nc, 'lat'))
  lon.len <- length(rhdf5::h5read(filepath_nc, 'lon'))
  if ((size.int[1] != lon.len) & (size.int[2] != lat.len)){
    stop(paste0('The size of your GeoTiff does not correspond to the latitude and longitude coordinates.
  Size should be: ', lon.len, ', ', lat.len, '. But is: ', size.int[1], ', ', size.int[2]))
  }
  if (size.int[1] != lon.len) {
    stop(paste0('The size of your GeoTiff doesn not match the longitudinal coordinates.
  Size sould be: ', lon.len, '. But size is: ', size.int[1]))
  } else if (size.int[2] != lat.len){
    stop(paste0('The size of your GeoTiff doesn not match the longitudinal coordinates.
  Size sould be: ', lat.len, '. But size is: ', size.int[2]))
  }

  ### end initial test ----

  #open hdf file ----
  hdf <- rhdf5::H5Fopen(filepath_nc)

  #get data from tif ----
  if (length(timestep) > 1){
    raster <- raster::brick(filepath_tif)[[band]] #RAM PROBLEM FOR HUGE DATA!!! read into delayed array?
    raster <- raster::brick(raster) #convert raster stack to raster brick
  } else{
    raster <- raster::raster(filepath_tif, band) #RAM PROBLEM FOR HUGE DATA!!! read into delayed array?
  }

  #get value range from tif
  if (raster@data@haveminmax){
    min <- min(raster@data@min)
    max <- max(raster@data@max)
    value_range <- c(min, max)
  }else{
    value_range <- 'None'
  }

  #get fill value from tif
  nodata <- raster@file@nodatavalue
  if (nodata == '-Inf'){
    nodata = 'None'
  }
  #get data type from tif
  type.hdf<- ebv_i_type_raster(raster@file@datanotation, raster@file@byteorder)

  #read amount of entities from hdf file
  len.e <- length(rhdf5::h5read(hdf, 'var_entity'))

  #create entity name accordingly
  if (entity.len>1){
    len.e <- nchar(as.character(len.e))+1
    #create metric group
    ending <- as.character((entity-1))
    while(nchar(ending)<len.e){
      ending <- paste0('0',ending)
    }
    name <- paste0('entity', ending)
  } else if (rhdf5::h5read(filepath_nc, 'var_entity')== 'data'){
    name = 'data'
  } else {
    name = 'entity0'
  }

  #rotate data
  if (length(timestep) > 1){
    data <- array(NA, dim=c(dim(raster)[2], dim(raster)[1], dim(raster)[3]))
    for (i in 1:length(timestep)){
      temp <- as.matrix(as.array(raster)[,,i])
      temp <- t(temp[nrow(temp):1,])
      temp <- temp[,ncol(temp):1]
      data[,,i] <- temp
    }
  } else {
    data <- as.matrix(raster)
    data <- t(data[nrow(data):1,])
    data <- data[,ncol(data):1]
  }

  #dims
  dims <- c(dim(data)[1], dim(data)[2], max_time)

  #get subgroup: scenario & metric
  #get n of metric zeros
  ls <- pkgcond::suppress_warnings(rhdf5::h5ls(filepath_nc)) #warning because file is opened
  metrics.len <- length(grep("^metric", ls[,2]))
  if (is.null(scenario)){
    #define path
    zeros.m <- ''
    for (m in 1:nchar(metrics.len)){
      zeros.m <- paste0(zeros.m, '0')
    }
    metric_path <- paste0('metric', zeros.m, metric-1)
  } else {
    #define scenario/metric path
    #get n of scenario zeros
    scenario.len <- length(grep("^scenario", ls[,2]))
    metrics.len <- metrics.len/scenario.len
    #define path
    zeros.s <- ''
    for (m in 1:nchar(scenario.len)){
      zeros.s <- paste0(zeros.s, '0')
    }
    zeros.m <- ''
    for (m in 1:nchar(metrics.len)){
      zeros.m <- paste0(zeros.m, '0')
    }
    metric_path <- paste0('scenario',zeros.s, (scenario-1),'/metric', zeros.m, metric-1)
  }

  #open path
  gid <- rhdf5::H5Gopen(hdf, metric_path)

  #create DS if not already existent ----
  if (rhdf5::H5Lexists(gid, name)) {
    did <- rhdf5::H5Dopen(gid, name)
  }else {
    tid <- rhdf5::H5Tcopy(type.hdf)
    sid <- rhdf5::H5Screate_simple(dims)
    did <- rhdf5::H5Dcreate(gid, name, tid, sid)
    rhdf5::H5Sclose(sid)
  }

  #add rotated data at specified timeslots
  name.ds <- paste0(metric_path, '/', name)
  rhdf5::h5write(obj=data, file=hdf, name=name.ds, start=c(1,1,min(timestep)), count=c(dims[1], dims[2], length(timestep)))

  #add entity attributes----

  #grid_mapping
  ebv_i_char_att(did, 'grid_mapping', 'crs')

  # :_ChunkSizes = 1U, 180U, 360U; // uint
  if (! rhdf5::H5Aexists(did, '_ChunkSizes')){
    chunk <- c(1, dim(data)[2], dim(data)[1])
    sid <- rhdf5::H5Screate_simple(length(chunk))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_UINT")
    aid <- rhdf5::H5Acreate(did, name = '_ChunkSizes', tid,sid)
    rhdf5::H5Awrite(aid, chunk)
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  }

  # :value_range
  if (! rhdf5::H5Aexists(hdf, 'value_range')){
    sid <- rhdf5::H5Screate_simple(length(value_range))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
    #H5Tset_size(tid, max(nchar(ebv_subgroups_desc))+1)
    aid <- rhdf5::H5Acreate(hdf,'value_range', tid,sid)
    rhdf5::H5Awrite(aid, value_range)
    rhdf5::H5Aclose(aid)
    rhdf5::H5Sclose(sid)
  } else{
    old.vr <- ebv_i_read_att(hdf, 'value_range')
    old.min <- old.vr[1]
    old.max <- old.vr[2]
    new.vr <- old.vr
    if (old.min > value_range[1]){
      new.vr[1] <- value_range[1]
    }
    if (old.max < value_range[2]){
      new.vr[2] <- value_range[2]
    }
    if ((old.min != new.vr[1])|(old.max != new.vr[2])){
      value_range <- new.vr
      rhdf5::H5Adelete(hdf, 'value_range')
      sid <- rhdf5::H5Screate_simple(length(value_range))
      tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
      aid <- rhdf5::H5Acreate(hdf, name = 'value_range', tid,sid)
      rhdf5::H5Awrite(aid, value_range)
      rhdf5::H5Aclose(aid)
      rhdf5::H5Sclose(sid)
    }
  }


  # :units = "mean change of species diversity per area (pixel size) to baseline 1900";
  ebv_i_char_att(did, 'units', 'default')
  #name of metric?

  # :long_name = "Changes in local bird diversity (cSAR)";
  ebv_i_char_att(did, 'long_name', 'default')
  #is long_name the title? what is it?

  # :least_significant_digit = 4; // int
  ebv_i_int_att(did, 'least_significant_digit', 999)

  #attributes that are filled by user - created empty
  # :label = "forest bird species";
  ebv_i_char_att(did, 'label', 'default')

  #enter json$biologicalEntity$taxonomicCoverage?

  # :standard_name = "Changes in local bird diversity (cSAR): forest bird species";
  ebv_i_char_att(did, 'standard_name', 'default')
  #fill standard name automatically when label is added (long_name needed)?

  # :_FillValue = -3.4E38f; // float
  if (nodata!='None'){
    ebv_i_num_att(did, '_FillValue', nodata)
  } else {
    ebv_i_num_att(did, '_FillValue', 999)
  }

  # :description = "Changes in bird diversity at the grid cell level caused by land-use, estimated by the cSAR model (Martins & Pereira, 2017). It reports changes in species number (percentage and absolute), relative to 1900, for all bird species, forest bird species, and non-forest bird species in each cell. Uses the LUH 2.0 projections for land-use, and the PREDICTS coefficients for bird affinities to land-uses.";
  ebv_i_char_att(did, 'description', 'default')

  #delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close
  rhdf5::H5Gclose(gid)
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}
