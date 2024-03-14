#' Add data to your EBV netCDF
#'
#' @description Add data to your EBV netCDF from GeoTiffs or in-memory arrays.
#'   First, create a new EBV netCDF using [ebvcube::ebv_create()].
#'
#' @param filepath_nc Character. Path to the self-created netCDF file.
#' @param data Character or matrix or array. If character: Path to the GeoTiff
#'   file containing the data. Ending needs to be *.tif. If matrix or array:
#'   in-memory object holding the data.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs. The character string can be obtained using
#'   [ebvcube::ebv_properties()]. Choose the entity you are interested in from
#'   the slot general and the list item entity_names.
#' @param timestep Integer or character. Default: 1. Define to which timestep or
#'   timesteps the data should be added. If several timesteps are given they
#'   have to be in a continuous and in order. Meaning c(4,5,6) is right but
#'   c(2,5,6) is wrong. Alternatively you can provide a date or list of dates in
#'   ISO format, such as '2015-01-01' (also in order).
#' @param band Integer. Default: 1. Define which band(s) to read from GeoTiff.
#'   Can be several. Don't have to be in order as the timesteps definition
#'   requires.
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
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE). Ignore
#'   this argument when you give an array or a matrix for 'data' (it will do
#'   nothing).
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @note If the data exceeds your memory the RAM check will throw an error. No
#'   block-processing or other method implemented so far. Move to a machine with
#'   more capacities if needed.
#'
#' @return Adds data to the EBV netCDF. Check your results using
#'   [ebvcube::ebv_read()] and/or [ebvcube::ebv_analyse()] and/or
#'   [ebvcube::ebv_map()] and/or [ebvcube::ebv_trend()].
#' @export
#'
#' @importFrom utils capture.output
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","test.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubepaths <- ebv_datacubepaths(file, verbose=FALSE)
#' #set path to GeoTiff with data
#' tif <- system.file(file.path("extdata","entity1.tif"), package="ebvcube")
#'
#' # add data to the timestep 1, 2 and 3 using the first three bands of the GeoTiff
#' \dontrun{
#' #use datacubepath argument and define timestep by integer
#' ebv_add_data(filepath_nc = file, datacubepath = datacubepaths[1,1],
#'              entity = 1, timestep = 1:3, data = tif, band = 1:3)
#' #use metric argument and define timestep by ISO-format
#' ebv_add_data(filepath_nc = file, entity = 1,
#'              timestep = paste0(as.character(seq(1900,1920,10)), '-01-01'),
#'              metric = 1, data = tif, band = 1:3, verbose = FALSE)
#' }
ebv_add_data <- function(filepath_nc, datacubepath = NULL, entity = NULL, timestep = 1,
                         data, band = 1, scenario = NULL, metric = NULL,
                         ignore_RAM = FALSE, verbose = TRUE){
  ### start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  withr::defer(
    if(exists('file_space')){
      if(rhdf5::H5Iis_valid(file_space)==TRUE){rhdf5::H5Sclose(file_space)}
    }
  )
  withr::defer(
    if(exists('did')){
      if(rhdf5::H5Iis_valid(did)==TRUE){rhdf5::H5Dclose(did)}
    }
  )

  #are all arguments given?
  if(missing(filepath_nc)){
    stop('Filepath_nc argument is missing.')
  }
  if(missing(data)){
    stop('Data argument is missing.')
  }

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=FALSE) != TRUE){
    stop('ignore_RAM must be of type logical.')
  }

  #check if nc file exists
  if (checkmate::checkCharacter(filepath_nc) != TRUE){
    stop('NetCDF Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath_nc) != TRUE){
    stop(paste0('NetCDF File does not exist.\n', filepath_nc))
  }
  if (!endsWith(filepath_nc, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
  }

  #if data is character
  character <- FALSE
  matrix <- FALSE
  array <- FALSE
  if(checkmate::test_character(data) && !any(class(data) == 'matrix')){
    character <- TRUE
    #check if tif file exists
    if (checkmate::checkFileExists(data) != TRUE){
      stop(paste0('GeoTiff does not exist.\n', data))
    }
    if (!endsWith(data, '.tif')){
      stop(paste0('GeoTiff file ending is wrong. File cannot be processed.'))
    }
  } else if(class(data)[1]=='array'){
    array <- TRUE
    if(length(dim(data))>3){
      stop(paste0('Maximum number of dimensions for the data array is 3.
                  Your data array has more than 3 dimensions.'))
    }
  } else if('matrix' %in% class(data)){
    matrix <- TRUE
  } else{
    stop('Your data argument is not of type character, array or matrix.')
  }

  #file closed?
  # ebv_i_file_opened(filepath_nc, verbose)

  #already rotate data for tests etc.
  if(matrix && !array){
    data <- t(data)
  } else if(array){
    #rotate array
    data <- apply(data, c(1,3), t)
  }

  #datacubepath check
  #1. make sure anything is defined
  if(is.null(datacubepath) && is.null(scenario) && is.null(metric)){
    stop('You need to define the datacubepath or the scenario and metric.
       Regarding the second option: If your EBV netCDF has no scenario,
       leave the argument empty.')
  }else if(!is.null(datacubepath)){
    #2. check datacubepath
    # open file
    hdf <- rhdf5::H5Fopen(filepath_nc, flags = "H5F_ACC_RDONLY")
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
    datacubepaths <- ebv_datacubepaths(filepath_nc, verbose)
    datacubepath <- ebv_i_datacubepath(scenario, metric,
                                       datacubepaths, verbose)
  }

  # get properties
  prop <- ebv_properties(filepath_nc, datacubepath, verbose=FALSE)
  dims <- prop@spatial$dimensions
  entity_names <- prop@general$entity_names

  #check file structure
  is_4D <- ebv_i_4D(filepath_nc)
  if(is_4D){
    if(is.null(entity)){
      stop('Your working with a 4D cube based EBV netCDF. Please specify the entity-argument.')
    }
    #check entity ----
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

  #timestep check -> in case of ISO, get index
  timestep <- ebv_i_date(timestep, prop@temporal$dates)

  #check if band is valid type
  if(checkmate::checkIntegerish(band) != TRUE){
    stop('Band has to be an integer or a list of integers.')
  }

  #check if timesteps and bands have the same length
  if(character){
    if (length(band) != length(timestep)){
      stop('The amount of bands to read from Tiff and the amount of timesteps to write to NetCDF differ. Have to be the same.')
    }
  }

  #check if timesteps and tif have the same length/amount of bands
  if(character){
    tif_info <- terra::describe(data)
    b.count <- stringr::str_count(tif_info, 'Band')
    b.sum <- sum(b.count, na.rm=TRUE)
    if (b.sum < length(timestep)){
      stop('The amount of timesteps to write to the netCDF is longer than the available bands in Tiff.')
    }
    #check if band available in Tif
    if (max(band) > b.sum){
      stop('The highest band that should be used exceeds the amount of bands in GeoTiff File.')
    }
  }else if (matrix || array){
    if(matrix && length(timestep)>1){
      stop('You are trying to write several timesteps to the netCDF even though you handed over data in a matrix and one timestep only can be filled.')
    }else if (array){
      if(dim(data)[3]< length(timestep)){
        stop('The amount of timesteps to write to the netCDF is longer than the available layers in your data array.')
      }
    }
  }

  #check needed RAM to read tif info
  if(character){
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
    #check RAM
    if (!ignore_RAM){
      ebv_i_check_ram(size.int,timestep,entity,type.long)
    } else{
      if(verbose){
        print('RAM capacities are ignored.')
      }
    }

  } else if(matrix || array){
    #DOING NOTHING - ELEMENT ALREADY IN MEMORY
    # if(is.double(data)){
    #   type.long <- 'double'
    # }else{
    #   type.long <-'integer'
    # }
    size.int <- dim(data)
  }



  #check if dims of tif data correspond to lat and lon in netcdf
  lat.len <- dims[1]
  lon.len <- dims[2]
  if ((size.int[1] != lon.len) && (size.int[2] != lat.len)){
    stop(paste0('The size of your GeoTiff/data does not correspond to the latitude and longitude coordinates.
  Size should be: ', lon.len, ', ', lat.len, '. But is: ', size.int[1], ', ', size.int[2]))
  }
  if (size.int[1] != lon.len) {
    stop(paste0('The size of your GeoTiff/data doesn not match the longitudinal coordinates.
  Size sould be: ', lon.len, '. But size is: ', size.int[1]))
  } else if (size.int[2] != lat.len){
    stop(paste0('The size of your GeoTiff/data doesn not match the longitudinal coordinates.
  Size sould be: ', lat.len, '. But size is: ', size.int[2]))
  }

  ### end initial test ----

  #get data from tif ----
  if(character){

    #open tif file, get raster data
    raster <- terra::rast(data)[[band]]

    #transform into array/matrix----
    if (length(timestep) > 1){
      data <- array(raster, dim=c(dim(raster)[2], dim(raster)[1], dim(raster)[3]))
    } else {
      data <- matrix(raster, nrow=dim(raster)[2], ncol=dim(raster)[1])
    }
  }

  #open file
  hdf <- rhdf5::H5Fopen(filepath_nc)

  #set dim of dataset ----
  did <- rhdf5::H5Dopen(hdf, datacubepath)
  file_space <- rhdf5::H5Dget_space(did)
  if (rhdf5::H5Sget_simple_extent_dims(file_space)$size[3] != dims[3]){
    #set new dimension of dataset
    rhdf5::H5Dset_extent(did, c(dims[2],dims[1],dims[3:length(dims)]))
    rhdf5::H5Dclose(did)
    rhdf5::H5Sclose(file_space)
  }else{
    rhdf5::H5Dclose(did)
    rhdf5::H5Sclose(file_space)
  }


  #think about replacing NaN with NA -> weird errors -> check again if this bug is real!
  data[is.nan(data)] <- NA

  #write data ----

  if(is_4D){
    #write data 4D
    rhdf5::h5write(data, hdf, datacubepath, start=c(1,1,min(timestep),entity_index),
                   count=c(lon.len,lat.len,length(timestep),length(entity)))
  } else{
    #write data 3D
    rhdf5::h5write(data, hdf, datacubepath, start=c(1,1,min(timestep)),
                   count=c(lon.len,lat.len,length(timestep)))
  }

  #open DS
  did <- rhdf5::H5Dopen(hdf, datacubepath)

  #delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}
