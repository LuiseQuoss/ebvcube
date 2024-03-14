#' Get a simple explorative analysis of an EBV netCDF datacube
#'
#' @description Get basic measurements of the data, including min, max, mean,
#'   sd, n, #NAs, q25, q50, q75 (no mean for categorical data).
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Optional. Default: NULL. Path to the datacube
#'   (use [ebvcube::ebv_datacubepaths()]). Alternatively, you can use the
#'   scenario and metric argument to define which cube you want to access.
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param subset Optional if you want measurements on a smaller subset. Possible
#'   via the path to a shapefile (character) or the indication of a bounding box
#'   (vector of four numeric values) defining the subset. Else the whole area is
#'   analysed.
#' @param timestep Integer or character. Select one or several timestep(s).
#'   Either provide an integer value or list of values that refer(s) to the
#'   index of the timestep(s) (minimum value: 1) or provide a date or list of
#'   dates in ISO format, such as '2015-01-01'.
#' @param touches Logical. Optional. Default: TRUE. Only relevant if the subset
#'   is indicated by a shapefile. See [ebvcube::ebv_read_shp()].
#' @param epsg Numeric. Optional. Only relevant if the subset is indicated by a
#'   bounding box and the coordinate reference system differs from WGS84. See
#'   [ebvcube::ebv_read_bb()].
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
#' @param numerical Logical. Default: TRUE. Change to FALSE if the data covered
#'   by the netCDF contains categorical data.
#' @param na_rm Logical. Default: TRUE. NA values are removed in the analysis.
#'   Change to FALSE to include NAs.
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Returns a named list containing the measurements.
#' @export
#' @seealso [ebvcube::ebv_read_bb()] and [ebvcube::ebv_read_shp()] for the usage
#'   of subsets.
#'
#' @importFrom stats quantile
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file, verbose=FALSE)
#' #set path to shp file
#' shp_path <- system.file(file.path("extdata","cameroon.shp"), package="ebvcube")
#'
#' \donttest{
#' #get measurements for full extent and the first three timesteps
#' data_global <- ebv_analyse(filepath = file, datacubepath = datacubes[1,1],
#'                            entity = 1, timestep = 1:3, verbose = FALSE)
#'
#' #get measurements for subset of Africa only (using bounding box) and one timestep
#' data_1910 <- ebv_analyse(filepath = file, datacubepath = datacubes[1,1],
#'                          entity = 1, timestep = "1900-01-01",
#'                          subset = c(-26, 64, 30, 38), verbose = FALSE)
#'
#' #get measurements for cameroon only (using shp) and one timestep
#' data_1930 <- ebv_analyse(filepath = file, entity = 1,
#'                          timestep = "1930-01-01",
#'                          subset = shp_path, verbose = FALSE,
#'                          metric = 'Absolute change in the number of species',)
#' }

ebv_analyse <- function(filepath, datacubepath = NULL, entity=NULL, timestep=1,
                        subset=NULL, touches=TRUE, epsg = 4326,
                        scenario = NULL, metric = NULL,
                        numerical=TRUE, na_rm=TRUE, verbose=TRUE){
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

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #filepath check - nc
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('File ending is wrong. File cannot be processed.'))
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
  prop <- ebv_properties(filepath, datacubepath, verbose=verbose)

  #timestep check -> in case of ISO, get index
  timestep <- ebv_i_date(timestep, prop@temporal$dates)

  #check logical arguments
  if(checkmate::checkLogical(na_rm, len=1, any.missing=FALSE) != TRUE){
    stop('na_rm must be of type logical.')
  }
  if(checkmate::checkLogical(numerical, len=1, any.missing=FALSE) != TRUE){
    stop('numerical must be of type logical.')
  }
  if(checkmate::checkLogical(touches, len=1, any.missing=FALSE) != TRUE){
    stop('touches must be of type logical.')
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

  #more checks are included in subset bb and subset shp

  ####initial tests end ----

    #process global scale ----
  if (is.null(subset)){
    #process whole file + variable+ timestep
    type.short <- ebv_i_type_r(prop@ebv_cube$type)
    all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath, as.sparse = TRUE, type = type.short)
    if(is_4D){
      #process 4D
      subset.array <- all[,,timestep,entity_index]
    } else{
      #process 3D
      subset.array <- all[,,timestep]
    }
    #give fillvalue as nodata value
    subset.array <- replace(subset.array, subset.array==prop@ebv_cube$fillvalue[1], c(NA))
  }  else if(methods::is(subset, 'numeric')){
    #process bb subset ----
    subset.raster <- ebv_read_bb(filepath, datacubepath, entity=entity, bb=subset,
                                 timestep=timestep, epsg=epsg, verbose=verbose)
    #raster to array
    subset.array <- terra::as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else if(endsWith(subset, '.shp')){
    #process shp subset ----
    subset.raster <- ebv_read_shp(filepath, datacubepath, entity=entity,
                                  shp=subset, timestep=timestep, touches=touches,
                                  verbose=verbose)
    #raster to array
    subset.array <- terra::as.array(subset.raster)
    #less ram
    rm(subset.raster)
  } else {
    stop('Not processed. Check your given subset-indication.')
  }

  if(numerical){
    #numerical stats ----
    n <- length(subset.array)
    temp <- as.numeric(summary(array(subset.array)))
    sd <- sd(subset.array, na.rm=na_rm)
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], mean =temp[4], q75 =temp[5], max=temp[6], std =sd, n =n, NAs =temp[7])
  }else{
    #categorical stats ----
    n <- length(subset.array)
    temp <- as.numeric(stats::quantile(subset.array, na.rm=na_rm))
    NAs <- sum(is.na(subset.array))
    stats <- list(min=temp[1], q25 = temp[2], q50=temp[3], q75 =temp[4], max=temp[5], n =n, NAs =NAs)
  }

  return(stats)

}
