#' Add datacube attributes to an EBV NetCDF
#'
#' @description Add standard_name, description and fillvalue to an
#'   entitiy/datacube of a self-created EBV NetCDF. First use
#'   [ebvnetcdf::ebv_ncdf_create()] to create a NetCDF afterwards use this
#'   function to add attributes.
#'
#' @param filepath Character. Path to the self-created NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param description Character. Value of the description attribute.
#' @param standard_name Value of the standard_name attribute.
#' @param fillvalue Numeric. Value of the fillvalue attribute.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Adds attributes to NetCDF at datacube level. Check results using
#'   [ebvnetcdf::ebv_properties()]
#' @export
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_new.nc"), package="ebvnetcdf")
#' # datacubes <- ebv_datacubepaths(file)
#' sn <- 'non forest birds species'
#' desc <- 'Changes in bird diversity at the grid cell level caused by land-use'
#' fv <- -3.4E38
#' #ebv_ncdf_entity_attributes(file, datacubes[1,1], sn, desc, fv)
ebv_ncdf_entity_attributes <- function(filepath, datacubepath, standard_name,
                                       description, fillvalue=NULL, verbose=FALSE){
  ### start initial test ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  withr::defer(
    if(exists('did')){
      if(rhdf5::H5Iis_valid(did)==TRUE){rhdf5::H5Dclose(did)}
    }
  )

  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath_nc argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }
  if(missing(description)){
    stop('description argument is missing.')
  }
  if(missing(standard_name)){
    stop('standard_name argument is missing.')
  }

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #check if nc file exists
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
    stop(paste0('File does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath)

  #variable check
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given variable is not valid:\n', datacubepath))
  }

  #check if attribute arguments have right class

  if(checkmate::checkCharacter(description)!=TRUE){
    stop('description argument has to be of class character.')
  }
  if(checkmate::checkCharacter(standard_name)!=TRUE){
    stop('standard_name argument has to be of class character.')
  }
  if(!is.null(fillvalue)){
    if(checkmate::checkNumber(fillvalue)!=TRUE & !is.na(fillvalue)){
      stop('The fillvalue needs to be a single numeric value or NA.')
    }
  }

  ### end initial test ----

  #open variable  ----
  did <- rhdf5::H5Dopen(hdf, datacubepath)

  # :description ----
  ebv_i_char_att(did, 'description', description)

  # :standard_name ----
  ebv_i_char_att(did, 'standard_name', standard_name)

  #optional
  # :_FillValue ----
  if(!is.null(fillvalue)) {
    ebv_i_num_att(did, '_FillValue', fillvalue)
  }

  # # :least_significant_digit = 4; // int
  # add_int_attribute(did, 'least_significant_digit', 999)

  #double check: delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close DS ----
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}

