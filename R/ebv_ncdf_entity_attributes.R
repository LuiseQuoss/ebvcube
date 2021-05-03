#' Add entity attributes to self-created EBV NetCDF
#'
#' @description Add long_name, label, units and fillvalue to an entitiy of a
#'   self-created EBV NetCDF. First use [ebvnetcdf::ebv_ncdf_create()] to create
#'   a NetCDF afterwards use this functiong to add attributes.
#'
#' @param filepath Path to the self-created NetCDF file.
#' @param datacubepath Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param long_name Value of the long_name attribute (character).
#' @param label Value of the label attribute (character).
#' @param units Value of the units attribute (character).
#' @param fillvalue Value of the fillvalue attribute (double).
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return Adds attributes to NetCDF. Check results using
#'   [ebvnetcdf::ebv_properties()]
#' @export
#'
#' @examples
#' # file <- 'path/to/self/created/netcdf.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # ebv_ncdf_entity_attributes(file, datacubes[1,1], 'habitat', 'bog', 'Percentage', fillvalue=-1)
ebv_ncdf_entity_attributes <- function(filepath, datacubepath, long_name, label, units, fillvalue=NULL, verbose=FALSE){
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
  if(missing(long_name)){
    stop('Long_name argument is missing.')
  }
  if(missing(label)){
    stop('Label argument is missing.')
  }
  if(missing(units)){
    stop('Units argument is missing.')
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

  if(checkmate::checkCharacter(long_name)!=TRUE){
    stop('Long_name argument has to be of class character.')
  }
  if(checkmate::checkCharacter(label)!=TRUE){
    stop('Label argument has to be of class character.')
  }
  if(checkmate::checkCharacter(units)!=TRUE){
    stop('Units argument has to be of class character.')
  }
  if(!is.null(fillvalue)){
    if(checkmate::checkNumber(fillvalue)!=TRUE & !is.na(fillvalue)){
      stop('The fillvalue needs to be a single numeric value or NA.')
    }
  }

  ### end initial test ----

  #open variables
  did <- rhdf5::H5Dopen(hdf, datacubepath)

  # :long_name = "Changes in local bird diversity (cSAR)";
  ebv_i_char_att(did, 'long_name', long_name)
  #is long_name the title? what is it?

  #attributes that are filled by user - created empty
  # :label = "forest bird species";
  ebv_i_char_att(did, 'label', label)

  # :standard_name = "Changes in local bird diversity (cSAR): forest bird species";
  standard_name <- paste0(long_name, ': ', label)
  ebv_i_char_att(did, 'standard_name', standard_name)
  #longname: label

  # :units = "mean change of species diversity per area (pixel size) to baseline 1900";
  ebv_i_char_att(did, 'units', units)
  #name of metric?

  #optional
  # :_FillValue = -3.4E38f; // float
  if(!is.null(fillvalue)) {
    ebv_i_num_att(did, '_FillValue', fillvalue)
  }

  ###ALSO?
  # # :least_significant_digit = 4; // int
  # add_int_attribute(did, 'least_significant_digit', 999)
  # :description = "default";
  # ebv_i_char_att(did, 'description', 'default')


  #double check: delete automatically created attribute: :rhdf5-NA.OK
  if(rhdf5::H5Aexists(did, 'rhdf5-NA.OK')){
    rhdf5::H5Adelete(did, 'rhdf5-NA.OK')
  }

  #close DS
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(hdf)

}

