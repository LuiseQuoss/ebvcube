#' Add entity attributes to EBV NetCDF
#'
#' @description Add long_name, label, units and fillvalue to an entitiy of a self-created EBV NetCDF. First use [ebvnetcdf::ebv_ncdf_create()] to create a NetCDF afterwards use this functiong to add attributes.
#'
#' @param filepath Path to the self-created NetCDF file.
#' @param datacubepath Path to the datacube (use [ebvnetcdf::ebv_datacubepaths()]).
#' @param long_name Value of the long_name attribute (character).
#' @param label Value of the label attribute (character).
#' @param units Value of the units attribute (character).
#' @param fillvalue Value of the fillvalue attribute (double).
#'
#' @return Adds attributes to NetCDF. Check results using [ebvnetcdf::ebv_properties()]
#' @export
#'
#' @examples
#' # file <- 'path/to/self/created/netcdf.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' #ebv_ncdf_entity_attributes(file, datacubes[1,1], 'European habitat suitability maps', 'Raised bog', 'Percentage', fillvalue=999)
ebv_ncdf_entity_attributes <- function(filepath, datacubepath, long_name, label, units, fillvalue=NULL){
  ### start initial test ----
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

  #check if nc file exists
  if (!file.exists(filepath)){
    stop(paste0('NetCDF file does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
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

  #check if attribute arguments have right type
  if(!HDF5Array::type(long_name)=='character'){
    stop('Long_name argument has to be of type character.')
  }
  if(!HDF5Array::type(label)=='character'){
    stop('Label argument has to be of type character.')
  }
  if(!HDF5Array::type(units)=='character'){
    stop('Units argument has to be of type character.')
  }
  if(!is.null(fillvalue)){
    if(!HDF5Array::type(fillvalue)=='double'){
      stop('Fillvalue argument has to be of type double')
    }
  }

  ### end initial test ----

  #open variables
  hdf <- rhdf5::H5Fopen(filepath)
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

