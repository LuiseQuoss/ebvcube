#' Write a new attribute value to an EBV NetCDF
#'
#' @description Write a new attribute value to an EBV NetCDF. Not all attributes
#'   can be changed. Some are always created automatically, e.g. the crs, time
#'   and var_entity datasets. In this case you have to re-create the NetCDF
#'   file.
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param attribute_name Character. Name of the attribute that should be
#'   changed.
#' @param value New value that should be assigned to the attribute.
#' @param levelpath Character. Default: NULL. Indicates the location of the
#'   attribute. The default means that the attribute is located at a global
#'   level. If the attribute is located at the datacubelevel just add the
#'   datacubepath. For the metric level the value may be 'metric00' or
#'   'scenario00/metric00'. This path depends on whether the NetCDF hierarchy
#'   has scenarios or not.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Adds the new value to the attribute. Check your results using
#'   [ebvnetcdf::ebv_properties()].
#' @export
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_new.nc"), package="ebvnetcdf")
#' attribute1 <- 'standard_name'
#' value1 <- 'mammals'
#' level1 <- 'metric00'
#' ebv_ncdf_write_attribute(file, attribute1, value1, level1)
#' attribute2 <- '_FillValue'
#' value2 <- -999
#' level2 <- 'metric00/entity00'
#' ebv_ncdf_write_attribute(file, attribute2, value2, level2)
#' attribute3 <- 'creator'
#' value3 <- 'Jane Doe'
#' ebv_ncdf_write_attribute(file, attribute3, value3)
ebv_ncdf_write_attribute <- function(filepath, attribute_name, value, levelpath=NULL, verbose=FALSE){
  #start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  withr::defer(
    if (exists('h5obj')){
      if(rhdf5::H5Iis_valid(h5obj)==TRUE){
        tryCatch(
          {
            rhdf5::H5Gclose(h5obj)
          },
          error = function(e){
            tryCatch(
              {
                rhdf5::H5Dclose(h5obj)
              },
              error = function(e){
                rhdf5::H5Fclose(h5obj)
              }
            )
          }
        )
      }
    }
  )

  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(attribute_name)){
    stop('Attribute_name argument is missing.')
  }
  if(missing(value)){
    stop('Value argument is missing.')
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

  #check filepath
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('NetCDF filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
    stop(paste0('NetCDF file does not exist.\n', filepath))
  }
  if (!endsWith(filepath, '.nc')){
    stop(paste0('NetCDF file ending is wrong. File cannot be processed.'))
  }

  #file closed?
  ebv_i_file_opened(filepath)

  #open file
  hdf <- rhdf5::H5Fopen(filepath)

  #check if levelpath exists
  if (!is.null(levelpath)){
    if (rhdf5::H5Lexists(hdf, levelpath)==FALSE){
      stop(paste0('The given levelpath is not valid:\n', levelpath))
    }
  }

  #end initial tests ----

  #set (block) list ----
  att.num <- c('_FillValue')
  att.int <- c('least_significant_digit')
  att.chr <- c('standard_name', 'description', 'units', 'long_name', 'label', 'title', 'creator',
               'institution', 'contactname', 'contactemail', 'ebv_class', 'ebv_name')
  att.blocked <- c('axis', 'calendar', 'grid_mapping', '_ChunkSizes', 'value_range',
                   'Conventions', 'ebv_subgroups', 't_delta', 'type')

  #check block list ----
  if(! is.null(levelpath)){
    if(mapply(grepl,'crs',levelpath,ignore.case=TRUE)){
      stop('Changes in CRS are blocked! Rebuild NetCDF if you want a different CRS definition.')
    } else if(mapply(grepl,'lat',levelpath,ignore.case=TRUE)){
      stop('Changes for the latitude dataset is blocked! Rebuild NetCDF if you want a different latitude definition.')
    }else if(mapply(grepl,'lon',levelpath,ignore.case=TRUE)){
      stop('Changes for the longitude dataset is blocked! Rebuild NetCDF if you want a different longitude definition.')
    }else if(mapply(grepl,'var_entity',levelpath,ignore.case=TRUE)){
      stop('Changes for the var_entity dataset is blocked! Always built automatically.')
    }else if(mapply(grepl,'time',levelpath,ignore.case=TRUE)){
      stop('Changes for the time dataset is blocked! Rebuild NetCDF if you want a different time definition.')
    }
  }
  if(attribute_name %in% att.blocked){
    stop(paste0('Changes for the attribute ', attribute_name, ' are blocked! Always built automatically.' ))
  }

  #extra check for ebv_class and ebv_name ----
  #get current ebv_name and ebv_class
  ebv.class <- ebv_i_read_att(hdf, 'ebv_class')
  ebv.name <- ebv_i_read_att(hdf, 'ebv_name')
  rhdf5::H5Fclose(hdf)
  ebv.classes <- c('Genetic composition', 'Species populations', 'Species traits', 'Community composition',
                   'Ecosystem functioning', 'Ecosystem structure', 'Ecosystem services')
  if(attribute_name=='ebv_class'){
    #check if new ebv.class value is valid
    if(! value %in% ebv.classes){
      stop('You are trying to change the ebv_class to a value that is not possible.')
    }
    #get ebv_names for new ebv_class
    ebv.class <- value
    if(ebv.class == ebv.classes[1]){
      ebv.names <- c('Intraspecific genetic diversity', 'Genetic differentiation',
                     'Effective population size', 'Inbreeding')
    } else if(ebv.class == ebv.classes[2]){
      ebv.names <- c('Species distributions', 'Species abundances')
    }else if(ebv.class == ebv.classes[3]){
      ebv.names <- c('Morphology', 'Physiology', 'Phenology', 'Movement')
    }else if(ebv.class == ebv.classes[4]){
      ebv.names <- c('Community abundance', 'Taxonomic and phylogenetic diversity', 'Trait diversity', 'Interaction diversity')
    }else if(ebv.class == ebv.classes[5]){
      ebv.names <- c('Primary productivity', 'Ecosystem phenology', 'Ecosystem disturbances')
    }else if(ebv.class == ebv.classes[6]){
      ebv.names <- c('Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile')
    }else if(ebv.class == ebv.classes[7]){
      ebv.names <- c('Pollination')
    }
    #check if ebv_name corresponds - else warning: also change ebv_name!
    if(attribute_name=='ebv_class' & ! ebv.name %in% ebv.names){
      warning(paste0('The current ebv_name ', ebv.name, ' does not correspond the new ebv_class ', ebv.class,  '. Possible ebv_name values: ', paste(ebv.names, collapse = ', '), '. Change ebv_name!'))
    }
  } else if (attribute_name=='ebv_name'){
    #get ebv_names
    if(ebv.class == ebv.classes[1]){
      ebv.names <- c('Intraspecific genetic diversity', 'Genetic differentiation',
                     'Effective population size', 'Inbreeding')
    } else if(ebv.class == ebv.classes[2]){
      ebv.names <- c('Species distributions', 'Species abundances')
    }else if(ebv.class == ebv.classes[3]){
      ebv.names <- c('Morphology', 'Physiology', 'Phenology', 'Movement')
    }else if(ebv.class == ebv.classes[4]){
      ebv.names <- c('Community abundance', 'Taxonomic and phylogenetic diversity', 'Trait diversity', 'Interaction diversity')
    }else if(ebv.class == ebv.classes[5]){
      ebv.names <- c('Primary productivity', 'Ecosystem phenology', 'Ecosystem disturbances')
    }else if(ebv.class == ebv.classes[6]){
      ebv.names <- c('Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile')
    }else if(ebv.class == ebv.classes[7]){
      ebv.names <- c('Pollination')
    }else{
      warning('The ebv_class seems to be wrong. No check of ebv_name. The value you add may be incorrect.')
      #add all names
      ebv.names <- c('Intraspecific genetic diversity', 'Genetic differentiation', 'Effective population size',
                     'Inbreeding', 'Species distributions', 'Species abundances', 'Morphology', 'Physiology',
                     'Phenology', 'Movement', 'Community abundance', 'Taxonomic and phylogenetic diversity',
                     'Trait diversity', 'Interaction diversity', 'Primary productivity', 'Ecosystem phenology',
                     'Ecosystem disturbances', 'Live cover fraction', 'Ecosystem distribution', 'Ecosystem Vertical Profile',
                     'Pollination')
    }
    #check of ebv_name
    if(! value %in% ebv.names){
      stop(paste0('You are trying to change the ebv_name to a value that is not possible for ebv_class ', ebv.class, '. If both values are to be changed, change ebv_class first.'))
    }
  }

  #open h5object ----
  if (! is.null(levelpath)){
    hdf <- rhdf5::H5Fopen(filepath)
    h5obj <- tryCatch(
      {
        h5obj <- rhdf5::H5Gopen(hdf, levelpath)
      },
      error = function(e){
        h5obj <- rhdf5::H5Dopen(hdf, levelpath)
      }
    )
  } else {
    h5obj <- rhdf5::H5Fopen(filepath)
  }

  #check if attribute exists - written correct? ----
  if (! rhdf5::H5Aexists(h5obj, attribute_name)){
    if (attribute_name %in% att.num | attribute_name %in% att.int | attribute_name %in% att.chr){
      stop('Attribute does not exist within given levelpath in NetCDF. Change your levelpath!')
    } else {
      stop('Attribute is written incorrectly or does not exist in NetCDF')
    }
  }

  #read attribute, change if different ----
  att <- ebv_i_read_att(h5obj, attribute_name)
  if(att==value){
    stop(paste0('Value of ', attribute_name, ' already is set to "', value, '".'))
  } else {
    if(attribute_name %in% att.num){
      if(! is.na(as.numeric(value))){
        ebv_i_num_att(h5obj, attribute_name, value)
      } else{
        stop(paste0('The attribute ', attribute_name, ' needs to be a double value.'))
      }
    } else if (attribute_name %in% att.chr){
      ebv_i_char_att(h5obj, attribute_name, value)
    } else if (attribute_name %in% att.int){
      if(! is.na(as.numeric(value))){
        ebv_i_int_att(h5obj, attribute_name, value)
      } else{
        stop(paste0('The attribute ', attribute_name, ' needs to be an integer value.'))
      }
    }
  }

  #close handles ----
  if (exists('h5obj')){
    tryCatch(
      {
        rhdf5::H5Gclose(h5obj)
      },
      error = function(e){
        tryCatch(
          {
              rhdf5::H5Dclose(h5obj)
          },
          error = function(e){
            rhdf5::H5Fclose(h5obj)
          }
        )
      }
    )
  }

  if(exists('hdf')){
    if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
  }

}
