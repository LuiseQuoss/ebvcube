#' Write a new attribute value to an EBV netCDF
#'
#' @description Write a new attribute value to an EBV netCDF. Not all attributes
#'   can be changed. Some are always created automatically, e.g. the attributes
#'   belonging to the crs, time and var_entity datasets. In this case you have
#'   to re-create the netCDF file.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param attribute_name Character. Name of the attribute that should be
#'   changed.
#' @param value New value that should be assigned to the attribute.
#' @param levelpath Character. Default: NULL. Indicates the location of the
#'   attribute. The default means that the attribute is located at a global
#'   level. If the attribute is located at the datacubelevel just add the
#'   datacubepath, e.g. metric_1/ebv_cube. For the metric level the value may be 'metric_1' or
#'   'scenario_1/metric_1'. This path depends on whether the netCDF hierarchy
#'   has scenarios or not.
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Adds the new value to the attribute. Check your results using
#'   [ebvcube::ebv_properties()].
#' @export
#'
#' @note You can change the ebv_class and the ebv_name. In this case you need to
#'   change the ebv_class first. Don't forget to change the ebv_name accordingly!
#'
#' @examples
#' #set path to EBV netCDF file <-
#' system.file(file.path("extdata","baisero_spepop_id5_20220405_v1_empty.nc"),
#' package="ebvcube")
#'
#' \dontrun{
#' try({
#' #change the standard_name of the metric
#' attribute1 <- 'standard_name'
#' value1 <- 'habitat availability'
#' level1 <- 'scenario_1/metric_1'
#' ebv_attribute(filepath = file, attribute_name = attribute1,
#'               value = value1, level = level1)
#'
#' #change the units of the ebv_cube
#' attribute2 <- 'units'
#' value2 <- 'Land-use of 5,090 mammals calculated in sqkm'
#' level2 <- 'scenario_1/metric_1/ebv_cube' #equal to the datacubepath
#' ebv_attribute(filepath = file, attribute_name = attribute2,
#'               value = value2, level = level2)
#'
#' #change the name of the creator at the global level
#' attribute3 <- 'creator_name'
#' value3 <- 'Jane Doe'
#' ebv_attribute(filepath = file, attribute_name = attribute3,
#'               value = value3)
#' }, TRUE)
#' }
ebv_attribute <- function(filepath, attribute_name, value,
                          levelpath=NULL, verbose=TRUE){
  #start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  withr::defer(
    if(exists('h5obj')){
      if(is.null(levelpath)){
        if(rhdf5::H5Iis_valid(h5obj)==TRUE){rhdf5::H5Fclose(h5obj)}
      } else if(stringr::str_detect(levelpath, 'ebv_cube')){
        if(rhdf5::H5Iis_valid(h5obj)==TRUE){rhdf5::H5Dclose(h5obj)}
      } else{
        if(rhdf5::H5Iis_valid(h5obj)==TRUE){rhdf5::H5Gclose(h5obj)}
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
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
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
  # ebv_i_file_opened(filepath, verbose)

  #get datacubes
  datacubes <- ebv_datacubepaths(filepath, verbose=verbose)

  #open file
  hdf <- rhdf5::H5Fopen(filepath)

  #check if levelpath exists
  if (!is.null(levelpath)){
    if (rhdf5::H5Lexists(hdf, levelpath)==FALSE){
      stop(paste0('The given levelpath is not valid:\n', levelpath))
    }
  }

  # check if attribute_name is string
  if (checkmate::check_string(attribute_name) != TRUE){
    stop('attribute_name must be of type character')
  }

  # check if value is string
  if (checkmate::check_string(value) != TRUE){
    stop('value must be of type character')
  }

  #end initial tests ----

  #set white list ----
  att.chr <- c(#global
    'history', 'keywords', 'title', 'summary', 'references', 'source',
    'project', 'date_created', 'creator_name', 'creator_institution',
    'creator_email','license','contributor_name','publisher_name',
    'publisher_institution','publisher_email','comment',
    'ebv_class', 'ebv_name','ebv_spatial_scope', 'ebv_spatial_description',
    'ebv_domain', 'ebv_scenario_classification_name', 'processing_level',
    #scenario and metric
    'standard_name', 'long_name',
    #metric and ebv_cube
    'units',
    #ebv_cube
    'coverage_content_type')

  # set and evaluate black list ----
  att.blocked <- c(
    #global
    'id', 'naming_authority', 'Conventions', 'date_issued',
    #ebv_cube
    '_FillValue', 'grid_mapping','coordinates','_ChunkSizes')

  if(attribute_name %in% att.blocked){
    stop(paste0('Changes for the attribute ', attribute_name, ' are blocked! Always built automatically.'))
  }


  # block entirely: crs, entity, lat, lon, time ----
  #check block list
  if(! is.null(levelpath)){
    if(mapply(grepl,'crs',levelpath,ignore.case=TRUE)){
      stop('Changes for the CRS are blocked! Rebuild netCDF if you want a different CRS definition.')
    } else if(mapply(grepl,'lat',levelpath,ignore.case=TRUE)){
      stop('Changes for the latitude dataset are blocked! Rebuild netCDF if you want a different latitude definition.')
    }else if(mapply(grepl,'lon',levelpath,ignore.case=TRUE)){
      stop('Changes for the longitude dataset are blocked! Rebuild netCDF if you want a different longitude definition.')
    }else if(mapply(grepl,'entity',levelpath,ignore.case=TRUE)){
      stop('Changes for the entity dataset are blocked! Always built automatically.')
    }else if(mapply(grepl,'time',levelpath,ignore.case=TRUE)){
      stop('Changes for the time dataset are blocked! Rebuild netCDF if you want a different time definition.')
    }
    if(stringr::str_detect(levelpath,'scenario') && stringr::str_detect(levelpath,'metric')){
      scenario_exist <- TRUE
    }else{
      scenario_exist <- FALSE
    }
    if (stringr::str_detect(levelpath,'metric')){
      metric_exists <- TRUE
    }else{
      metric_exists <- FALSE
    }
  }else{
    scenario_exist <- FALSE
    metric_exists <- FALSE
  }

  #extra check for ebv_class and ebv_name ----
  #get current ebv_name and ebv_class
  ebv.class <- ebv_i_read_att(hdf, 'ebv_class', verbose)
  ebv.name <- ebv_i_read_att(hdf, 'ebv_name', verbose)
  #rhdf5::H5Fclose(hdf)
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
    if(attribute_name=='ebv_class' && ! ebv.name %in% ebv.names){
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

  #extra check for ebv_domain ----
  if (attribute_name == 'ebv_domain'){
    if(! value %in% c('Terrestrial', 'Marine', 'Freshwater')){
      stop(paste0('The ebv_domain can only have one of the following values: ',
                  'Terrestrial, Marine or Freshwater'))
    }
  }

  #open h5object ----
  if(is.null(levelpath)){
    if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    h5obj <- rhdf5::H5Fopen(filepath)
  } else if(stringr::str_detect(levelpath, 'ebv_cube')){
    h5obj <- rhdf5::H5Dopen(hdf, levelpath)
  } else{
    h5obj <- rhdf5::H5Gopen(hdf, levelpath)
  }

  #check if attribute exists - written correct? ----
  if (! rhdf5::H5Aexists(h5obj, attribute_name)){
    if (attribute_name %in% att.chr){
      stop('Attribute does not exist within given levelpath in netCDF. Change your levelpath!')
    } else {
      stop(paste0('Attribute is written incorrectly or does not exist in netCDF.',
                  ' Available attributes: ', paste(att.chr, collapse=', ')))
    }
  }

  #if file has scenarios and a metric or ebv_cube attribute is changed -> change in all scenarios
  #CASE: FILE HAS SCENARIOS----
  if(scenario_exist){
    #close current handle
    if(is.null(levelpath)){
      rhdf5::H5Fclose(h5obj)
    } else if(stringr::str_detect(levelpath, 'ebv_cube')){
      rhdf5::H5Dclose(h5obj)
    } else{
      rhdf5::H5Gclose(h5obj)
    }
    #get all scenarios----
    parts <- unique(unlist(stringr::str_split(datacubes[,1], '/')))
    index <- which(stringr::str_detect(parts, 'scenario'))
    scenarios <- parts[index]
    #if more than 1 scenario, inform user
    if(length(scenarios)>1){
      if(verbose){
        message('You are changing an attribute that is repeated over the different scenarios in your file. All of them will be changed.')
      }
    }
    #change attribute in all scenarios ----
    base <- paste0(stringr::str_split(levelpath, '/')[[1]][c(-1)], collapse = '/')
    for(scenario in scenarios){
      path <- paste0(scenario, '/', base)
      #open path
      if(stringr::str_detect(path, 'ebv_cube')){
        h5obj <- rhdf5::H5Dopen(hdf, path)
      } else{
        h5obj <- rhdf5::H5Gopen(hdf, path)
      }
      #read att, change if different
      att <- ebv_i_read_att(h5obj, attribute_name, verbose)
      if(att==value){
        message(paste0('Value of ', attribute_name, 'in path ', path ,' already is set to "', value, '".'))
      } else if (attribute_name %in% att.chr){
        ebv_i_char_att(h5obj, attribute_name, value)
      }

      #close handle
      if(stringr::str_detect(path, 'ebv_cube')){
        rhdf5::H5Dclose(h5obj)
      } else{
        rhdf5::H5Gclose(h5obj)
      }
    }
  } else{
    #CASE: FILE HAS NO SCENARIOS----
    #read single attribute, change if different ----
    att <- ebv_i_read_att(h5obj, attribute_name, verbose)
    if(att==value){
      stop(paste0('Value of ', attribute_name, ' already is set to "', value, '".'))
    } else if (attribute_name %in% att.chr){
      ebv_i_char_att(h5obj, attribute_name, value)
    }
    #close handles ----
    if(is.null(levelpath)){
      rhdf5::H5Fclose(h5obj)
    } else if(stringr::str_detect(levelpath, 'ebv_cube')){
      rhdf5::H5Dclose(h5obj)
    } else{
      rhdf5::H5Gclose(h5obj)
    }

    path <- levelpath

  }

  #cover redundant attributes in ebv_cube and metric----
  if(scenario_exist){
    for(scenario in scenarios){
      path <- paste0(scenario, '/', base)
      if(metric_exists){
        if(attribute_name=='units'){
          #change path to corresponding other component
          if(stringr::str_detect(path, 'ebv_cube')){
            path <- stringr::str_remove(path, '/ebv_cube')
            h5obj <- rhdf5::H5Gopen(hdf, path)
          }else{
            path <- paste0(path, '/ebv_cube')
            h5obj <- rhdf5::H5Dopen(hdf, path)
          }
          #change attribute
          att <- ebv_i_read_att(h5obj, attribute_name, verbose)
          if(att==value){
            message(paste0('Value of ', attribute_name, 'in path ', path ,' already is set to "', value, '".'))
          } else if (attribute_name %in% att.chr){
            ebv_i_char_att(h5obj, attribute_name, value)
          }

        }else if(attribute_name=='long_name' && stringr::str_detect(path, 'ebv_cube')){
          #change path to corresponding other component
          path <- stringr::str_remove(path, '/ebv_cube')
          h5obj <- rhdf5::H5Gopen(hdf, path)

          #change corresponding attribute - standard_name
          att <- ebv_i_read_att(h5obj, 'standard_name', verbose)
          if(att==value){
            message(paste0('Value of standard_name in path ', path ,' already is set to "', value, '".'))
          } else if (attribute_name %in% att.chr){
            ebv_i_char_att(h5obj, 'standard_name', value)
          }

        }else if(attribute_name=='standard_name' && !stringr::str_detect(path, 'ebv_cube')){
          #change path to corresponding other component
          path <- paste0(path, '/ebv_cube')
          h5obj <- rhdf5::H5Dopen(hdf, path)

          #change corresponding attribute - long_name
          att <- ebv_i_read_att(h5obj, 'long_name', verbose)
          if(att==value){
            message(paste0('Value of long_name in path ', path ,' already is set to "', value, '".'))
          } else if (attribute_name %in% att.chr){
            ebv_i_char_att(h5obj, 'long_name', value)
          }

        }
        #close handle
        if(rhdf5::H5Iis_valid(h5obj)){
          if(stringr::str_detect(path, 'ebv_cube')){
            rhdf5::H5Dclose(h5obj)
          } else{
            rhdf5::H5Gclose(h5obj)
          }
        }
      }


      }
  }else{
    if(metric_exists){
      if(attribute_name=='units'){
        #change path to corresponding other component
        if(stringr::str_detect(path, 'ebv_cube')){
          path <- stringr::str_remove(path, '/ebv_cube')
          h5obj <- rhdf5::H5Gopen(hdf, path)
        }else{
          path <- paste0(path, '/ebv_cube')
          h5obj <- rhdf5::H5Dopen(hdf, path)
        }
        #change attribute
        att <- ebv_i_read_att(h5obj, attribute_name, verbose)
        if(att==value){
          message(paste0('Value of ', attribute_name, 'in path ', path ,' already is set to "', value, '".'))
        } else if (attribute_name %in% att.chr){
          ebv_i_char_att(h5obj, attribute_name, value)
        }

      }else if(attribute_name=='long_name' && stringr::str_detect(path, 'ebv_cube')){
        #change path to corresponding other component
        path <- stringr::str_remove(path, '/ebv_cube')
        h5obj <- rhdf5::H5Gopen(hdf, path)

        #change corresponding attribute - standard_name
        att <- ebv_i_read_att(h5obj, 'standard_name', verbose)
        if(att==value){
          message(paste0('Value of standard_name in path ', path ,' already is set to "', value, '".'))
        } else if (attribute_name %in% att.chr){
          ebv_i_char_att(h5obj, 'standard_name', value)
        }

      }else if(attribute_name=='standard_name' && !stringr::str_detect(path, 'ebv_cube')){
        #change path to corresponding other component
        path <- paste0(path, '/ebv_cube')
        h5obj <- rhdf5::H5Dopen(hdf, path)

        #change corresponding attribute - long_name
        att <- ebv_i_read_att(h5obj, 'long_name', verbose)
        if(att==value){
          message(paste0('Value of long_name in path ', path ,' already is set to "', value, '".'))
        } else if (attribute_name %in% att.chr){
          ebv_i_char_att(h5obj, 'long_name', value)
        }

      }
      #close handle
      if(rhdf5::H5Iis_valid(h5obj)){
        if(stringr::str_detect(path, 'ebv_cube')){
          rhdf5::H5Dclose(h5obj)
        } else{
          rhdf5::H5Gclose(h5obj)
        }
      }
    }
  }







  #check if any attribute inside keywords is changed -> change keywords ----
  if (attribute_name == 'ebv_class' || attribute_name == 'ebv_name' || attribute_name == 'ebv_domain' ||
      attribute_name == 'ebv_spatial_scope' || attribute_name == 'ebv_entity_type'){
    #reopen file
    hdf <- rhdf5::H5Fopen(filepath)
    keywords_old <- ebv_i_read_att(hdf, 'keywords', verbose)
    #get part 1: before changed value
    start_i <- stringr::str_locate(keywords_old, attribute_name)[1]
    end_i <- start_i+nchar(attribute_name)+1 #1: for ': '
    part_1 <- stringr::str_sub(keywords_old, 1, end_i)
    #get everything after changed values
    if(attribute_name == 'ebv_class'){
      start_j <- stringr::str_locate(keywords_old, ', ebv_name')[1]
    }else if(attribute_name == 'ebv_name'){
      start_j <- stringr::str_locate(keywords_old, ', ebv_domain')[1]
    }else if(attribute_name == 'ebv_domain'){
      start_j <- stringr::str_locate(keywords_old, ', ebv_spatial_scope')[1]
    }else if(attribute_name == 'ebv_spatial_scope'){
      start_j <- stringr::str_locate(keywords_old, ', ebv_entity_type')[1]
    }else if(attribute_name == 'ebv_entity_type'){
      start_j <- nchar(keywords_old)+1
    }
    part_2 <- stringr::str_sub(keywords_old, start_j)
    #put together new keywords
    keywords_new <- paste0(part_1, value, part_2)
    #overwrite keywords
    ebv_i_char_att(hdf, 'keywords', keywords_new)
  }


  if(exists('hdf')){
    if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
  }

}
