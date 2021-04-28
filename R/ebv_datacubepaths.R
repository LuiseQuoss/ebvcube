#' Get datacubepaths
#' @description Get the paths to the datacubes to access and read the data.
#'
#' @param filepath Path to the NetCDF file.
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return Dataframe containing the paths to access the datacubes and
#'   descriptions of scenario, metric and entity if existing.
#' @export
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
ebv_datacubepaths <- function(filepath, verbose = FALSE){
  #turn off local warnings if verbose=TRUE ----
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  # ensure file and all datahandles are closed on exit ----
  defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  ####initial tests start ----
  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
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

  #######initial test end ----

  #file overview
  ls <- rhdf5::h5ls(filepath)
  #get all datasets
  remove <- c('crs', 'dim_entity', 'lat', 'lon', 'crs', 'time', 'var_entity')
  for (r in remove){
    ls <- ls[ls[,2]!=r,]
  }

  #paths
  paths = c()
  for (row in 1:nrow(ls)){
    if(ls[row,3] == 'H5I_DATASET'){
      p <- paste0(ls[row,'group'], '/', ls[row, 'name'])
      p <- substring(p, 2, nchar(p))
      paths <- c(paths, p)
    }
  }

  #open file
  hdf <- rhdf5::H5Fopen(filepath)

  #ebv_subgroups
  subgroups <- ebv_i_read_att(hdf, 'ebv_subgroups')

  #build result
  entity_longnames <- c()
  if('scenario' %in% subgroups & 'metric' %in% subgroups){
    scenario_longnames <- c()
    metric_longnames <- c()
    for (p in paths){
      #entity longname
      dh <- hdf&p
      e <- ebv_i_read_att(dh, 'label')
      rhdf5::H5Dclose(dh)
      #scenario longname
      scenario <- stringr::str_split(p, '/')[[1]][1]
      dh <- hdf&scenario
      s <- ebv_i_read_att(dh, 'label')
      rhdf5::H5Gclose(dh)
      #metric longname
      metric <- paste0(scenario, '/', stringr::str_split(p, '/')[[1]][2])
      dh <- hdf&metric
      m <- ebv_i_read_att(dh, 'label')
      rhdf5::H5Gclose(dh)
      #collect infos
      scenario_longnames <- c(scenario_longnames, s)
      metric_longnames <- c(metric_longnames, m)
      entity_longnames <- c(entity_longnames, e)
    }
    #build result data.frame
    result = data.frame(paths, scenario_longnames, metric_longnames, entity_longnames)
  } else{
    metric_longnames <- c()
    for (p in paths){
      #entity longname
      dh <- hdf&p
      e <- ebv_i_read_att(dh, 'label')
      rhdf5::H5Dclose(dh)
      #metric longname
      metric <- stringr::str_split(p, '/')[[1]][1]
      #delete 'if' when hennekens is corrected! - all datasets must have a metric!
      if (metric == ''){
        m <- 'none'
      }else{
        dh <- hdf&metric
        m <- ebv_i_read_att(dh, 'label')
        rhdf5::H5Gclose(dh)
      }
      entity_longnames <- c(entity_longnames, e)
      metric_longnames <- c(metric_longnames, m)

    }
    #build result data.frame
    result = data.frame(paths, metric_longnames, entity_longnames)
  }

  #close file
  rhdf5::H5Fclose(hdf)

  return(result)

}
