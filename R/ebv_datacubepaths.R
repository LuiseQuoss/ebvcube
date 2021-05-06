#' Get datacubepaths of EBV NetCDF
#' @description Get the paths to the datacubes of the EBV NetCDF to access the
#'   data.
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Dataframe containing the paths to access the datacubes and
#'   descriptions of scenario, metric and entity if existing.
#' @export
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
ebv_datacubepaths <- function(filepath, verbose = FALSE){
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

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #filepath check
  if (checkmate::checkCharacter(filepath) != TRUE){
    stop('Filepath must be of type character.')
  }
  if (checkmate::checkFileExists(filepath) != TRUE){
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
  #get all datasets ----
  remove <- c('crs', 'dim_entity', 'lat', 'lon', 'crs', 'time', 'var_entity')
  for (r in remove){
    ls <- ls[ls[,2]!=r,]
  }

  #paths ----
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

  #build result ----
  entity_longnames <- c()
  #scenario and metric ----
  if('scenario' %in% subgroups & 'metric' %in% subgroups){
    scenario_longnames <- c()
    metric_longnames <- c()
    for (p in paths){
      #entity longname
      dh <- hdf&p
      if (rhdf5::H5Aexists(dh, 'label')){
        e <- ebv_i_read_att(dh, 'label')
      } else if (rhdf5::H5Aexists(dh, 'standard_name')) {
        e <- ebv_i_read_att(dh, 'standard_name')
      } else {
        e <- 'not defined'
      }
      rhdf5::H5Dclose(dh)
      #scenario longname
      scenario <- stringr::str_split(p, '/')[[1]][1]
      dh <- hdf&scenario
      if (rhdf5::H5Aexists(dh, 'label')){
        s <- ebv_i_read_att(dh, 'label')
      } else if (rhdf5::H5Aexists(dh, 'standard_name')) {
        s <- ebv_i_read_att(dh, 'standard_name')
      } else {
        s <- 'not defined'
      }
      rhdf5::H5Gclose(dh)
      #metric longname
      metric <- paste0(scenario, '/', stringr::str_split(p, '/')[[1]][2])
      dh <- hdf&metric
      if (rhdf5::H5Aexists(dh, 'label')){
        m <- ebv_i_read_att(dh, 'label')
      } else if (rhdf5::H5Aexists(dh, 'standard_name')) {
        m <- ebv_i_read_att(dh, 'standard_name')
      } else {
        m <- 'not defined'
      }
      rhdf5::H5Gclose(dh)
      #collect infos
      scenario_longnames <- c(scenario_longnames, s)
      metric_longnames <- c(metric_longnames, m)
      entity_longnames <- c(entity_longnames, e)
    }
    #build result data.frame
    result = data.frame(paths, scenario_longnames, metric_longnames, entity_longnames)
  } else{
    # only metric ----
    metric_longnames <- c()
    for (p in paths){
      #entity longname
      dh <- hdf&p
      if (rhdf5::H5Aexists(dh, 'label')){
        e <- ebv_i_read_att(dh, 'label')
      } else if (rhdf5::H5Aexists(dh, 'standard_name')) {
        e <- ebv_i_read_att(dh, 'standard_name')
      } else {
        e <- 'not defined'
      }
      rhdf5::H5Dclose(dh)
      #metric longname
      metric <- stringr::str_split(p, '/')[[1]][1]
      #delete 'if' when hennekens is corrected! - all datasets must have a metric!
      if (metric == ''){
        m <- 'none'
      }else{
        dh <- hdf&metric
        if (rhdf5::H5Aexists(dh, 'label')){
          m <- ebv_i_read_att(dh, 'label')
        } else if (rhdf5::H5Aexists(dh, 'standard_name')) {
          m <- ebv_i_read_att(dh, 'standard_name')
        } else {
          m <- 'not defined'
        }
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
