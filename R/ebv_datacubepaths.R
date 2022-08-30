#' Get datacubepaths of EBV netCDF
#' @description Get the paths to the datacubes of the EBV netCDF to access the
#'   data.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @return Dataframe containing the paths to access the datacubes and
#'   descriptions of scenario, metric and entity if existing.
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
#'
#' #get all datacubepaths of EBV netCDF
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

  #######initial test end ----

  #file overview
  ls <- rhdf5::h5ls(filepath)

  #check structure
  if('entities' %in% ls$name){
    new <- TRUE
  } else{
    new <- FALSE
  }

  #check 4D or 3D
  is_4D <- ebv_i_4D(filepath)

  #get all datasets ----
  remove <- c('crs', 'dim_entity', 'lat', 'lon', 'crs', 'time', 'var_entity',
              'entities', 'entity', 'nchar')
  for (r in remove){
    ls <- ls[ls[,2]!=r,]
  }

  #paths ----
  datacubepaths = c()
  for (row in 1:nrow(ls)){
    if(ls[row,3] == 'H5I_DATASET'){
      p <- paste0(ls[row,'group'], '/', ls[row, 'name'])
      p <- substring(p, 2, nchar(p))
      datacubepaths <- c(datacubepaths, p)
    }
  }

  #open file
  hdf <- rhdf5::H5Fopen(filepath, flags="H5F_ACC_RDONLY")

  #ebv_subgroups
  if (! new){
    subgroups <- ebv_i_read_att(hdf, 'ebv_subgroups')
  } else{
    subgroups <- c()
  }

  if(!is_4D){
    #process 3D----
    entity_names <- c()
    #scenario and metric ----
    if(('scenario' %in% subgroups )| stringr::str_detect(datacubepaths[1], 'scenario')){
      scenario_names <- c()
      metric_names <- c()
      for (p in datacubepaths){
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
        scenario_names <- c(scenario_names, s)
        metric_names <- c(metric_names, m)
        entity_names <- c(entity_names, e)
      }
      #build result data.frame
      if(!new | !any(entity_names=='not defined')){
        result = data.frame(datacubepaths, scenario_names, metric_names, entity_names)
      } else{
        result = data.frame(datacubepaths, scenario_names, metric_names)
      }
    } else{
      # only metric ----
      metric_names <- c()
      for (p in datacubepaths){
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
        entity_names <- c(entity_names, e)
        metric_names <- c(metric_names, m)

      }
      #build result data.frame
      if(!new | ! any(entity_names=='not defined')){
        result = data.frame(datacubepaths, metric_names, entity_names)
      } else{
        result = data.frame(datacubepaths, metric_names)
      }

    }
  } else{
    #process 4D----

    #scenario and metric ----
    if(('scenario' %in% subgroups )| stringr::str_detect(datacubepaths[1], 'scenario')){
      scenario_names <- c()
      metric_names <- c()
        #scenario longname
      for (p in datacubepaths){
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
        scenario_names <- c(scenario_names, s)
        metric_names <- c(metric_names, m)
      }
        #build result data.frame
        result = data.frame(datacubepaths, scenario_names, metric_names)
      } else{
      # only metric ----
      metric_names <- c()
      for (p in datacubepaths){
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
        metric_names <- c(metric_names, m)

      }
      #build result data.frame
        result = data.frame(datacubepaths, metric_names)
    }
  }


  #close file
  rhdf5::H5Fclose(hdf)

  return(result)

}
