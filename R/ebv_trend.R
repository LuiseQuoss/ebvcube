#' Plot the average over time of one datacube of an EBV NetCDF
#'
#' @description Plot the average (y-axis) of one datacube of a EBV NetCDF over
#'   time (x-axis). If the datacube has only one timestep a single mean value is
#'   returned.
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param method. Character. Default: mean. Choose one of the following options
#'   for different plots: mean, boxplot. See **Note** for more information.
#' @param color Character. Default: dodgerblue4. Change to any color known by R
#'   [grDevices::colors()]
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note More information on the `method` argument: using `mean` will result in
#'   a plot of the mean over time, additionally a vector of the mean values is
#'   returned. If the data encompasses only one timestep a single mean is
#'   returned. The `boxplot` option results in boxplots over time.
#'
#' @return Returns plots and eventually values based on the `method` argument. See **Note**
#'   for more information
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom graphics par plot
#' @importFrom grDevices colors
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #plot the change of the mean over time of the first datacube
#' ebv_trend(filepath = file, datacubepath = datacubes[1,1], entity = NULL)
ebv_trend <- function(filepath, datacubepath, entity=NULL, method='mean',
                          color="dodgerblue4", verbose=FALSE){
  # start initial tests ----
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
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
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

  #datacubepath check
  hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given datacubepath is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

  #check color
  if( ! color %in% grDevices::colors()){
    stop('color not known. Choose a different one!')
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

  # end initial tests ----

  # basic attributes ----
  prop <- ebv_properties(filepath, datacubepath, verbose)
  time <- prop@spatial$dimensions[3]
  timevalues <- prop@temporal$timesteps_natural
  title <- prop@general$title
  fillvalue <- prop@ebv_cube$fillvalue
  type.short <- ebv_i_type_r(prop@ebv_cube$type)
  dims <- prop@spatial$dimensions
  #label
  ls <- rhdf5::h5ls(filepath)
  if('entities' %in% ls$name){
    new <- TRUE
  } else{
    new <- FALSE
  }
  if(new){
    if(checkmate::checkIntegerish(entity, len=1) == TRUE){
      label <- prop@general$entity_names[entity]
      entity_index <- entity
    } else if (checkmate::checkCharacter(entity)==TRUE){
      label <- entity
      entity_index <- which(entity_names==entity)
    }
  }else{
    label <- prop@ebv_cube$standard_name
    entity_index <- 1
  }

  #check method values
  if(! method %in% c('mean', 'boxplot')){
    stop('The method argument is invalid. Please check the help page for all available method values.')
  }

  if(method=='mean'){
    #method == mean----
    #check if only one timestep
    if (dims[3]==1){
      message('Dataset has only one timestep. Single mean will be returned.')
      #data.all
      data.all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath,
                                       type = type.short)
      values <- mean(data.all)
    }else{
      #data.all
      data.all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath,
                                       type = type.short)

      #mask out fillvalue
      data.all <- replace(data.all, data.all==fillvalue, c(NA))

      # warning for longer calculation
      if(is_4D){
        size <- dims[1]*dims[2]*dims[3]*dims[4]
      }else{
        size <- dims[1]*dims[2]*dims[3]
      }
      if (size > 100000000){
        message('Wow that is huge! Maybe get a tea...')
      }

      #calculate mean values
      false <- c()
      values <- c(1:time)
      print('calculating timesteps...')
      pb <- utils::txtProgressBar(min = 1, max = dims[3], initial = 1)
      for (t in 1:time){
        utils::setTxtProgressBar(pb,t)
        f <- tryCatch(
          {
            if(is_4D){
              data <- data.all[,,t,entity_index]
            }else{
              data <- data.all[,,t]
            }
            mean <- mean(data, na.rm=TRUE)
            f <- 0
          },
          error = function(e){
            message(paste0('No mean value for timestep ', t,
                           '. Substituted with mean value of timestep ', t-1, '.'))
            mean <- mean
            return(t)
          }
        )

        values[t] <- mean
        false <- c(false, f)
      }

      #derive years
      timevalues <- format(as.Date(timevalues, format='%Y-%m-%d'), '%Y')

      #plot
      withr::local_par(mar=c(7,5,3,1))
      plot(timevalues, values, xlab = 'time', ylab='average', type = 'b',
           main = paste(strwrap(
             title,
             width = 80
           ), collapse = "\n"),
           col.main = 'grey', cex.main = 1.2, font.main=2,
           sub =label, col.sub = 'grey', cex.sub=0.8, font.sub=2,
           lwd = 2,
           col = ifelse(1:time %in% as.integer(false), 'red', color)
      )
    }

    #return mean values
    return(values)
  } else if(method=='boxplot'){
    #method == boxplot----
    stop('Method boxplot is not yet implemented.')
  }


}
