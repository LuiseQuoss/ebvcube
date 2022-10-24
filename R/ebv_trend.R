#' Plot the trend of an EBV NetCDF
#'
#' @description Plot the trend of one datacube of a EBV NetCDF over time
#'   (x-axis). Different options can be chosen based on the `method` argument.
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param method Character. Default: mean. Choose one of the following options
#'   for different plots: mean, min, max, boxplot. See **Note** for more
#'   information.
#' @param subset Character. Default: NULL. If you want to look at the trend for
#'   a spatial subset, define the path to the shapefile encompassing the area.
#'   Ending needs to be *.shp.
#' @param touches Logical. Optional. Default: TRUE. Only relevant if the subset is
#'   indicated by a shapefile. See [ebvcube::ebv_read_shp()].
#' @param color Character. Default: dodgerblue4. Change to any color known by R
#'   [grDevices::colors()]
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note More information on the `method` argument: using `mean` will result in
#'   a plot of the mean over time, additionally a vector of the mean values is
#'   returned. If the data encompasses only one timestep a single mean is
#'   returned. Corresponding behavior can be expected for `min` and `max`. The
#'   `boxplot` option results in boxplots over time (no values are returned).
#'
#' @return Returns plots and eventually values based on the `method` argument.
#'   See **Note** for more information
#'
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom grDevices colors
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #plot the change of the mean over time of the first datacube
#' #ebv_trend(filepath = file, datacubepath = datacubes[1,1], entity = 1)
ebv_trend <- function(filepath, datacubepath, entity=NULL, method='mean',
                      subset=NULL, color="dodgerblue4", touches=TRUE,
                      verbose=FALSE){
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
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE | !stringr::str_detect(datacubepath, 'ebv_cube')){
    stop(paste0('The given datacubepath is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

  #check color
  if( ! color %in% grDevices::colors()){
    stop('color not known. Choose a different one!')
  }

  #get properties----
  prop <- ebv_properties(filepath, datacubepath, verbose)

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

  # only basic shp filepath check - leave the rest of the checks to ebv_read_shp
  if(!is.null(subset)){
    if (checkmate::checkCharacter(subset) != TRUE){
      stop('Shapefilepath must be of type character.')
    }
    if (checkmate::checkFileExists(subset) != TRUE){
      stop(paste0('Shapefile does not exist.\n', subset))
    }
    if (!endsWith(subset, '.shp')){
      stop(paste0('File ending of subset is wrong. File cannot be processed.'))
    }
  }

  #check method values
  if(! method %in% c('mean', 'boxplot', 'min', 'max')){
    stop('The method argument is invalid. Please check the help page for all available method values.')
  }

  # end initial tests ----

  # basic attributes ----
  time <- prop@spatial$dimensions[3]
  timevalues <- prop@temporal$timesteps_natural
  #derive years
  if(grepl('^P\\d{4}-?\\d{0,2}-?\\d{0,2}$', prop@temporal$resolution)){
    if(stringr::str_split(prop@temporal$resolution, '-')[[1]][2]=='00' &
       stringr::str_split(prop@temporal$resolution, '-')[[1]][3]=='00'){
      timevalues <- format(as.Date(timevalues, format='%Y-%m-%d'), '%Y')
    }
  }
  title <- prop@general$title
  fillvalue <- prop@ebv_cube$fillvalue
  type.short <- ebv_i_type_r(prop@ebv_cube$type)
  dims <- prop@spatial$dimensions
  units <- prop@ebv_cube$units

  #label
  ls <- rhdf5::h5ls(filepath)
  if('entity' %in% ls$name){
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

  #1. check subset or not -> get data accordingly
  #2. check method
  #3. check timestep in data

  #1. check subset or not and get data ----
  if(!is.null(subset)){
    print('Getting subset of the data')
    data.all.raster <- ebv_read_shp(filepath = filepath,
                             datacubepath = datacubepath,
                             entity = entity,
                             timestep = 1:dims[3],#get all timesteps
                             shp = subset,
                             touches = touches)
    #turn into array
    #DelayedArray::DelayedArray(data.all.raster)
    data.all <- terra::as.array(data.all.raster)
    rm(data.all.raster)

  }else{
    #1. get data for spatial extent
    data.all <- HDF5Array::HDF5Array(filepath = filepath, name =datacubepath,
                                     type = type.short)

    #mask out fillvalue
    data.all <- replace(data.all, data.all==fillvalue, c(NA))

    #data.all
    if(is_4D){
      data.all <- data.all[,,,entity_index]
    }


  }

  #2. check method ----
  if(method=='mean' | method=='min' | method=='max'){
    #method == mean, min, max----
    #only one timestep----
    if (dims[3]==1){
      message('Dataset has only one timestep. Single value will be returned.')

      if(method=='mean'){
        values <- mean(data.all, na.rm =T)
      } else if(method=='min'){
        values <- min(data.all, na.rm =T)
      } else if(method=='max'){
        values <- max(data.all, na.rm =T)
      }

    }else{
      #several timesteps----

      # warning for longer calculation
      if(is_4D){
        size <- dims[1]*dims[2]*dims[3]*dims[4]
      }else{
        size <- dims[1]*dims[2]*dims[3]
      }
      if (size > 100000000){
        message('Wow that is huge! Maybe get a tea...')
      }

      #calculate mean/min/max values
      false <- c()
      values <- c(1:time)
      print('calculating timesteps...')
      pb <- utils::txtProgressBar(min = 1, max = dims[3], initial = 1)
      for (t in 1:time){
        utils::setTxtProgressBar(pb,t)
        f <- tryCatch(
          {
            if(is_4D){
              data <- data.all[,,t]
            }else{
              data <- data.all[,,t]
            }
            if(method=='mean'){
              v <- mean(data, na.rm =T)
            } else if(method=='min'){
              v <- min(data, na.rm =T)
            } else if(method=='max'){
              v <- max(data, na.rm =T)
            }
            f <- 0
          },
          error = function(e){
            message(paste0('No data in netCDF for timestep ', t,
                           '. Substituted with value of timestep ', t-1, '.'))
            mean <- v
            return(t)
          }
        )

        values[t] <- v
        false <- c(false, f)
      }

      #plot
      timevalues <- as.character(timevalues)
      dt <- as.data.frame(values)
      dt <- cbind(dt, timevalues)

      print(ggplot2::ggplot(data=dt, ggplot2::aes(x=timevalues, y=values))+
        ggplot2::geom_point(color=color, shape=20, size=2) +
        ggplot2::xlab('time') +
        ggplot2::ggtitle(paste(strwrap(
          title,
          width = 80
          ), collapse = "\n")) +
        ggplot2::labs(subtitle=label)+
        ggplot2::ylab(paste0(method, '\n(',units,')')) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90)) +
        {if(length(timevalues)>15){
          ggplot2::scale_x_discrete(breaks=timevalues[seq(1, length(timevalues), 5)])
        } else{
          ggplot2::scale_x_discrete(breaks=timevalues)
        }}

        )


    }

    #return values
    return(values)

  } else if(method=='boxplot'){
    #method == boxplot----

    #only one timestep----
    if (dims[3]==1){
      message('Dataset has only one timestep. Single boxplot will be returned.')

      #data to data.frame
      dataset <- as.data.frame(x = c(data.all))
      colnames(dataset) <- 'V1'

      #return(dataset)
      ggp <- ggplot2::ggplot(data = dataset, ggplot2::aes(x=factor(timevalues), y=dataset$V1,)) +
        ggplot2::geom_boxplot(fill=color, outlier.size = 0.7, outlier.shape = 20) +
        ggplot2::ylab(units) +
        ggplot2::xlab('Time') +
        ggplot2::ggtitle(label=title, subtitle=label) +
        ggplot2::theme_minimal() +
        ggplot2::stat_summary(fun = "mean", geom = "point", shape = 3,
                              size = 1, color = "lightblue")
      print(ggp)

    }else{
      #multiple timesteps----

      # warning for longer calculation
      if(is_4D){
        size <- as.numeric(dims[1])*as.numeric(dims[2])*as.numeric(dims[3])*as.numeric(dims[4])
      }else{
        size <- as.numeric(dims[1])*as.numeric(dims[2])*as.numeric(dims[3])
      }
      if (size > 100000000){
        message('Wow that is huge! Maybe get a tea...')
      }

      #rearrange data into data frame
      df <- reshape2::melt(as.array(data.all), na.rm = TRUE)

      ggp <- ggplot2::ggplot(data = df, ggplot2::aes(x=factor(df$Var3), y=df$value)) +
        ggplot2::geom_boxplot(fill=color, outlier.size = 0.7, outlier.shape = 20) +
        ggplot2::scale_x_discrete('Time',  breaks=unique(df$Var3), labels= timevalues)+
        ggplot2::ylab(units) +
        ggplot2::ggtitle(label=title, subtitle=label) +
        ggplot2::theme_minimal() +
        ggplot2::stat_summary(fun = "mean", geom = "point", shape = 3,
                              size = 1, color = "lightblue") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=50)) #turn axis labels by 50 degrees


      print(ggp)

    }
  }


}
