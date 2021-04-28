#' Plot the average over time of one datacube of an EBV NetCDF
#'
#' @description Plot the average (y-axis) of one datacube of a EBV NetCDF over
#'   time (x-axis).
#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param color Default: dodgerblue4. Change as you like.
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return Displays a plot in 'Plots' pane in RStudio. Returns a vector of the
#'   averages.
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom graphics par
#'
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' #ebv_plot_indicator(file, datacubes[1,1])
ebv_plot_indicator <- function(filepath, datacubepath, color="dodgerblue4",
                               verbose=FALSE){
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

  # start initial tests ----
  #are all arguments given?
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
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

  #datacubepath check
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given datacubepath is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

  # end initial tests ----

  # basic attributes ----
  prop <- ebv_properties(filepath, datacubepath, verbose)
  time <- prop@spatial$dimensions[3]
  timevalues <- prop@temporal$timesteps.natural
  title <- prop@general$title
  label <- prop@entity$label
  fillvalue <- prop@entity$fillvalue
  type.short <- ebv_i_type_r(prop@entity$type)
  dims <- prop@spatial$dimensions

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

    # warning for longer caluculation
    size <- dims[1]*dims[2]*dims[3]
    if (size > 100000000){
      message('Wow that is huge! Maybe get a tea...')
    }

    #calculate mean values ----
    false <- c()
    values <- c(1:time)
    print('calculating timesteps...')
    pb <- utils::txtProgressBar(min = 1, max = dims[3], initial = 1)
    for (t in 1:time){
      utils::setTxtProgressBar(pb,t)
      f <- tryCatch(
        {
          data <- data.all[,,t]
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

    #plot ----
    withr::local_par(mar=c(7,5,3,1))
    plot(timevalues, values, xlab = 'time', ylab='average', type = 'b',
         main = title, col.main = 'grey', cex.main = 1.2, font.main=2,
         sub =label, col.sub = 'grey', cex.sub=0.8, font.sub=2,
         lwd = 2,
         col = ifelse(1:time %in% as.integer(false), 'red', color)
    )
  }

  #return mean values
  return(values)

}
