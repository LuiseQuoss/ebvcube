#' Map plot of an EBV NetCDF
#'
#' @description Map plot of the data of one timestep in one datacube of an EBV
#'   NetCDF. This functions sometimes writes temporary files on your disk.
#'   Speficy a directory for these setting via
#'   options('temp_directory'='/path/to/temp/directory').
#'
#' @param filepath Character. Path to the NetCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param timestep Integer. Choose one timestep.
#' @param countries Logical. Default: TRUE. Simple country outlines will be
#'   plotted on top of the raster data. Disable by setting this option to FALSE.
#' @param col.rev Logical. Default: TRUE. Set to FALSE if you want the color
#'   ramp to be the other way around.
#' @param classes Integer. Default: 5. Define the amount of classes (quantiles)
#'   for the symbology. Currently restricted to maximum 15 classes.
#' @param ignore.RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note Uses the country outlines data from the
#'   \href{https://cran.r-project.org/package=maptools}{maptools package}.
#'
#' @return Plots a map.
#' @export
#' @importFrom utils data
#' @importFrom colorspace diverging_hcl sequential_hcl
#'
#' @examples
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' ebv_plot_map(file, datacubes[1,1], timestep=9, classes=7)
ebv_plot_map <- function(filepath, datacubepath, timestep=1, countries =TRUE,
                         col.rev=TRUE, classes = 5, ignore.RAM=FALSE, verbose=FALSE){
  # start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

  #ensure that all tempfiles are deleted on exit
  withr::defer(
    if(exists('temp.map')){
      if(file.exists(temp.map)){
        file.remove(temp.map)
      }
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
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given datacubepath is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

  #get properties
  prop <- ebv_properties(filepath, datacubepath, verbose)

  #timestep check
  #check if timestep is valid type
  if (checkmate::checkInt(timestep)!=TRUE){
    stop('The argument timestep must be of type "single integerish value"')
  }

  #check timestep range
  min_time <- 1
  max_time <- prop@spatial$dimensions[3]
  if(checkmate::checkInt(timestep, lower=min_time, upper=max_time)!= TRUE){
    stop(paste0('Chosen timestep ', timestep, ' is out of bounds. Timestep range is ',
                min_time, ' to ', max_time, '.'))
  }

    #check classes argument - single integer
  if (checkmate::checkInt(classes)!=TRUE){
    stop('The argument classes must be of type "single integerish value"')
  }
  if (checkmate::checkInt(classes, upper=15)!=TRUE){
    stop('The value of classes is too big. It is limitated to 15.')
  }

  #check logical arguments
  if(checkmate::checkLogical(ignore.RAM, len=1, any.missing=F) != TRUE){
    stop('ignore.RAM must be of type logical.')
  }
  if(checkmate::checkLogical(countries, len=1, any.missing=F) != TRUE){
    stop('countries must be of type logical.')
  }
  if(checkmate::checkLogical(col.rev, len=1, any.missing=F) != TRUE){
    stop('col.rev must be of type logical.')
  }

  # end initial tests ----

  #get needed properties
  fillvalue <- prop@entity$fillvalue[1]
  type.short <- ebv_i_type_r(prop@entity$type)
  title <- prop@general$title
  label <- prop@entity$standard_name
  subtitle <- paste0(label, ' (timestep: ', timestep, ')')
  epsg <- prop@spatial$epsg

  #get raster data - ram check included
  results <- tryCatch(
    #try reading whole data----
    {
      data.raster <- ebv_data_read(filepath, datacubepath, timestep = timestep,
                                   delayed = FALSE, raster=TRUE, ignore.RAM=ignore.RAM,
                                   verbose=verbose) #if this throws an error the data is going to plotted in lower res
      hdf <- rhdf5::H5Fopen(filepath)
      if (is.empty(ebv_i_check_data(hdf, datacubepath))){
        message('Quantiles based on all layers.')
        data.all <- HDF5Array::HDF5Array(filepath = filepath, name = datacubepath,
                                         type = type.short)
      }else{
        message('Quantiles only based on current layer because not all layers hold data.')
        data.all <- raster::as.matrix(data.raster)
      }
      rhdf5::H5Fclose(hdf)

      #mask out fillvalue ----
      data.all <- replace(data.all, data.all==fillvalue, c(NA))
      results <- list(data.raster, data.all, 'NULL')
    },
    #change res if data is too big----
    error = function(cond){
      if (!stringr::str_detect(cond, 'memory')){
        stop(cond)
      }
      message(paste0('Data will be displayed in a lower resolution. May take up to a few minutes. Original resolution: ',
                     prop@spatial$resolution[1] , ', displayed resoultion: 1 degree.'))
      message('Quantiles only based on current layer.')
      #check temp directory
      temp_path <- getOption('temp_directory')[[1]]
      if (is.null(temp_path)){
        stop('This function creates a temporary file. Please specify a temporary directory via options.')
      } else {
        if (checkmate::checkCharacter(temp_path) != TRUE){
          stop('The temporary directory must be of type character.')
        }
        if (checkmate::checkDirectoryExists(temp_path) != TRUE){
          stop('The temporary directory given by you does not exist. Please change!\n', temp_path)
        }
      }
      #define temp file
      name <- 'temp_EBV_change_res_plot_map.tif'
      temp.map <- file.path(temp_path, name)
      if (file.exists(temp.map)){
        file.remove(temp.map)
      }
      data.raster <- ebv_data_change_res(filepath, datacubepath, c(1,1, 4326),
                                         temp.map, timestep, return.raster = TRUE, ignore.RAM=ignore.RAM, verbose=verbose)
      data.raster <- data.raster[[1]] #get first layer of brick --> class = raster
      data.all <- raster::as.array(data.raster) #use only one layer for quantile analysis
      results <- list(data.raster, data.all, temp.map)
      return(results)
    }
  )
  data.raster <- results[[1]]
  data.all <- results[[2]]
  temp.map <- results[[3]]
  #get quantiles ----
  s <- stats::quantile(data.all, probs = seq(0, 1, (1/classes)), na.rm=TRUE)

  #check if quantile list values are unique
  if(length(unique(s)) != (classes+1)){
    message('Error while creating quantiles. Color Scale will be corrupted')
    s <- unique(s)
  }

  #get correct colors ----
  if (min(s)<0 & max(s)>0){
    diverging <- 'Blue-Red'
    col.regions <- colorspace::diverging_hcl(classes, palette = diverging,
                                             rev = col.rev)
  } else {
    single <- 'YlGn' # 'Greens'#
    col.regions <- colorspace::sequential_hcl(classes, palette = single,
                                              rev = col.rev)
  }

  #define display options ----
  old.par <- lattice::trellis.par.get()
  my.theme <- lattice::trellis.par.get()
  my.theme$par.sub.text$cex <- 0.8
  my.theme$par.sub.text$col <- 'grey'
  my.theme$par.main.text$col <- 'grey'
  my.theme$par.main.text$cex <- 1.2
  lattice::trellis.par.set(my.theme)
  withr::defer(lattice::trellis.par.set(old.par))

  #plot with country outlines ----
  if (countries){
    if(epsg != 4326){
      wrld_simpl <- sp::spTransform(wrld_simpl,
                                    sp::CRS(SRS_string = paste0('EPSG:', epsg)))
    }

    print(
      sp::spplot(data.raster, xlab='latitude', ylab='longitude',
             scales=list(draw = TRUE), at = s,
             col.regions=col.regions,
             sp.layout = list(wrld_simpl, first=FALSE),
             set = TRUE,
             main = title,
             sub = subtitle
      )
    )

  } else{
    #plot without country outlines ----
    print(
      sp::spplot(data.raster, xlab='latitude', ylab='longitude',
             scales=list(draw = TRUE), at = s,
             col.regions=col.regions,
             set = TRUE,
             main = title,
             sub = subtitle
      )
    )

  }

  #remove temporary file ----
  if (temp.map!= 'NULL'){
    if (file.exists(temp.map)){
      file.remove(temp.map)
    }
  }

}
