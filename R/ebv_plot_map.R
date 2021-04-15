#' Map plot of EBV NetCDF
#'
#' @description Map plot of the data of one timestep in one datacube of an EBV
#'   NetCDF. This functions sometimes writes temporary files on your disk.
#'   Speficy a directory for these setting via
#'   options('temp_directory'='/path/to/temp/directory').
#'
#' @param filepath Path to the NetCDF file.
#' @param datacubepath Path to the datacube (use
#'   [ebvnetcdf::ebv_datacubepaths()]).
#' @param timestep Choose one timestep.
#' @param countries Default: TRUE. Simple country outlines will be plotted on
#'   top of the raster data. Disable by setting this option to FALSE.
#' @param col.rev Default: TRUE. Set to FALSE if you want the color ramp to be
#'   the other way around.
#'
#' @note Uses the country outlines data from the
#'   \href{https://cran.r-project.org/package=maptools}{maptools package}.
#'
#' @return Plots a map into the 'Plots' pane in RStudio.
#' @export
#' @importFrom utils data
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # ebv_plot_map(file, datacubes[1,1], 9)
ebv_plot_map <- function(filepath, datacubepath, timestep=1, countries =TRUE, col.rev=TRUE){
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
    rhdf5::H5Fclose(hdf)
    stop(paste0('The given datacubepath is not valid:\n', datacubepath))
  } else {
    rhdf5::H5Fclose(hdf)
  }

  #get properties
  prop <- ebv_properties(filepath, datacubepath)

  #timestep check
  #check if timestep is valid type
  if (class(timestep)=='numeric'){
    for (t in timestep){
      if (! as.integer(t)==t){
        stop('Timestep has to be an integer or a list of integers.')
      }
    }
  } else {
    stop('Timestep has to be of class numeric.')
  }

  #check timestep range
  for (t in timestep){
    max_time <- prop@spatial$dimensions[3]
    min_time <- 1
    if (t>max_time | t<min_time){
      stop(paste0('Chosen timestep ', t, ' is out of bounds. Timestep range is ', min_time, ' to ', max_time, '.'))
    }
  }

  # end initial tests ----

  #get needed properties
  fillvalue <- prop@entity$fillvalue
  type.short <- ebv_i_type_r(prop@entity$type)
  title <- prop@general$title
  label <- prop@entity$label
  subtitle <- paste0(label, ' (timestep: ', timestep, ')')
  epsg <- prop@spatial$epsg

  #get raster data - ram check included
  results <- tryCatch(
    #try ----
    {
      data.raster <- ebv_data_read(filepath, datacubepath, timestep = timestep, delayed = FALSE, raster=TRUE) #if this throws an error the data is going to plotted in lower res
      data.all <- HDF5Array::HDF5Array(filepath = filepath, name = datacubepath, type = type.short)
      #data.all <- replace(data.all, c(which(data.all==fillvalue)), c(NA))
      #mask out fillvalue ----
      data.all <- replace(data.all, data.all==fillvalue, c(NA))
      results <- c(data.raster, data.all, 'NULL')
    },
    #change res ----
    error = function(cond){
      print(cond)
      message(paste0('Data will be displayed in a lower resolution. May take up to a few minutes. Original resolution: ',
                     prop@spatial$resolution[1] , ', displayed resoultion: 1 degree.'))
      #check temp directory
      temp_path <- getOption('temp_directory')[[1]]
      if (is.null(temp_path)){
        stop('This function creates a temporary file. Please specify a temporary directory via options.')
      } else if (!dir.exists(temp_path)){
        stop('The temporary directory given by you does not exist. Please change!\n', temp_path)
      }
      #define temp file
      name <- 'temp_EBV_change_res_plot_map.tif'
      temp.map <- file.path(temp_path, name)
      if (file.exists(temp.map)){
        file.remove(temp.map)
      }
      #(filepath_src, datacubepath_src, resolution, outputpath, timestep = 1, method='average', return.raster=FALSE, overwrite = FALSE, ignore.RAM=FALSE)
      data.raster <- ebv_data_change_res(filepath, datacubepath, c(1,1, 4326), temp.map, timestep, return.raster = TRUE)
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
  s <- stats::quantile(data.all, probs = seq(0, 1, 0.2), na.rm=TRUE)
  #s <- quantile(data.all[,,c(1,2,4,5,6,7,8,9,10,11,12,13,14,15)], probs=seq(0,1,0.2), na.rm=T)
  #for predict

  #check if quantile list values are unique
  if(length(unique(s)) != 6){
    message('Error while creating quantiles. Color Scale will be corrupted')
    s <- unique(s)
  }

  #get correct colors
  if (min(s)<0 & max(s)>0){
    diverging <- 'Blue-Red'
    col.regions <- colorspace::diverging_hcl(5, palette = diverging, rev = col.rev) #"BluYl"
  } else {
    single <- 'YlGn' # 'Greens'#
    col.regions <- colorspace::sequential_hcl(5, palette = single, rev = col.rev) #"BluYl"
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
      wrld_simpl <- pkgcond::suppress_warnings(sp::spTransform(wrld_simpl, sp::CRS(SRS_string = paste0('EPSG:', epsg))))
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
