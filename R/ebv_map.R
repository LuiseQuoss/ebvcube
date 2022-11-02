#' Map plot of an EBV netCDF
#'
#' @description Map plot of the data of one timestep in one datacube of an EBV
#'   netCDF.
#'
#' @param filepath Character. Path to the netCDF file.
#' @param datacubepath Character. Path to the datacube (use
#'   [ebvcube::ebv_datacubepaths()]).
#' @param entity Character or Integer. Default is NULL. If the structure is 3D,
#'   the entity argument is set to NULL. Else, a character string or single
#'   integer value must indicate the entity of the 4D structure of the EBV
#'   netCDFs.
#' @param timestep Integer. Choose one timestep.
#' @param countries Logical. Default: TRUE. Simple country outlines will be
#'   plotted on top of the raster data. Disable by setting this option to FALSE.
#' @param col_rev Logical. Default: FALSE Set to TRUE if you want the color
#'   ramp to be the other way around.
#' @param classes Integer. Default: 5. Define the amount of classes (quantiles)
#'   for the symbology. Currently restricted to maximum 15 classes.
#' @param all_data Logical. Default: FALSE. The quantiles are based on the one
#'   timestep you chose (default). If you want include the full data of the
#'   datacube to produce several maps that are based on the same color scale,
#'   set this argument to TRUE (to allow for viusual comparison between entities
#'   or timesteps. Does not cover different datacubes.)
#' @param ignore_RAM Logical. Default: FALSE. Checks if there is enough space in
#'   your memory to read the data. Can be switched off (set to TRUE).
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Plots a map.
#' @export
#'
#' @examples
#' \dontrun{
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #plot a map for the 9th timestep, divide into 7 classes
#' ebv_map(filepath = file, datacubepath = datacubes[1,1], entity = 1,
#'         timestep = 9, classes = 7)
#' }
ebv_map <- function(filepath, datacubepath, entity=NULL, timestep=1, countries =TRUE,
                    col_rev=FALSE, classes = 5, all_data = FALSE, ignore_RAM=FALSE,
                    verbose=TRUE){
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
        if(!is.null(temp.map)){
          file.remove(temp.map)
        }
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

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
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
  if(checkmate::checkLogical(ignore_RAM, len=1, any.missing=F) != TRUE){
    stop('ignore_RAM must be of type logical.')
  }
  if(checkmate::checkLogical(countries, len=1, any.missing=F) != TRUE){
    stop('countries must be of type logical.')
  }
  if(checkmate::checkLogical(col_rev, len=1, any.missing=F) != TRUE){
    stop('col_rev must be of type logical.')
  }

  # end initial tests ----

  #get properties ----
  fillvalue <- prop@ebv_cube$fillvalue[1]
  type.short <- ebv_i_type_r(prop@ebv_cube$type)
  title <- prop@general$title
  epsg <- prop@spatial$epsg
  units <- prop@ebv_cube$units
  timestep.nat <- prop@temporal$timesteps_natural[timestep]

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
      label <- prop@general$entity_names[entity_index]
    } else if (checkmate::checkCharacter(entity)==TRUE){
      entity_index <- which(entity_names==entity)
      label <- prop@general$entity_names[entity_index]
    } else{
      entity <- 1 #set entity to 1 (for ebv_i_check_ram)
      label <- prop@general$entity_names[entity]
    }
  } else{
    entity <- 1
    label <- prop@ebv_cube$standard_name
  }

  subtitle <- paste0(label, ' (', timestep.nat,')')

  #read the data necessary for the quantiles----
  data.all <- HDF5Array::HDF5Array(filepath = filepath, name = datacubepath,
                                   type = type.short)

  #choose data of timestep only if chosen by user
  if(!all_data){
    if(is_4D){
      data.all <- data.all[,,timestep,entity_index]
    }else{
      data.all <- data.all[,,timestep]
    }
  }

  #get the raster for plotting----
  #in case the raster is too big for memory -> resample and plot at lower resolution
  results <- tryCatch(
    #try reading whole data
    {
      data.raster <- ebv_read(filepath, datacubepath, entity=entity, timestep = timestep,
                              type='r', ignore_RAM=ignore_RAM,
                              verbose=verbose) #if this throws an error the data is going to plotted in lower res
      results <- list(data.raster, NULL)

    },error = function(cond){
      if (!stringr::str_detect(cond, 'memory')){
        stop(cond)
      }
      message(paste0('Data will be displayed in a lower resolution. May take up to a few minutes. Original resolution: ',
                     prop@spatial$resolution[1] , ', displayed resoultion: 1 degree (WGS84).'))
      #resample data -> temporary file
      temp.map <- tempfile(fileext = '.tif')
      if (file.exists(temp.map)){
        file.remove(temp.map)
      }
      data.raster <- ebv_resample(filepath_src=filepath, datacubepath_src=datacubepath,
                                  entity_src=entity, resolution=c(1,1, 4326),
                                  outputpath=temp.map, timestep_src = timestep,
                                  method='near', return_raster=TRUE, overwrite = TRUE,
                                  ignore_RAM=ignore_RAM, verbose=verbose)
      results <- list(data.raster, temp.map)
    }

  )

  data.raster <- results[[1]]
  temp.map <- results[[2]]

  #reduce data.all if resampling took place and not all timesteps are needed for quantiles
  if(!is.null(temp.map) & !all_data){
    data.all <- terra::as.array(terra::rast(temp.map))
  }
  #get dimensions of array
  dims <- dim(data.all)

  #check if huge data
  # warning for longer calculation
  if(length(dims)==4){
    size <- dims[1]*dims[2]*dims[3]*dims[4]
  }else if(length(dims)==3){
    size <- dims[1]*dims[2]*dims[3]
  }else if(length(dims)==2){
    size <- dims[1]*dims[2]
  }
  if (size > 100000000 & verbose){
    print('Wow that is huge! Maybe get a tea, the caluculation will take a while...')
  }

  #get quantiles ----
  s <- stats::quantile(data.all, probs = seq(0, 1, (1/classes)), na.rm=TRUE)

  #check if quantile list values are unique
  if(length(unique(s)) != (classes+1)){
    message('Error while creating quantiles. Color Scale will be corrupted.
            Most likely you will see less classes than you defined.')
    s <- unique(s)
  }

  #get reverses color----
  if(col_rev){
    direction <- 1
  } else{
    direction <- -1
  }

  #get correct colors ----

  if (min(s)<0 & max(s)>0){
    palette <- 'RdYlBu'
  } else {
    palette <- 'YlGn'
    direction <- direction * -1
  }

  #get x and y lab
  if(stringr::str_starts(prop@spatial$wkt2, 'GEOGCRS')){
    xlab <- 'latitude'
    ylab <- 'longitude'
  } else{
    xlab <- 'x coordinate'
    ylab <- 'y coordinate'
  }


  #define display options ----

  #plot with country outlines ----
  if (countries){
    #prepare data
    world_boundaries <- terra::vect(world_boundaries, geom='geometry', crs='EPSG:4326')

    if(epsg != 4326){
      world_boundaries <- terra::project(world_boundaries, paste0('EPSG:', epsg))
    }

    #crop world_boundaries to extent
    extent <- terra::ext(data.raster)
    world_boundaries <- terra::crop(world_boundaries, extent )

    print(
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = data.raster) +
        tidyterra::geom_spatvector(data = world_boundaries, fill = NA) +
        ggplot2::coord_sf(expand = FALSE)+
        ggplot2::ggtitle(paste(strwrap(
                             title,
                             width = 80
                             ), collapse = "\n"),
                         subtitle = subtitle) +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_fermenter(na.value=NA, palette = palette, breaks =  as.numeric(s),
                                      label = round(as.numeric(s),2),
                                      direction = direction,
                                      guide=ggplot2::guide_bins(title=units,
                                                                #even.steps = FALSE,
                                                                show.limits = TRUE,
                                                                reverse=!col_rev,
                                                                axis=F
                                      )
                                      # guide = ggplot2::guide_coloursteps(even.steps = FALSE
                                      #                                    ),
                                      )+
        ggplot2::ylab(ylab) +
        ggplot2::xlab(xlab)
    )

  } else{
    #plot without country outlines ----
    print(
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = data.raster) +
        ggplot2::coord_sf(expand = FALSE)+
        ggplot2::ggtitle(paste(strwrap(
                             title,
                             width = 80
                           ), collapse = "\n"),
                         subtitle = subtitle) +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_fermenter(na.value=NA, palette = palette, breaks =  as.numeric(s),
                                      label = round(as.numeric(s),2),
                                      direction = direction,
                                      guide=ggplot2::guide_bins(title=units,
                                                                #even.steps = FALSE,
                                                                show.limits = TRUE,
                                                                reverse=!col_rev,
                                                                axis=F
                                      )
                                      # guide = ggplot2::guide_coloursteps(even.steps = FALSE
                                      #                                    ),
        )+
        ggplot2::ylab(ylab) +
        ggplot2::xlab(xlab)
    )

  }

  #remove temporary file ----
  if (!is.null(temp.map)){
    if (file.exists(temp.map)){
      file.remove(temp.map)
    }
  }

}
