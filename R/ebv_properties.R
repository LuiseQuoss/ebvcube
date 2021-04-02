ebv_properties <- function(filepath, datacubepath = NULL, verbose=FALSE){
  if(verbose){
    options(warn = 0)
  }else{
    options(warn = -1)
  }

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

  if(!is.null(datacubepath)){
    #variable check
    hdf <- rhdf5::H5Fopen(filepath)
    if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
      rhdf5::H5Fclose(hdf)
      stop(paste0('The given variable is not valid:\n', datacubepath))
    } else {
      rhdf5::H5Fclose(hdf)
    }
  }
  ####initial tests end ----

  #### define S4 subclasses ----
  methods::setClass("temporal_info_EBV", slots=list(units="character", t_delta="character", timesteps="array", timesteps.natural = 'Date'))
  methods::setClass("spatial_info_EBV", slots=list(srs="CRS", epsg="numeric", resolution="numeric", extent="numeric", dimensions="numeric"))
  methods::setClass("entity_information", slots=list(longname="character", label="character", unit='character',
                                            type="character",  fillvalue="array"))
  methods::setClass("metric_information", slots=list(label="character" , description="character"))
  methods::setClass("scenario_information", slots=list(label="character", description="character"))


  #get resolution ----
  lat.data <- rhdf5::h5read(filepath, 'lat', start=1, count = 2)
  lon.data <- rhdf5::h5read(filepath, 'lon', start=1, count = 2)
  res.lon <- lon.data[1]-lon.data[2]
  res.lat <- lat.data[1]-lat.data[2]
  if (res.lon<0){
    res.lon <- res.lon*-1
  }
  if (res.lat<0){
    res.lat <- res.lat*-1
  }
  resolution <- c(res.lon, res.lat)

  #get extent
  lon.data <- rhdf5::h5read(filepath, 'lon')
  lat.data <- rhdf5::h5read(filepath, 'lat')
  xmin <- min(lon.data) - resolution[1]/2
  xmax <- max(lon.data) + resolution[1]/2
  ymin <- min(lat.data) - resolution[2]/2
  ymax <- max(lat.data) + resolution[2]/2
  extent <- c(xmin, xmax, ymin, ymax)

  #open file ----
  hdf <- rhdf5::H5Fopen(filepath)

  ####general properties of file ----
  #get depth of nested structure
  depth <- ebv_i_read_att(hdf, 'ebv_subgroups')
  if(!depth[1]==''){
    if('metric' %in% depth){
      metric <- TRUE
    }else {metric <- FALSE}
    if('scenario' %in% depth){
      scenario <- TRUE
    } else {scenario <- FALSE}
    if('entity' %in% depth){
      entity <- TRUE
    }else {entity <- FALSE}
  } else {
    metric<-TRUE
    scenario <-FALSE
    entity<-FALSE
  }

  #get title
  title <- ebv_i_read_att(hdf, 'title')[1]

  #get description
  description.file <- ebv_i_read_att(hdf, 'description')[1]

  #get value_range
  value_range <- ebv_i_read_att(hdf, 'value_range')
  if(is.null(value_range)){
    value_range <- array(NA)
  }

  #get srs ----
  srs.ds <- rhdf5::H5Dopen(hdf, 'crs')
  srs.chr <- ebv_i_read_att(srs.ds, 'spatial_ref')
  srs <- pkgcond::suppress_warnings(sp::CRS(srs.chr))
  rhdf5::H5Dclose(srs.ds)

  #get epsg
  #index <- length(strsplit(srs.chr, '"')[[1]])-1
  #epsg <- as.integer(strsplit(srs.chr, '"')[[1]][index])
  epsg <- as.integer(rgdal::showEPSG(srs.chr))

  #get dims
  dims <- c(dim(hdf$lat), dim(hdf$lon), dim(hdf$time))

  #create object of S4 class
  srs.s4 <- methods::new('spatial_info_EBV', srs=srs, epsg=epsg, resolution=resolution, extent=extent, dimensions=dims)

  #get time info ----
  add <- 40177
  dh <- rhdf5::H5Dopen(hdf, 'time')
  #time units
  time <- ebv_i_read_att(dh, 'units')[1]
  #time delta
  t_delta <- ebv_i_read_att(dh, 't_delta')[1]
  #timesteps
  timesteps <- rhdf5::h5read(hdf, 'time')
  #timesteps natural language
  time.natural <- as.Date(timesteps-add, origin='1970-01-01')

  #close data handle
  rhdf5::H5Dclose(dh)

  #create object of S4 class
  time.s4 <- methods::new('temporal_info_EBV', units=time, t_delta=t_delta, timesteps=timesteps, timesteps.natural=time.natural)


  ####variable specific properties ----
  if (!is.null(datacubepath)){
    #### sm(e) ----
    if(scenario & metric){

      ####get entity info ----
      dh <- hdf&datacubepath

      #get type
      info <- utils::capture.output(dh)
      indices <- stringr::str_locate(info, ' type')
      for (row in 1:dim(indices)[1]){
        if (!is.na(indices[row,1])){
          i <- c(row, indices[row,])
        }
      }
      type <- as.vector(info)[i[1]]
      type <- stringr::str_remove(type, 'type')
      type <- stringr::str_replace_all(type, stringr::fixed(" "), "")

      #get fillvalue
      fillvalue <- ebv_i_read_att(dh, '_FillValue')
      if(is.null(fillvalue)){
        fillvalue <- array(NA)
      }

      #long name
      long_name <- ebv_i_read_att(dh, 'long_name')[1]

      #label
      label <- ebv_i_read_att(dh, 'label')[1]

      #unit
      unit <- ebv_i_read_att(dh, 'units')[1]
      #only because of error in globES - delete when corrected!
      if (is.null(unit)){
        unit <- ebv_i_read_att(dh, 'unit')[1]
      }

      #close data handle
      rhdf5::H5Dclose(dh)

      #create object of S4 class
      entity.s4 <- methods::new('entity_information', longname=long_name, label=label, unit=unit,
                       type=type, fillvalue=fillvalue)

      ####get metric info ----
      #one level higher
      i <- length(stringr::str_split(datacubepath, '/')[[1]])
      group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
      dh <- rhdf5::H5Gopen(hdf, group)

      #label
      label <- ebv_i_read_att(dh, 'label')[1]

      #description
      description <- ebv_i_read_att(dh, 'description')[1]

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      metric.s4 <- methods::new('metric_information', label=label, description=description)

      ####get scenario info ----
      #two levels higher
      i <- length(stringr::str_split(datacubepath, '/')[[1]])
      group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
      i <- length(stringr::str_split(group, '/')[[1]])-1
      group <- stringr::str_remove(group, stringr::str_split(datacubepath, '/')[[1]][i])
      group <- stringr::str_remove(group, '/')
      dh <- rhdf5::H5Gopen(hdf, group)

      #label
      label <- ebv_i_read_att(dh, 'label')

      #description
      description <- ebv_i_read_att(dh, 'description')

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      scenario.s4 <- methods::new('scenario_information', description=description, label=label)

      #define new S4 class
      methods::setClass("EBV NetCDF variable properties", slots=list(title="character", description="character",
                                                            value_range="array",
                                                            spatial_information='spatial_info_EBV',
                                                            time_information='temporal_info_EBV',
                                                            scenario_information='scenario_information',
                                                            metric_information='metric_information',
                                                            entity_information='entity_information'))
      #create object of S4 class ----
      prop <- methods::new('EBV NetCDF variable properties', title=title, description=description.file, value_range=value_range,
                  spatial_information=srs.s4,
                  time_information=time.s4,
                  scenario_information=scenario.s4,
                  metric_information=metric.s4,
                  entity_information=entity.s4)

      # m(e) ----
    } else if (!scenario & metric){
      ####get entity info ----
      dh <- hdf&datacubepath

      #get type
      info <- utils::capture.output(dh)
      indices <- stringr::str_locate(info, ' type')
      for (row in 1:dim(indices)[1]){
        if (!is.na(indices[row,1])){
          i <- c(row, indices[row,])
        }
      }
      type <- as.vector(info)[i[1]]
      type <- stringr::str_remove(type, 'type')
      type <- stringr::str_replace_all(type, stringr::fixed(" "), "")

      #get fillvalue
      fillvalue <- ebv_i_read_att(dh, '_FillValue')
      if(is.null(fillvalue)){
        fillvalue <- array(NA)
      }

      #long name
      long_name <- ebv_i_read_att(dh, 'long_name')[1]

      #label
      label <- ebv_i_read_att(dh, 'label')[1]

      #unit
      unit <- ebv_i_read_att(dh, 'units')[1]

      #close data handle
      rhdf5::H5Dclose(dh)

      #create object of S4 class
      entity.s4 <- methods::new('entity_information', longname=long_name, label=label, unit=unit,
                       type=type, fillvalue=fillvalue)

      ####get metric info ----
      #one level higher
      i <- length(stringr::str_split(datacubepath, '/')[[1]])
      group <- stringr::str_remove(datacubepath, stringr::str_split(datacubepath, '/')[[1]][i])
      dh <- rhdf5::H5Gopen(hdf, group)

      #label
      label <- ebv_i_read_att(dh, 'label')[1]

      #description
      description <- ebv_i_read_att(dh, 'description')[1]

      #close data handle
      rhdf5::H5Gclose(dh)

      #create object of S4 class
      metric.s4 <- methods::new('metric_information', label=label, description=description)

      #define new S4 class
      methods::setClass("EBV NetCDF variable properties", slots=list(title='character', description='character',
                                                            value_range='array',
                                                            spatial_information='spatial_info_EBV',
                                                            time_information='temporal_info_EBV',
                                                            metric_information='metric_information',
                                                            entity_information='entity_information'))
      #create object of S4 class ----
      prop <- methods::new('EBV NetCDF variable properties', title=title, description=description.file, value_range=value_range,
                  spatial_information=srs.s4,
                  time_information=time.s4, metric_information=metric.s4,
                  entity_information=entity.s4)

    } else{
      # e ----
      #only for hennekens! - delete when hennekens is deleted!
      ####get entity info ----
      dh <- hdf&datacubepath

      #get type
      info <- utils::capture.output(dh)
      indices <- stringr::str_locate(info, ' type')
      for (row in 1:dim(indices)[1]){
        if (!is.na(indices[row,1])){
          i <- c(row, indices[row,])
        }
      }
      type <- as.vector(info)[i[1]]
      type <- stringr::str_remove(type, 'type')
      type <- stringr::str_replace_all(type, stringr::fixed(" "), "")

      #get fillvalue
      fillvalue <- ebv_i_read_att(dh, '_FillValue')
      if(is.null(fillvalue)){
        fillvalue <- array(NA)
      }

      #long name
      long_name <- ebv_i_read_att(dh, 'long_name')[1]

      #label
      label <- ebv_i_read_att(dh, 'label')[1]

      #unit
      unit <- ebv_i_read_att(dh, 'units')[1]

      #close data handle
      rhdf5::H5Dclose(dh)

      #create object of S4 class
      entity.s4 <- methods::new('entity_information', longname=long_name, label=label, unit=unit,
                       type=type, fillvalue=fillvalue)

      #define new S4 class
      methods::setClass("EBV NetCDF variable properties", slots=list(title='character', description='character',
                                                            value_range = 'array',
                                                            spatial_information='spatial_info_EBV',
                                                            time_information='temporal_info_EBV',
                                                            entity_information='entity_information'))
      #create object of S4 class ----
      prop <- methods::new('EBV NetCDF variable properties', title=title, description=description.file, value_range=value_range,
                  spatial_information=srs.s4,
                  time_information=time.s4, entity_information=entity.s4)

    }
    # define S4 for filepath only ----
  } else {
    #define new S4 class
    methods::setClass("EBV NetCDF file properties", slots=list(title="character", description="character", value_range='array', spatial_information='spatial_info_EBV', time_information='temporal_info_EBV'))
    #create object of S4 class
    prop <- methods::new('EBV NetCDF file properties', title=title, description=description.file, value_range=value_range, spatial_information=srs.s4, time_information=time.s4)
  }

  #close file
  rhdf5::H5Fclose(hdf)

  #reset global warnings
  if(! verbose){
    options(warn = 0)
  }

  return(prop)
}
