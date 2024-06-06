#'Create an EBV netCDF with taxonomy
#'
#'@description Create the core structure of the EBV netCDF based on the json
#'  from the \href{https://portal.geobon.org}{EBV Data Portal}. Additionally,
#'  you can add the hierarchy of the taxonomy. This is not provided in the
#'  [ebvcube::ebv_create()] function. Use the [ebvcube::ebv_create()] function
#'  if your dataset holds no taxonomic information. Data will be added
#'  afterwards using [ebvcube::ebv_add_data()].
#'
#'@param jsonpath Character. Path to the json file downloaded from the EBV Data
#'  Portal. Login to the page and click on 'Uploads' and 'New Upload' to start
#'  the process.
#'@param outputpath Character. Set path where the netCDF file should be created.
#'@param taxonomy Character. Path to the csv table holding the taxonomy.
#'  Default: comma-separated delimiter, else change the `sep` argument
#'  accordingly. The csv needs to have the following structure: The header
#'  displays the names of the different taxon levels ordered from the highest
#'  level to the lowest, e.g. "Order", "Family", "Genus", "Species". The last
#'  column (if `lsid`=FALSE) is equivalent to the `entity` argument in the
#'  [ebvcube::ebv_create()] function. Each row of the csv corresponds to a
#'  unique entity. In case the `lsid` argument (see below) is set to the TRUE,
#'  this table gets an additional last column which holds the lsid per entity -
#'  in this case the second last column contains the entity names, e.g. the
#'  following column order: "Order", "Family", "Genus", "Species", "lsid".
#'@param lsid Logical. Default: FALSE. Set to TRUE if the last column in your
#'  taxonomy csv file defines the lsid for each entity. For more info check
#'   \href{https://cfconventions.org/Data/cf-conventions/cf-conventions-1.8/cf-conventions.html#taxon-names-and-identifiers}{CF
#'   convention 1.8: Taxon Names and Identifiers}.
#'@param epsg Integer. Default: 4326 (WGS84). Defines the coordinate reference
#'  system via the corresponding epsg code.
#'@param extent Numeric. Default: c(-180,180,-90,90). Defines the extent of the
#'  data: c(xmin, xmax, ymin, ymax).
#'@param resolution Numerical. Vector of two numerical values defining the
#'  longitudinal and latitudinal resolution of the pixel: c(lon,lat).
#'@param timesteps Character. Vector of the timesteps in the dataset. Default:
#'  NULL - in this case the time will be calculated from the start-, endpoint
#'  and temporal resolution given in the metadata file (json). Else, the dates
#'  must be given in in ISO format 'YYYY-MM-DD' or shortened 'YYYY' in case of
#'  yearly timesteps.
#'@param fillvalue Numeric. Value of the missing data in the array. Not
#'  mandatory but should be defined!
#'@param prec Character. Default: 'double'. Precision of the data set. Valid
#'  options: 'short' 'integer' 'float' 'double' 'char' 'byte'.
#'@param sep Character. Default: ','. If the delimiter of the csv specifying the
#'  entity-names differs from the default, indicate here.
#'@param force_4D Logical. Default is TRUE. If the argument is TRUE, there will
#'  be 4D cubes (lon, lat, time, entity) per metric. If this argument is changed
#'  to FALSE, there will be 3D cubes (lon, lat, time) per entity (per metric).
#'  So the latter yields a higher amount of cubes and does not bundle all
#'  information per metric. In the future the standard will be restricted to the
#'  4D version. Recommendation: go with the 4D cubes!
#'@param overwrite Logical. Default: FALSE. Set to TRUE to overwrite the output
#'  file defined by 'outputpath'
#'@param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'  it to FALSE. #' @note To check out the results take a look at your netCDF
#'  file with \href{https://www.giss.nasa.gov/tools/panoply/}{Panoply} provided
#'  by the NASA.
#'
#'@note You can check the taxonomy info with [ebvcube::ebv_properties()] in the slot
#'  'general' under the name 'taxonomy' and 'taxonomy_lsid'.
#'
#'@return Creates the netCDF file at the 'outputpath' location including the
#'  taxonomy information.
#'@export
#'
#'@importFrom utils head
#'
#' @examples
#' #set path to JSON file
#' json <- system.file(file.path("extdata/testdata","5.json"), package="ebvcube")
#' #set output path of the new EBV netCDF
#' out <-  tempfile(fileext='.nc')
#' #set path to the csv holding the taxonomy names
#' taxonomy <- file.path(system.file(package='ebvcube'),"extdata/testdata","id5_entities.csv")
#'
#' #create new EBV netCDF with taxonomy
#' \dontrun{
#' ebv_create_taxonomy(jsonpath = json, outputpath = out, taxonomy = taxonomy,
#'            fillvalue = -127, resolution = c(0.25, 0.25), verbose = FALSE)
#' #remove file
#' file.remove(out)
#' }
ebv_create_taxonomy <- function(jsonpath, outputpath, taxonomy, lsid=FALSE,
                                epsg = 4326, extent = c(-180,180,-90,90), resolution = c(1,1),
                                timesteps = NULL, fillvalue = NULL, prec = 'double',
                                sep=',', force_4D = TRUE, overwrite = FALSE,
                                verbose = TRUE){

  # start initial tests ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )
  gids <- c('mgid', 'sgid', 'mid')
  withr::defer(
    for (id in gids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Gclose(id)}
      }
    }
  )
  dids <- c('crs.id', 'lat.id', 'lon.id', 'time.id', 'did', 'entity.id')
  withr::defer(
    for (id in dids){
      if(exists(id)){
        id <- eval(parse(text = id))
        if(rhdf5::H5Iis_valid(id)==TRUE){rhdf5::H5Dclose(id)}
      }
    }
  )
  withr::defer(
    if(exists('nc')){
      tryCatch(l <- utils::capture.output(ncdf4::nc_close(nc)))
    }
  )
  withr::defer(
    if(exists('nc_test')){
      tryCatch(l <- utils::capture.output(ncdf4::nc_close(nc_test)))
    }
  )

  #ensure that all tempfiles are deleted on exit
  withr::defer(
    if(exists('temp')){
      if(file.exists(temp)){
        file.remove(temp)
      }
    }
  )
  withr::defer(
    if(exists('csv_txt')){
      rm(csv_txt)
    }
  )

  #set UTF8 encoding
  withr::local_options(list(encoding = "UTF-8"))

  #are all arguments given?
  if(missing(jsonpath)){
    stop('Jsonpath argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }
  if(missing(taxonomy)){
    stop('Taxonomy argument is missing.')
  }

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=FALSE) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(force_4D, len=1, any.missing=FALSE) != TRUE){
    stop('force_4D must be of type logical.')
  }

  #check if json exists
  if (checkmate::checkCharacter(jsonpath) != TRUE){
    stop('Filepath (JSON) must be of type character.')
  }
  if (checkmate::checkFileExists(jsonpath) != TRUE){
    stop(paste0('Json file does not exist.\n', jsonpath))
  }
  if (!(endsWith(jsonpath, '.json') || endsWith(jsonpath, '.js'))){
    stop(paste0('Json file ending is wrong. File cannot be processed.'))
  }

  #check if ouputpath exists
  #outputpath check
  if (checkmate::checkCharacter(outputpath) != TRUE){
    stop('Outputpath must be of type character.')
  }
  if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  if(!endsWith(outputpath, '.nc')){
    stop('Outputpath needs to end with *.nc ')
  }
  #check if outpufile exists if overwrite is disabled
  if(!overwrite){
    if(checkmate::checkPathForOutput(outputpath) != TRUE){
      stop('Output file already exists. Change name or enable overwrite.')
    }
  }

  #check if epsg is valid
  crs_wkt <- ebv_i_eval_epsg(epsg)

  #check extent
  if (checkmate::checkNumeric(extent, len = 4) != TRUE){
    stop('extent needs to be a list of 4 numeric values.')
  }

  #check taxonomy
  if (checkmate::checkCharacter(taxonomy) != TRUE){
    stop('Taxonomy must be of type character.')
  }
  if(checkmate::checkCharacter(taxonomy, len=1) != TRUE){
    #length longer than 1 -> return error
    stop('Taxonomy must be of length 1.')
  }else{
    # check csv
      if (checkmate::checkFileExists(taxonomy) != TRUE){
        stop(paste0('Taxonomy csv file does not exist.\n', taxonomy))
      }
      if (!endsWith(taxonomy, '.csv')){
        stop(paste0('Taxonomy file ending is wrong. File cannot be processed. Must be *.csv.'))
      }
      #read csv---
      # check if data inside
      tryCatch({
        csv_txt <- suppressWarnings(utils::read.csv(taxonomy, sep=sep, header=TRUE, fileEncoding="UTF-8"))
      },
      error=function(e){
        if(stringr::str_detect(as.character(e), 'no lines available')){
          stop('Empty csv table given for taxonomy')
        } else {
          stop(paste0('Could not read csv (taxonomy):\n', as.character(e)))
        }
      })
  }

  #check prec
  if (! prec %in% c('short', 'integer', 'float', 'double', 'char', 'byte')){
    stop('prec value not valid!')
  }

  #check fillvalue
  if (! is.null(fillvalue)){
    if(checkmate::checkNumber(fillvalue) != TRUE && !is.na(fillvalue)){
      stop('The fillvalue needs to be a single numeric value or NA.')
    }
  }

  #check resolution
  if (checkmate::checkNumeric(resolution, len = 2) != TRUE){
    stop('resolution needs to be a list of 2 numeric values.')
  }

  #get temp directory
  temp_path <- tempdir()

  #read json ----
  file <- jsonlite::fromJSON(txt=jsonpath)
  #json root
  json <- file$data

  #check timesteps----
  t_res <- json$time_coverage$time_coverage_resolution

  if(!is.null(timesteps) && t_res != 'Paleo'){
    if (checkmate::checkCharacter(timesteps) != TRUE){
      stop('timesteps needs to be a list of character values.')
    }else {
      for(ts in timesteps){
        #check ISO format
        if(!(grepl('^\\d{4}-\\d{2}-\\d{2}$', ts) || grepl('^\\d{4}$',ts))){
          stop(paste0('Your timestep ', ts, ' is not following the indicated ISO format. Check help page for more information.'))
        }

      }
    }
  }

  # end initial tests ----

  #overwrite --> delete file ---
  if (file.exists(outputpath) && overwrite==TRUE){
    tryCatch(file.remove(outputpath),
             warning = function(w){
               temp <- stringr::str_remove(as.character(w), '\\\\')
               if(stringr::str_detect(temp,'cannot remove file')){
                 stop('Outputpath file already exists and you enabled overwrite, but file cannot be overwritten. Most likely the file is opened in another application.')
               }
             })

  }

  # read taxonomy info ----
  dim_csv <- dim(csv_txt)

  #get entities and maybe lsid-list
  if(lsid){
    entities <- csv_txt[,(dim_csv[2]-1)]
    lsid_list <- csv_txt[,dim_csv[2]]
    csv_txt <- csv_txt[,-dim_csv[2]]
    taxon_list <- names(csv_txt)
  }else{
    taxon_list <- names(csv_txt)
    entities <- csv_txt[,dim_csv[2]]
    lsid_list <- NA
  }

  #check lsid list and entities
  if(lsid && (length(lsid_list)!=length(entities))){
    stop(paste0('The length of your lsid-list differs from the length of the entities:\n',
                'length lsid: ', length(lsid),
                '\nlength entities: ', length(entities)))
  }

  # get basic hierarchy info ----
  metrics_no <- length(json$ebv_metric)
  entities_no <- length(entities)
  scenarios_no <- length(json$ebv_scenario)-3
  taxon_no <- length(taxon_list)
  if (scenarios_no==1){
    if(ebv_i_empty(file$data$ebv_scenario[[1]]) || file$data$ebv_scenario[[1]]=='N/A')
      scenarios_no <- 0
  } else if(scenarios_no<0){
    scenarios_no <- 0
  }

  # get crs information ----
  # :GeoTransform
  res <- resolution
  geo_trans <- paste0(extent[1], " ",res[1]," 0.0 ", extent[4], " 0.0 -", res[2])

  # :spatial_ref
  #remove additional whitespaces
  crs_temp <- stringr::str_replace_all(crs_wkt, '\n', ' ')
  crs_temp <- stringr::str_replace_all(crs_temp, '         ', ' ')
  crs_ref <- stringr::str_replace_all(crs_temp, '     ', ' ')

  # unit
  if(stringr::str_detect(crs_ref,'PROJCRS')){
    crs_unit <- 'meter'
  } else{
    crs_unit <- 'degree'
  }

  # get dimensions ----
  # time ----
  t_start <- json$time_coverage$time_coverage_start
  t_end <- json$time_coverage$time_coverage_end

  #get ISO timesteps for irregular and paleo -> shiny app
  if(t_res=='Irregular'){
    if(is.null(timesteps)){
      timesteps <- json$timesteps[[1]]
    }
    if(!is.null(timesteps)){
      if(timesteps[1]=='N/A'){
        timesteps <- NULL
      }
    }
  }

  if(t_res=='Paleo'){
    if(is.null(timesteps)){
      timesteps <- json$timesteps[[1]]
    }
    if(!is.null(timesteps)){
      if(timesteps[1]=='N/A'){
        timesteps <- NULL
      }
    }
    if(!is.null(timesteps)){
      timesteps <- as.numeric(timesteps)
      timesteps <- sort(timesteps, decreasing = TRUE)
    }
  }

  #create integer timesteps
  add <- 40177

  #calculate timesteps
  if(is.null(timesteps) && t_res!='Paleo'){
    if(t_res=="P0000-00-00"){
      #one timestep only
      #check
      if(t_start!=t_end && verbose){
        warning('Your dataset has one timestep only based on the temporal resolution attribute but your given start and end date are different. Note: the end date will be applied to the dataset.')
      }
      date <- as.numeric(as.Date(t_end))
      timesteps <- date+add
    }else if(grepl('^P\\d{4}-?\\d{0,2}-?\\d{0,2}$', t_res)){
      #process ISO standard PYYYY-MM-YY or short PYYYY
      y <- stringr::str_split(stringr::str_remove(t_res, 'P')[[1]], '-')[[1]][1]
      m <- stringr::str_split(stringr::str_remove(t_res, 'P')[[1]], '-')[[1]][2]
      d <- stringr::str_split(stringr::str_remove(t_res, 'P')[[1]], '-')[[1]][3]
      if(as.numeric(y)!= 0){
        sequence <- seq.Date(from = as.Date(t_start),
                             to = as.Date(t_end),
                             by = paste0(y, ' year'))
      } else if(as.numeric(m)!= 0){
        sequence <- seq.Date(from = as.Date(t_start),
                             to = as.Date(t_end),
                             by = paste0(m, ' month'))
      }else if(as.numeric(d)!= 0){
        sequence <- seq.Date(from = as.Date(t_start),
                             to = as.Date(t_end),
                             by = paste0(d, ' day'))
      }

      #timestep values
      timesteps <- c()
      for (s in sequence){
        date <- as.numeric(s)
        timestep <- date+add
        timesteps <- c(timesteps, timestep)
      }

    }else{
      #process old standard
      if (mapply(grepl,'year',t_res,ignore.case=TRUE)){
        start <- as.integer(stringr::str_split(t_start, '-')[[1]][1])
        end <- as.integer(stringr::str_split(t_end, '-')[[1]][1])
        intervall <- as.numeric(regmatches(t_res, gregexpr("[[:digit:]]+", t_res))[[1]][1])
        if(is.na(intervall)){ #yearly
          intervall <- 1
        }
        sequence <- seq(start, end, intervall)
        timesteps <- c()
        for (s in sequence){
          date <- as.numeric(as.Date(paste0(as.character(s),'-01-01'), format = '%Y-%m-%d'))
          timestep <- date+add
          timesteps <- c(timesteps, timestep)
        }
      } else if (mapply(grepl,'month',t_res,ignore.case=TRUE)){
        start <- as.Date(t_start)
        end   <- as.Date(t_end)
        sequence <- seq(from=start, to=end,by='month')
        timesteps <- c()
        for (s in sequence){
          date <- as.numeric(as.Date(s,origin=as.Date("1970-01-01")))
          timestep <- s+add
          timesteps <- c(timesteps, timestep)
        }
      }else if (mapply(grepl,'day',t_res,ignore.case=TRUE)){
        start <- as.Date(t_start)
        end   <- as.Date(t_end)
        sequence <- seq(from=start, to=end,by='days')
        timesteps <- c()
        for (s in sequence){
          date <- as.numeric(as.Date(s,origin=as.Date("1970-01-01")))
          timestep <- s+add
          timesteps <- c(timesteps, timestep)
        }
      } else if (mapply(grepl,'decad',t_res,ignore.case=TRUE)){
        start <- as.integer(stringr::str_split(t_start, '-')[[1]][1])
        end <- as.integer(stringr::str_split(t_end, '-')[[1]][1])
        intervall <- 10
        sequence <- seq(start, end, intervall)
        timesteps <- c()
        for (s in sequence){
          date <- as.numeric(as.Date(paste0(as.character(s),'-01-01'), format = '%Y-%m-%d'))
          timestep <- date+add
          timesteps <- c(timesteps, timestep)
        }
      } else if (mapply(grepl,'annually',t_res,ignore.case=TRUE)){
        start <- as.integer(stringr::str_split(t_start, '-')[[1]][1])
        end <- as.integer(stringr::str_split(t_end, '-')[[1]][1])
        intervall <- 1
        sequence <- seq(start, end, intervall)
        timesteps <- c()
        for (s in sequence){
          date <- as.numeric(as.Date(paste0(as.character(s),'-01-01'), format = '%Y-%m-%d'))
          timestep <- date+add
          timesteps <- c(timesteps, timestep)
        }
      } else {
        warning('Could not detect delta time. Empty time dataset created')
        timesteps <- c(0)
      }
    }
  }else if (t_res != 'Paleo'){
    #take given timesteps and transform them into integer values
    temp_temp <- c()
    for (ts in timesteps){
      if(!grepl('d{4}-\\d{2}-\\d{2}$', ts)){
        ts <- paste0(ts, '-01-01')
        date <- as.numeric(as.Date(ts))
        temp_temp <- c(temp_temp, date+add)
      }else{
        date <- as.numeric(as.Date(ts))
        temp_temp <- c(temp_temp, date+add)
      }
    }
    timesteps <- temp_temp
  }
  #if no timesteps are presented anywhere, throw error
  if(is.null(timesteps)){
    stop('There are no timesteps given. Define the argument "timesteps", to create your EBV netCDF.')
  }


  # lat ----
  res <- as.numeric(res)
  lat.min <- extent[3]
  lat.max <- extent[4]
  lat_data <- seq((lat.min+(res[2]/2)), (lat.max-(res[2]/2)), res[2])
  lat_data <- rev(lat_data)

  # lon ----
  lon.min <- extent[1]
  lon.max <- extent[2]
  lon_data <- seq((lon.min+(res[1]/2)),(lon.max-(res[1]/2)), res[1])

  # entities ----
  if(!entities_no==0){
    #create entity list
    entity.list <- c()
    for (e in 1:(entities_no)){
      ent <- paste0('entity_', as.character(e))
      entity.list <- c(entity.list, ent)
    }
  } else {
    entity.list <- c('data')
  }

  # create dimensions ----
  lat_dim <- ncdf4::ncdim_def('lat', crs_unit , vals = lat_data)
  lon_dim <- ncdf4::ncdim_def('lon', crs_unit, vals = lon_data)
  if(t_res=='Paleo'){
    time_dim <- ncdf4::ncdim_def('time', 'kyrs B.P.' , timesteps, unlim = TRUE)#HERE
  }else{
    time_dim <- ncdf4::ncdim_def('time', 'days since 1860-01-01 00:00:00.0' , timesteps, unlim = TRUE)#HERE
  }
  entity_dim <- ncdf4::ncdim_def('entity', '', vals = 1:entities_no, create_dimvar=FALSE)
  taxon_dim <- ncdf4::ncdim_def('taxonlevel', '', vals = 1:taxon_no, create_dimvar=FALSE)

  # create list of vars 3D----
  if(force_4D==FALSE){
    var_list <- c()
    # 1. metric, no scenario
    if(scenarios_no==0){
      for (j in 1:(metrics_no)){
        #all entities for that metric
        for (ent in entity.list){
          var_list <- c(var_list, paste0('metric_', as.character(j), '/', ent))
        }
      }
      #2. scenario and metric (entities are not relevant)
    } else {
      for (i in 1:(scenarios_no)){
        for (j in 1:(metrics_no)){
          #add entities
          for (ent in entity.list){
            var_list <- c(var_list, paste0('scenario_', as.character(i), '/metric_', as.character(j), '/',ent))
          }
        }
      }
    }
  } else{
    # create list of vars 4D----
    var_list <- c()
    # 1. metric, no scenario
    if(scenarios_no==0){
      for (j in 1:(metrics_no)){
        #add ebv_cube
        var_list <- c(var_list, paste0('metric_', as.character(j), '/ebv_cube'))
      }
      #2. scenario and metric (entities are not relevant)
    } else {
      for (i in 1:(scenarios_no)){
        #create scenario group
        ending.s <- as.character(i)
        for (j in 1:(metrics_no)){
          #create metric group
          ending.m <- as.character(j)
          #add ebv_cube
          var_list <- c(var_list, paste0('scenario_', ending.s, '/metric_', ending.m, '/ebv_cube'))
        }
      }
    }
  }

  #get units of metric ----
  units <- c()
  for (j in 1:(metrics_no)){
    #metric units list
    units <- c(units,eval(parse(text=paste0('json$ebv_metric$ebv_metric_',j,'$`:units`'))))
  }

  var_list_nc <- list()
  enum <- 1

  #check shuffle
  if(prec=='integer' || prec=='short'){
    shuffle <- TRUE
  } else{
    shuffle <- FALSE
  }

  #define chunksize----
  #create one 3D var to detect default chunksize
  #aim: do not chunk along entities but time!
  if(force_4D){
    #create temporary 3D file
    temp <- file.path(temp_path, 'ebv_chunksize_3d_test.nc')
    test_def <- ncdf4::ncvar_def(name = 'test_var', units = 'some units',
                                 dim= list(lon_dim, lat_dim, time_dim),
                                 compression=5, prec=prec,
                                 verbose=FALSE, shuffle=shuffle)
    nc_test <- ncdf4::nc_create(filename = temp,
                                vars = test_def,
                                force_v4 = TRUE,
                                verbose = FALSE)
    ncdf4::nc_close(nc_test)
    #read out chunksize definition
    nc_test <- ncdf4::nc_open(temp)
    chunksizes_old <- nc_test$var$test_var$chunksizes
    ncdf4::nc_close(nc_test)
    #define chunksize
    chunksizes_new <- c(chunksizes_old,1)
    #remove temp file
    if(file.exists(temp)){
      file.remove(temp)
    }
  }

  # create all vars 3D ----
  if(force_4D==FALSE){
    if (!is.null(fillvalue)){
      for (var in var_list){
        metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
        metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
        name <- paste0('var', enum)
        assign(name, ncdf4::ncvar_def(name = var, units = units[metric.digit],
                                      dim= list(lon_dim, lat_dim, time_dim),
                                      missval=fillvalue, compression=5,
                                      prec=prec, verbose=verbose, shuffle=shuffle
        ))
        var_list_nc[[enum]] <- eval(parse(text=name))
        enum <- enum +1
      }
    } else {
      for (var in var_list){
        metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
        metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
        name <- paste0('var', enum)
        assign(name, ncdf4::ncvar_def(name = var, units = units[metric.digit],
                                      dim= list(lon_dim, lat_dim, time_dim),
                                      compression=5, prec=prec,
                                      verbose=verbose, shuffle=shuffle
        ))
        var_list_nc[[enum]] <- eval(parse(text=name))
        enum <- enum +1
      }
    }
  }else{
    # create all vars 4D ----
    if (!is.null(fillvalue)){
      for (var in var_list){
        metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
        metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
        name <- paste0('var', enum)
        assign(name, ncdf4::ncvar_def(name = var, units = as.character(units[metric.digit]),
                                      dim= list(lon_dim, lat_dim, time_dim, entity_dim),
                                      missval=fillvalue, compression=5, prec=prec,
                                      verbose=verbose, shuffle=shuffle,
                                      chunksizes=chunksizes_new))
        var_list_nc[[enum]] <- eval(parse(text=name))
        enum <- enum +1
      }
    } else {
      for (var in var_list){
        metric.str <- stringr::str_split(var, '/')[[1]][stringr::str_detect(stringr::str_split(var, '/')[[1]], 'metric')]
        metric.digit <- as.numeric(regmatches(metric.str, gregexpr("[[:digit:].]+", metric.str))[[1]])
        name <- paste0('var', enum)
        assign(name, ncdf4::ncvar_def(name = var, units = as.character(units[metric.digit]),
                                      dim= list(lon_dim, lat_dim, time_dim, entity_dim),
                                      compression=5, prec=prec,
                                      verbose=verbose, shuffle=shuffle,
                                      chunksizes=chunksizes_new))
        var_list_nc[[enum]] <- eval(parse(text=name))
        enum <- enum +1
      }
    }
  }

  #add crs variable ----
  var_list_nc[[enum]] <- ncdf4::ncvar_def(name = 'crs', units = '',
                                          dim= list(),
                                          prec='char', verbose=verbose)

  enum <- enum+1
  #check for special characters
  sz <- c()
  for (u in c('\ufc', '\uf6', '\ue4', '\udf', '\udc', '\uc4', '\ud6')){
    if(any(stringr::str_detect(entities,u))){
      sz <- c(sz, u)
    }
  }
  if (!ebv_i_empty(sz)){
    message(paste0('Your entity names (csv) encompasses the following special characters: ', paste(sz, collapse = ' '),
                   '. Please change these as they will not be stored correctly!'))
  }

  #add entities variable ----
  max_char_entity <- max(apply(csv_txt, 2, function(x) max(nchar(x))))
  dimchar_entity <- ncdf4::ncdim_def("nchar", "", 1:max_char_entity, create_dimvar=FALSE)
  #entity
  var_list_nc[[enum]] <- ncdf4::ncvar_def(name = 'entity', unit='1', #HERE adimensional
                                          dim=list(dimchar_entity,entity_dim),
                                          prec='char', verbose = verbose)
  enum <- enum+1
  #add entity_list variable ----
  var_list_nc[[enum]] <- ncdf4::ncvar_def(name = 'entity_list', unit='1', #HERE adimensional
                                          dim=list(taxon_dim, entity_dim, dimchar_entity),
                                          prec='char', verbose = verbose)
  enum <- enum+1

  # add entity_levels variable ----
  max_char_taxonlevel <- max(nchar(taxon_list))
  dimchar_taxonlevel <- ncdf4::ncdim_def("nchar_taxonlist", "", 1:max_char_taxonlevel, create_dimvar=FALSE)
  var_list_nc[[enum]] <- ncdf4::ncvar_def(name = 'entity_levels', unit='1', #HERE adimensional
                                          dim=list(taxon_dim, dimchar_taxonlevel),
                                          prec='char', verbose = verbose)
  enum <- enum+1

  # add entity_ids variable ----
  if(lsid){
    max_char_lsid <- max(nchar(lsid_list))
    dimchar_lsid <- ncdf4::ncdim_def("nchar_lsid", "", 1:max_char_lsid, create_dimvar=FALSE)
    var_list_nc[[enum]] <- ncdf4::ncvar_def(name = 'entity_lsid', unit='1', #HERE adimensional
                                            dim=list(entity_dim, dimchar_lsid),
                                            prec='char', verbose = verbose)
  }

  # add all vars ----
  # also creates groups
  nc <- ncdf4::nc_create(filename = outputpath,
                         vars = var_list_nc,
                         force_v4 = TRUE,
                         verbose = verbose)

  # close file
  ncdf4::nc_close(nc)

  # use hdf5 to add all attributes ----
  # open file
  hdf <- rhdf5::H5Fopen(outputpath)

  # global attributes ----
  #static attributes
  ebv_i_char_att(hdf, 'Conventions', 'CF-1.8, ACDD-1.3, EBV-1.0')
  ebv_i_char_att(hdf, 'naming_authority', 'The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig')
  ebv_i_char_att(hdf, 'date_issued', 'pending')
  ebv_i_char_att(hdf, 'history', paste0('EBV netCDF created using ebvcube, ', Sys.Date()))
  ebv_i_char_att(hdf, 'ebv_vocabulary', 'https://portal.geobon.org/api/v1/ebv')
  if(force_4D){
    ebv_i_char_att(hdf, 'ebv_cube_dimensions', 'lon, lat, time, entity')
  } else{
    ebv_i_char_att(hdf, 'ebv_cube_dimensions', 'lon, lat, time')
  }

  #dynamic attributes
  {
    global.att <- list()
    global.att['title'] <- 'title'
    global.att['id'] <- 'preliminary_id'
    global.att['summary'] <- 'summary'
    global.att['references'] <- 'references'
    global.att['source'] <- 'source'
    global.att['project_name'] <- 'project'
    global.att['project_url'] <- 'project_url'
    global.att['date_created'] <- 'date_created'
    global.att['creator_name'] <- 'creator$creator_name'
    global.att['creator_institution'] <- 'creator$creator_institution'
    global.att['creator_email'] <- 'creator$creator_email'
    global.att['license'] <- 'license'
    global.att['contributor_name'] <- 'contributor_name'
    global.att['publisher_name'] <- 'publisher$publisher_name'
    global.att['publisher_institution'] <- 'publisher$publisher_institution'
    global.att['publisher_email'] <- 'publisher$publisher_email'
    global.att['comment'] <- 'comment'
    global.att['ebv_class']<-'ebv$ebv_class'
    global.att['ebv_name']<-'ebv$ebv_name'
    global.att['ebv_spatial_scope']<-'ebv_geospatial$ebv_geospatial_scope'
    global.att['ebv_spatial_description']<-'ebv_geospatial$ebv_geospatial_description'
    global.att['ebv_domain']<-'ebv_domain'
  }

  #keywords
  keywords <- paste0('ebv_class: ', json$ebv$ebv_class, ', ebv_name: ', json$ebv$ebv_name,
                     ', ebv_domain: ', paste0(json$ebv_domain[[1]], collapse=', '), ', ebv_spatial_scope: ',
                     json$ebv_geospatial$ebv_geospatial_scope, ', ebv_entity_type: ',
                     json$ebv_entity$ebv_entity_type)

  if(scenarios_no > 0){
    global.att['ebv_scenario_classification_name']<-'ebv_scenario$ebv_scenario_classification_name'
    global.att['ebv_scenario_classification_version']<-'ebv_scenario$ebv_scenario_classification_version'
    global.att['ebv_scenario_classification_url']<-'ebv_scenario$ebv_scenario_classification_url'
    keywords <- paste0(keywords, ', ebv_scenario_classification_name: ',
                       json$ebv_scenario$ebv_scenario_classification_name)
  }


  #terranova datasets
  if(!is.null(json$terranova_type)){
    keywords <- paste0(keywords, ', terranova_type: ', json$terranova_type)
  }

  ebv_i_char_att(hdf, 'keywords', keywords)

  #add global.att to netcdf
  for (i in 1:length(global.att)){
    att.txt <- eval(parse(text = paste0('json$', global.att[i][[1]])))
    att.txt <- paste0(trimws(att.txt), collapse = ', ')
    if(names(global.att[i])=='contributor_name' || names(global.att[i])=='ebv_domain'){
      att.txt <- paste0(trimws(trimws(stringr::str_split(att.txt,',')[[1]])), collapse = ', ')
    }
    ebv_i_char_att(hdf, names(global.att[i]), att.txt)
  }

  #double check id - final jsons don't have 'preliminary_id' att
  id <- json$preliminary_id
  if(is.null(id)){
    id <- json$id
    ebv_i_char_att(hdf, 'id', id)
  }

  #geospatial attributes
  #bounds
  xmin <- min(lon_data) - res[1]/2
  xmax <- max(lon_data) + res[1]/2
  ymin <- min(lat_data) - res[2]/2
  ymax <- max(lat_data) + res[2]/2
  bounds <- paste0('POLYGON((', xmin, ' ' , ymin, ', ', xmin, ' ' , ymax, ', ',
                   xmax, ' ' , ymax, ', ', xmax, ' ' , ymin, ', ',
                   xmin, ' ' , ymin,'))')
  #lat and lon
  if(stringr::str_detect(epsg, 'ESRI')){
    ebv_i_char_att(hdf, 'geospatial_bounds_crs', epsg)
  }else{
    ebv_i_char_att(hdf, 'geospatial_bounds_crs', paste0('EPSG:', epsg))
  }

  ebv_i_char_att(hdf, 'geospatial_bounds', bounds)
  ebv_i_char_att(hdf, 'geospatial_lat_resolution', paste0(res[2], ' ', crs_unit))
  ebv_i_char_att(hdf, 'geospatial_lon_resolution', paste0(res[1], ' ', crs_unit))
  if(crs_unit == 'meter'){
    ebv_i_char_att(hdf, 'geospatial_lon_units', 'meter')
    ebv_i_char_att(hdf, 'geospatial_lat_units', 'meter')
  }else{
    ebv_i_char_att(hdf, 'geospatial_lon_units', 'degree_east')
    ebv_i_char_att(hdf, 'geospatial_lat_units', 'degree_north')
  }

  #temporal attributes
  # acdd terms
  ebv_i_char_att(hdf, 'time_coverage_start', t_start)
  ebv_i_char_att(hdf, 'time_coverage_end', t_end)
  ebv_i_char_att(hdf, 'time_coverage_resolution', t_res)

  # change crs variable ----
  crs.id <- rhdf5::H5Dopen(hdf, 'crs')

  # :wkt ref
  #ebv_i_char_att(crs.id, 'crs_ref', crs_ref)
  ebv_i_char_att(crs.id, 'spatial_ref', crs_ref)
  ebv_i_char_att(crs.id, 'GeoTransform', geo_trans)

  #get grid mapping attributes
  crs_grid <- ebv_i_eval_epsg(epsg, proj=TRUE)
  crs_wkt_list <- stringr::str_split(crs_wkt, '\n')[[1]]


  #check name: change standard_name of lat and lon accordingly
  if(stringr::str_detect(crs_wkt, 'PROJCRS')){
    crs_proj <- TRUE
  }else{
    crs_proj<- FALSE
  }

  if(stringr::str_detect(crs_grid, 'utm')){
    #add grid mapping for UTM (not supported by ncmeta)
    #check WKT version and process accordingly
    if(ebv_i_eval_wkt(crs_wkt)){
      #process WKT2 (2019)
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'Latitude of natural origin'))]
      lat_proj <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'Longitude of natural origin'))]
      lon_proj <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'Scale factor at natural origin'))]
      scale_fac <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'False easting'))]
      f_east <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'False northing'))]
      f_north <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
    }else{
      #process WKT
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'latitude_of_origin'))]
      lat_proj <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'central_meridian'))]
      lon_proj <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'scale_factor'))]
      scale_fac <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'false_easting'))]
      f_east <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]
      part <- crs_wkt_list[which(stringr::str_detect(crs_wkt_list, 'false_northing'))]
      f_north <- regmatches(part, gregexpr("[[:digit:].]+", part))[[1]]

    }

    #add attributes
    ebv_i_char_att(crs.id, 'grid_mapping_name', 'transverse_mercator')
    ebv_i_num_att(crs.id, 'latitude_of_projection_origin',lat_proj)
    ebv_i_num_att(crs.id, 'longitude_of_projection_origin',lon_proj)
    ebv_i_num_att(crs.id, 'scale_factor_at_projection_origin', scale_fac)
    ebv_i_num_att(crs.id, 'false_easting',f_east)
    ebv_i_num_att(crs.id, 'false_northing',f_north)

  } else{
    #get grid mapping attributes
    grid_mapping <- ncmeta::nc_prj_to_gridmapping(crs_grid)

    if(!nrow(grid_mapping)==0){

      #add grid mapping name and remove from tibble
      ebv_i_char_att(crs.id, 'grid_mapping_name', grid_mapping$value[grid_mapping$name=='grid_mapping_name'][[1]])
      grid_mapping <- grid_mapping[!grid_mapping$name=='grid_mapping_name',]

      #additional attributes
      for (name in grid_mapping$name){
        ebv_i_num_att(crs.id, name, grid_mapping$value[grid_mapping$name==name][[1]])
      }
    }else{
      warning('Simple georefencing done - without the CF conform gridmapping.')
    }

  }

  # #add standard_name and long_name
  # ebv_i_char_att(crs.id, 'standard_name', 'CRS')
  ebv_i_char_att(crs.id, 'long_name', 'CRS definition')

  #close ds
  rhdf5::H5Dclose(crs.id)

  # change lat variable ----
  # open dataset
  lat.id <- rhdf5::H5Dopen(hdf, 'lat')

  #if CRS is projected, add different standard_name
  if(crs_proj){
    ebv_i_char_att(lat.id, 'standard_name', 'projection_y_coordinate')
  }else{
    ebv_i_char_att(lat.id, 'standard_name', 'latitude')
  }

  # :axis = "Y";
  ebv_i_char_att(lat.id, 'axis', 'Y')

  # :units = 'degrees_north';
  if(crs_proj){
    ebv_i_char_att(lat.id, 'units', 'meter')#paste0(crs_unit, '_north'))
  }else{
    ebv_i_char_att(lat.id, 'units', 'degree_north')#paste0(crs_unit, '_north'))
  }


  #close dataset
  rhdf5::H5Dclose(lat.id)

  # change lon variable ----
  #open dataset
  lon.id <- rhdf5::H5Dopen(hdf, 'lon')

  #if CRS is projected, add different standard_name
  if(crs_proj){
    ebv_i_char_att(lon.id, 'standard_name', 'projection_x_coordinate')
  }else{
    ebv_i_char_att(lon.id, 'standard_name', 'longitude')
  }

  # :axis = "X";
  ebv_i_char_att(lon.id, 'axis', 'X')

  # :units = 'degrees_east';
  if(crs_proj){
    ebv_i_char_att(lon.id, 'units', 'meter')#paste0(crs_unit, '_north'))
  }else{
    ebv_i_char_att(lon.id, 'units', 'degree_east')#paste0(crs_unit, '_east'))
  }

  #close dataset
  rhdf5::H5Dclose(lon.id)

  # change time variable ----
  # open dataset
  time.id <- rhdf5::H5Dopen(hdf, 'time')

  # :axis = "T";
  ebv_i_char_att(time.id, 'axis', 'T')

  # :calendar = "standard";
  if(t_res != 'Paleo'){
    ebv_i_char_att(time.id, 'calendar', 'standard')
  }

  #close
  rhdf5::H5Dclose(time.id)

  #add values to entity var----
  entity_names <- as.data.frame(stringr::str_split(stringr::str_pad(entities, max_char_entity, side = c("right")),''))
  entity_n <- enc2utf8(unlist(entity_names))

  entity.id <- rhdf5::H5Dopen(hdf, 'entity')#HERE
  rhdf5::H5Dwrite(entity.id, entity_n)

  # acdd terms
  ebv_i_char_att(entity.id, 'ebv_entity_type', json$ebv_entity$ebv_entity_type)
  ebv_i_char_att(entity.id, 'ebv_entity_scope', json$ebv_entity$ebv_entity_scope)
  ebv_i_char_att(entity.id, 'ebv_entity_classification_name', json$ebv_entity$ebv_entity_classification_name)
  ebv_i_char_att(entity.id, 'ebv_entity_classification_url', json$ebv_entity$ebv_entity_classification_url)

  #add long_name and standard_name
  ebv_i_char_att(entity.id, 'long_name', 'entity')

  rhdf5::H5Dclose(entity.id)

  # add metric and scenario attributes ----
  # 1. metric, no scenario (entities are not relevant)
  if(scenarios_no==0){
    for (i in 1:(metrics_no)){
      mgid <- rhdf5::H5Gopen(hdf, paste0('metric_', i))
      #add metric attributes
      standard_name <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:standard_name`')))
      long_name <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:long_name`')))
      unit.m <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:units`')))
      ebv_i_char_att(mgid, 'standard_name', standard_name)
      ebv_i_char_att(mgid, 'long_name', long_name)
      ebv_i_char_att(mgid, 'units', unit.m)
      #close data handle
      rhdf5::H5Gclose(mgid)
    }
    #2. scenario and metric (entities are not relevant)
  }else{
    for (j in 1:(scenarios_no)){
      #scenario path
      sgid <- rhdf5::H5Gopen(hdf, paste0('scenario_', j))
      #add attributes
      standard_name <- eval(parse(text=paste0('json$ebv_scenario$ebv_scenario_',j,'$`:standard_name`')))
      long_name <- eval(parse(text=paste0('json$ebv_scenario$ebv_scenario_',j,'$`:long_name`')))
      ebv_i_char_att(sgid, 'standard_name', standard_name)
      ebv_i_char_att(sgid, 'long_name', long_name)
      rhdf5::H5Gclose(sgid)
      for (i in 1:(metrics_no)){
        #open metric group
        mgid <- rhdf5::H5Gopen(hdf, paste0('scenario_', j, '/metric_', i))
        #add metric attributes
        standard_name <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:standard_name`')))
        long_name <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:long_name`')))
        unit.m <- eval(parse(text=paste0('json$ebv_metric$ebv_metric_',i,'$`:units`')))
        ebv_i_char_att(mgid, 'standard_name', standard_name)
        ebv_i_char_att(mgid, 'long_name', long_name)
        ebv_i_char_att(mgid, 'units', unit.m)
        #close datahandle
        rhdf5::H5Gclose(mgid)
      }
    }
  }

  #add entity attributes 3D ----
  if(force_4D==FALSE){
    #enum <- 1
    for(var in var_list){
      part <- stringr::str_split(var, '/')[[1]][2]
      enum <- as.integer(paste0(stringr::str_extract_all(part, '\\d')[[1]], collapse=''))
      did <- rhdf5::H5Dopen(hdf, var)
      ebv_i_char_att(did, 'grid_mapping', '/crs')
      ebv_i_char_att(did, 'coordinates', '/entity')#HERE
      ebv_i_char_att(did, 'coverage_content_type', paste0(json$coverage_content_type, collapse=', '))
      ebv_i_char_att(did, 'standard_name', entities[enum])
      #close dh
      rhdf5::H5Dclose(did)
      #enum <- enum +1
    }
  }else{
    #add entity attributes 4D ----
    enum <-1
    for(var in var_list){
      parts <- stringr::str_split(var, '/')[[1]]
      m <- paste0(parts[1:(length(parts)-1)], collapse='/')
      mid <- rhdf5::H5Gopen(hdf, m)
      long_name <- ebv_i_read_att(mid, 'standard_name')
      rhdf5::H5Gclose(mid)
      did <- rhdf5::H5Dopen(hdf, var)
      ebv_i_char_att(did, 'long_name', long_name)
      ebv_i_char_att(did, 'grid_mapping', '/crs')
      ebv_i_char_att(did, 'coordinates', '/entity')#HERE
      ebv_i_char_att(did, 'coverage_content_type', paste0(json$coverage_content_type, collapse=', '))
      #close dh
      rhdf5::H5Dclose(did)
    }
  }

  # close file  ----
  rhdf5::H5Fclose(hdf)

  # add values to 'entity_list' var ----
  level_i <- length(taxon_list)

  for(level in taxon_list){
    #transform values so they fit into the variable
    data_level_clean <- ebv_i_char_variable(csv_txt[,level], max_char_entity)

    if(verbose){
      print(paste0('add ',level,' data to level: ', level_i))
    }

    rhdf5::h5write(data_level_clean, file=outputpath,
                   name="entity_list", index=list(level_i,NULL, NULL))

    level_i <- level_i-1
  }

  # add values to 'entity_levels' var ----
  level_d <- ebv_i_char_variable(taxon_list, max_char_taxonlevel, TRUE)
  rhdf5::h5write(level_d, file=outputpath,
                 name="entity_levels")

  # add values to 'entity_lsid' var ----
  if(lsid){
    ls_id_d <- ebv_i_char_variable(lsid_list, max_char_lsid)
    rhdf5::h5write(ls_id_d, file=outputpath,
                   name="entity_lsid")
  }


}
