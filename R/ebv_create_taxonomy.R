ebv_create_taxonomy <- function(jsonpath, outputpath, taxonomy, lsid=TRUE,
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
  dids <- c('crs.id', 'lat.id', 'lon.id', 'time.id', 'did')
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
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }
  if(checkmate::checkLogical(force_4D, len=1, any.missing=F) != TRUE){
    stop('force_4D must be of type logical.')
  }

  #check if json exists
  if (checkmate::checkCharacter(jsonpath) != TRUE){
    stop('Filepath (JSON) must be of type character.')
  }
  if (checkmate::checkFileExists(jsonpath) != TRUE){
    stop(paste0('Json file does not exist.\n', jsonpath))
  }
  if (!(endsWith(jsonpath, '.json') | endsWith(jsonpath, '.js'))){
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
        csv_txt <- suppressWarnings(utils::read.csv(taxonomy, sep=sep, header=F, fileEncoding="UTF-8"))
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
    if(checkmate::checkNumber(fillvalue) != TRUE & !is.na(fillvalue)){
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

  if(!is.null(timesteps) & t_res != 'Paleo'){
    if (checkmate::checkCharacter(timesteps) != TRUE){
      stop('timesteps needs to be a list of character values.')
    }else {
      for(ts in timesteps){
        #check ISO format
        if(!(grepl('^\\d{4}-\\d{2}-\\d{2}$', ts) | grepl('^\\d{4}$',ts))){
          stop(paste0('Your timestep ', ts, ' is not following the indicated ISO format. Check help page for more information.'))
        }

      }
    }
  }

  # end initial tests ----
  
  # read taxonomy info ----
  # csv_txt <- tryCatch(utils::read.csv(taxonomy, sep=sep, header=TRUE, fileEncoding="UTF-8"),
  #                     error = function(e){
  #                       stop(paste0('Cannot read the CSV provided for the taxonomy. See error below:\n'),
  #                            as.character(e)
  #                       )
  #                     })

  dim_csv <- dim(csv_txt)
  taxon_list <- names(csv_txt)

  #get entities and maybe lsid-list
  if(lsid){
    entities <- csv_txt[,(dim_csv[2]-1)]
    lsid_list <- csv_txt[,dim_csv[2]]
  }else{
    entities <- csv_txt[,dim_csv[2]]
    lsid_list <- NA
  }


}
