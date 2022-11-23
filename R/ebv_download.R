#' Download an EBV netCDF file
#'
#' @description Returns the list of all available data sets at the EBV Portal if
#'   you no arguments are given. If an ID is given, the corresponding file
#'   (netCDF) and its metadata (json file) will be downloaded to the given
#'   output directory.
#'
#' @param id Integer or Character. Must be a single integer value or a character
#'   string representing the title of the data set. Both can be retrieved by
#'   running [ebvcube::ebv_download()] without any arguments which returns the
#'   list of data sets available and their title and ID.
#' @param outputdir Character. Output directory of the downloaded files.
#' @param overwrite Logical. Default: FALSE. Set to TRUE if you want to
#'   overwrite the netCDF and json.
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Downloades a netCDF and json file to the given output directory.
#' @export
#'
#' @examples
#' #get all available datasets
#' datasets <- ebv_download()
#'
#' \donttest{
#' ebv_download(id = datasets$id[1], outputdir =
#' tempdir(), overwrite=TRUE,
#' verbose=FALSE)
#' }
ebv_download <- function(id=NULL, outputdir, overwrite=FALSE, verbose=TRUE){

  #start initial tests----
  #check for internet connection
  internet <- tryCatch({
    #try
    url('https://portal.geobon.org', 'rb')
    internet <-TRUE
  },
  #catch error/warning
  warning = function(e){
    if(stringr::str_detect(as.character(e), 'InternetOpenUrl failed')){
      internet <- FALSE
    }
  })

  if(internet!=TRUE){
    stop('It seems that you are not connected to the internet and therefore cannot download any files. Please check your connection. If you are sure you are connected, it could also be that https://portal.geobon.org is down. Check in your browser.')
  }

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=F) != TRUE){
    stop('Verbose must be of type logical.')
  }

  #end initial tests----

  #retrieve data set list
  datasets <- jsonlite::fromJSON('https://portal.geobon.org/api/v1/datasets')
  no_datasets <- dim(datasets$data)[1]

  datasets_list <- datasets$data$id
  datasets_list <- cbind(datasets_list, datasets$data$title) # add id
  colnames(datasets_list) <- c('id', 'title')
  datasets_list <- as.data.frame(datasets_list)
  datasets_list <- datasets_list[order(as.numeric(datasets_list$id)),] #sort by id
  datasets_list$id <- as.integer(datasets_list$id)


  if(is.null(id)){
    #no dataset chosen
    #return list of datasets----
    if(verbose){
      print(paste0('There are ', no_datasets, ' datasets available at the portal.'))
    }

    return(datasets_list)

  }else{
    #download netCDF and json----
    if(missing(outputdir)){
      stop('Please specify an output directory.')
    }

    #id checks----
    #check id - integer, valid? OR Title
    if (checkmate::checkInt(id)==TRUE){
      #pass - ID is already a single integer value
    }else if(checkmate::checkCharacter(id, len=1)==TRUE){
      #id is title -> turn into integer
      id <- which(datasets_list$title==id)
      if(ebv_i_empty(id)){
        stop('Yout title does not match any of the titles available. Run this function without any arguments to get the list of available datasets, choose one and download it.')
      }
    }else{
      stop('The ID argument must be a single integer value or a single character string ')
    }

    #check output dir
    if (checkmate::checkCharacter(outputdir) != TRUE){
      stop('Outputdir must be of type character.')
    }
    if(checkmate::checkDirectoryExists(dirname(outputdir)) != TRUE){
      stop(paste0('Output directory does not exist.\n', outputdir))
    }


    root <- 'https://portal.geobon.org/api/v1/datasets/'
    json_path <- paste0(root, id)

    metadata <- jsonlite::fromJSON(json_path)

    #get netcdf path and name
    nc_path <- metadata$data$dataset$download
    nc_url <- paste0('https://', nc_path)
    name_nc <- basename(nc_path)

    #check if netCDF file already exists
    if(file.exists(file.path(outputdir, name_nc)) & overwrite==FALSE ){
      stop('NetCDF already downloaded to this directory. Set overwrite to TRUE to replace the older version.')
    }

    #download netcdf
    if(verbose){
      print('Downloading file... Please wait.')
      }

    curl::curl_download(nc_url,
                        destfile = file.path(outputdir, name_nc),
                        quiet = !verbose)

    #download json
    name_js <- paste0(stringr::str_remove(name_nc, '.nc'), '_metadata.json')

    curl::curl_download(json_path,
                        destfile = file.path(outputdir, name_js),
                        quiet = !verbose)


  }
  if(verbose){
    print(paste0('Check out your files: ', file.path(outputdir, name_nc)))
  }

}
