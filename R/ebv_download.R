#' Download an EBV netCDF file
#'
#' @description Returns the list of all available data sets at the EBV Portal if
#'   you no arguments are given. If an ID is given, the corresponding file
#'   (netCDF) and its metadata (json file) will be downloaded to the given
#'   output directory.
#'
#' @param id Integer or Character. There are three option to identify the
#'   dataset to be downloaded. (1) It can be a single integer value representing
#'   the ID of the dataset. (2) It can be a character string representing the
#'   title of the data set. (3) It can be a character string representing the
#'   DOI of the dataset in the format '10.25829/f2rdp4' (Dataset 'Habitat
#'   availability for African great apes' by Jessica Junker from the EBV Data
#'   Portal). All three identifier can be retrieved by running
#'   [ebvcube::ebv_download()] without any arguments which returns a data.frame
#'   of all available data sets and their ID, title and DOI.
#' @param outputdir Character. Output directory of the downloaded files.
#' @param overwrite Logical. Default: FALSE. Set to TRUE if you want to
#'   overwrite the netCDF and json.
#' @param verbose Logical. Default: TRUE. Turn off additional prints by setting
#'   it to FALSE.
#'
#' @return Downloads a netCDF and json file (ACDD metadata) to the given output
#'   directory. If run empty returns a data.frame of all available data sets and
#'   their ID, title and DOI.
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
ebv_download <- function(id=NULL,
                         outputdir,
                         overwrite=FALSE,
                         verbose=TRUE){

  #start initial tests----
  #check for internet connection
  if(!curl::has_internet()){
    stop('You are not connected to the internet.')
  }

  #check if portal website can be reached
  con <- url('https://portal.geobon.org') #, 'rb'
  check <- suppressWarnings(try(open.connection(con,open="rt"),silent=TRUE)[1])
  suppressWarnings(try(close.connection(con),silent=TRUE))
  website <- ifelse(is.null(check),TRUE,FALSE)

  if(website!=TRUE){
    stop(paste0('The EBV Data Portal https://portal.geobon.org cannot be reached.\n', check))
  }

  #check verbose
  if(checkmate::checkLogical(verbose, len=1, any.missing=FALSE) != TRUE){
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
  datasets_list$doi <- c('coming soon')

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
      #check if DOI - check will have a lot of false positives!
      if(grepl('^10[.]\\d{1,}[.]?\\d*[/]?', id)){

        #check if only DOI or link
        if(! stringr::str_detect(id, 'https://doi.org/')){
          url_id <- paste0('https://doi.org/', id)
        }else{
          url_id <- id
        }

        #check if this website is available
        con_doi <- url(url_id)
        check_doi <- suppressWarnings(try(open.connection(con_doi,open="rt",timeout=t),silent=TRUE)[1])
        suppressWarnings(try(close.connection(con_doi),silent=TRUE))
        website_doi <- ifelse(is.null(check_doi),TRUE,FALSE)

        if(website_doi!=TRUE){
          stop(paste0('The DOI you provided (', url_id ,') cannot be reached.\n', check_doi))
        }

        #get the url of the redirection from the DOI url
        a <- httr::HEAD(url_id)
        path_portal <- (a$all_headers[[1]])$headers$location
        #check if redirect to portal website
        if(!grepl('portal.geobon.org/ebv-detail?id=', path_portal, fixed=TRUE)){
          #stop if DOI is not pointing to EBV Data Portal
          stop('The DOI you have entered does not point to a dataset from the EBV Data Portal and therefore cannot be downloaded.')
        }else{
          #get the ID from the path
          id <- stringr::str_split(path_portal, '=')[[1]][2]
          id <- stringr::str_remove(id, '&v')
        }

        #get download link from API
        #download

    }else{
        #check if it is the title of a data set
        #id is title -> turn into integer
        id <- which(datasets_list$title==id)
        if(ebv_i_empty(id)){
          stop('Yout title does not match any of the titles available. Run this function without any arguments to get the list of available datasets, choose one and download it.')
        }
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
    if(file.exists(file.path(outputdir, name_nc)) && overwrite==FALSE){
      if(verbose){
        print('NetCDF already downloaded to this directory. Set overwrite to TRUE to replace the older file')
      }
      return(file.path(outputdir, name_nc))
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

  return(file.path(outputdir, name_nc))

}
