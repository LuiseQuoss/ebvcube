ebv_download <- function(id=NULL, outputdir){

  #initial tests----
  #check for internet connection


  #retrieve dataset list
  datasets <- jsonlite::fromJSON('https://portal.geobon.org/api/v1/datasets')
  no_datasets <- dim(datasets$data)[1]

  datasets_list <- datasets$data$id
  datasets_list <- cbind(datasets_list, datasets$data$title) # add id
  colnames(datasets_list) <- c('id', 'title')

  if(is.null(id)){
    #no dataset chosen
    #return list of datasets----
    message(paste0('There are ', no_datasets, ' datasets available at the portal.'))
    return(datasets_list)

  }else{
    #download netCDF and json

    #initial tests----
    #check id - integer, valid? OR Title
    #check output dir
    #check if file already exists

    #if ID is title -> turn into integer

    root <- 'https://portal.geobon.org/api/v1/datasets/'
    json_path <- paste0(root, id)

    metadata <- jsonlite::fromJSON(json_path)

    #download netcdf
    nc_path <- metadata$data$dataset$download
    nc_url <- paste0('https://', nc_path)

    name <- basename(nc_path)

    utils::download.file(url = nc_url,
                  destfile = file.path(outputdir, name)
                  )

    #download json
    name_js <- paste0(stringr::str_remove(name, '.nc'), '_metadata.json')

    utils::download.file(url = json_path,
                         destfile = file.path(outputdir, name_js)
    )


  }



}
