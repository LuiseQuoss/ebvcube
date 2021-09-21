#' Write the extracted data on your disk as a GeoTiff
#' @description After you extracted data from the EBV netCDF and worked with it
#'   this function gives you the possibility to write it to disk as a GeoTiff.
#'   This functions writes temporary files on your disk. Specify a directory for
#'   these setting via options('ebv_temp'='/path/to/temp/directory').
#' @note Not yet implemented for subsets of the data (only whole spatial
#'   coverage of the corresponding EBV netCDF).
#'
#' @param data Your data object. May be raster, array, DelayedMatrix or list of
#'   DelayedMatrix (see return values of [ebvnetcdf::ebv_read()])
#' @param epsg Integer. Default: 4326 (WGS84). Defines the coordinate reference
#'   system via the corresponding epsg code.
#' @param extent Numeric. Default: c(-180,180,-90,90). Defines the extent of the
#'   data: c(xmin, xmax, ymin, ymax).
#' @param type Character. Default is FLT8S Indicate the datatype of the GeoTiff
#'   file. Possible values: LOG1S, INT1S, INT1S, INT2S, INT2U, INT4S, INT4U,
#'   FLT4S, FLT8S.
#' @param outputpath Character. Set the path where you want to write the data to
#'   disk as a GeoTiff. Ending needs to be *.tif.
#' @param overwrite Locigal. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note For more info on the datatype definition see [raster::dataType()].
#'
#' @return Returns the outputpath.
#' @export
#'
#' @examples
#' #define temp directory
#' options('ebv_temp'=system.file("extdata/", package="ebvnetcdf"))
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
#' datacubes <- ebv_datacubepaths(file)
#' data <- ebv_read(file, datacubes[1,1], 1)
#' # WORK WITH YOUR DATA
#' out <- system.file(file.path("extdata","write_data.tif"), package="ebvnetcdf")
#' #ebv_write(data, out)
ebv_write <- function(data, outputpath, epsg=4326, extent=c(-180, 180, -90, 90),
                      type='FLT8S', overwrite=FALSE, verbose=FALSE){
  ####initial tests start ----

  #ensure that all tempfiles are deleted on exit
  withr::defer(
    if(exists('temps')){
      for (t in temps){
        if(file.exists(t)){
          file.remove(t)
        }
      }
    }
  )
  withr::defer(
    if(exists('temp.vrt')){
      if(file.exists(temp.vrt)){
        file.remove(temp.vrt)
      }
    }
  )

  #are all arguments given?
  if(missing(data)){
    stop('Data argument is missing.')
  }
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
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

  #check logical arguments
  if(checkmate::checkLogical(overwrite, len=1, any.missing=F) != TRUE){
    stop('overwrite must be of type logical.')
  }

  #outputpath check
  if (checkmate::checkCharacter(outputpath) != TRUE){
    stop('Outputpath must be of type character.')
  }
  if(checkmate::checkDirectoryExists(dirname(outputpath)) != TRUE){
    stop(paste0('Output directory does not exist.\n', dirname(outputpath)))
  }
  if(!endsWith(outputpath, '.tif')){
    stop('Outputpath needs to end with *.tif. Other datatypes are not yet implemented.')
  }
  #check if outpufile exists if overwrite is disabled
  if(!overwrite){
    if(checkmate::checkPathForOutput(outputpath) != TRUE){
      stop('Output file already exists. Change name or enable overwrite.')
    }
  }

  #check if epsg is valid
  if(checkmate::checkIntegerish(epsg) != TRUE){
    stop('epsg must be of type integer.')
  }
  crs <- gdalUtils::gdalsrsinfo(paste0('EPSG:',epsg))
  if(any(stringr::str_detect(as.character(crs), 'crs not found'))){
    stop('Given EPSG code is not in PROJ library. Did you give a wrong EPSG code?')
  } else if (any(stringr::str_detect(as.character(crs), '(?i)error'))){
    stop(paste0('Could not process EPSG. See error from gdalUtils:\n', as.character(paste0(crs, collapse = '\n'))))
  }

  #check type
  if(! type %in% c('LOG1S', 'INT1S', 'INT1S', 'INT2S', 'INT2U', 'INT4S', 'INT4U', 'FLT4S', 'FLT8S')){
    stop('The type needs to be one of the following values: LOG1S, INT1S, INT1S, INT2S, INT2U, INT4S, INT4U, FLT4S, FLT8S.')
  }

  #######initial test end ----

  # write DelayedMatrix ----
  if (class(data) == "DelayedMatrix"){

    #check temp directory
    temp_path <- getOption('ebv_temp')[[1]]
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

    #data from H5Array - on disk
    message('Note: Writing data from HDF5Array to disc. This may take a few minutes depending on the data dimensions.')

    #derive other variables
    name <- stringr::str_remove(basename(outputpath),'.tif')
    temp.tif <- file.path(temp_path, 'temp_EBV_write_data.tif')

    #temp.tif must be new file, remove tempfile
    if (file.exists(temp.tif)){
      file.remove(temp.tif)
    }

    #turn data back
    data <- t(data[nrow(data):1,])
    data <- data[,ncol(data):1]

    out <- HDF5Array::writeHDF5Array(
      data,
      filepath = temp.tif,
      name = name
    )

    #get output type ot for gdal
    type.long <- type
    ot <- ebv_i_type_ot(type.long)

    a_srs <- paste0('EPSG:', epsg)
    #a_srs <- sp::CRS(SRS_string = paste0('EPSG:', prop@spatial$epsg))

    #add CRS, shift to -180,90, add nodata value
    if(!is.null(ot)){
      gdalUtils::gdal_translate(temp.tif, outputpath, overwrite = overwrite,
                     a_ullr = c(extent[1], extent[4], extent[2], extent[3]),
                     a_srs = a_srs,
                     co = c('COMPRESS=DEFLATE', 'BIGTIFF=IF_NEEDED'),
                     #a_nodata=prop@ebv_cube$fillvalue,
                     ot = ot)
    } else{
      gdalUtils::gdal_translate(temp.tif, outputpath, overwrite = overwrite,
                     a_ullr = c(extent[1], extent[4], extent[2], extent[3]),
                     a_srs = a_srs,
                     co = c('COMPRESS=DEFLATE', 'BIGTIFF=IF_NEEDED')
                     #a_nodata=prop@ebv_cube$fillvalue
                     )
    }

    #delete temp file
    if (file.exists(temp.tif)){
      file.remove(temp.tif)
    }

    # write several DelayedMatrix (list) ----
  } else if(class(data)=='list'){

    #check temp directory
    temp_path <- getOption('ebv_temp')[[1]]
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

    #data from H5Array - on disk
    message('Note: Writing data from HDF5Array to disc. This may take a few minutes depending on the data dimensions.')

    #a_srs <- sp::CRS(SRS_string = paste0('EPSG:', prop@spatial$epsg))
    a_srs <- paste0('EPSG:', epsg)
    temps <- c()
    #turn listed DelayedArrays into tif
    for (i in 1:length(data)){
      #derive other variables
      name <- paste0(stringr::str_remove(basename(outputpath),'.tif'), '_', i)
      #change tempname
      temp.tif <- file.path(temp_path, paste0('temp_EBV_write_data_', i, '.tif'))
      #temp.tif must be new file, remove tempfile
      if (file.exists(temp.tif)){
        file.remove(temp.tif)
      }

      band <- data[[i]]
      band <- t(band[nrow(band):1,])
      band <- band[,ncol(band):1]

      #write temp tif per timestep
      out <- HDF5Array::writeHDF5Array(
        band,
        filepath = temp.tif,
        name = name
      )
      #add filename to list
      temps <- c(temps, temp.tif)

      temp.vrt <- file.path(temp_path, paste0('temp_EBV_write_data_', i, '.vrt'))

      #add filename to list
      temps <- c(temps, temp.vrt)

      #add georeference, shift to -180,-90,
      gdalUtils::gdal_translate(temp.tif, temp.vrt, of='VRT', overwrite=TRUE,
                     a_ullr = c(extent[1], extent[4], extent[2],extent[3]),
                     a_srs = a_srs)#,
      #a_nodata=prop@ebv_cube_information@fillvalue)
    }

    #merge all vrts to one vrt
    temp.vrt <- file.path(temp_path, 'temp_EBV_write_data.vrt')
    gdalUtils::gdalbuildvrt(temps, temp.vrt, separate=TRUE, overwrite=TRUE,
                            a_srs = a_srs,
                            te = c(extent[1], extent[3],
                                   extent[2], extent[4]))

    #get output type ot for gdal
    type.long <- type
    ot <- ebv_i_type_ot(type.long)

    #gdal translate: add fillvalue, add ot if given, output final tif
    if(!is.null(ot)){
      gdalUtils::gdal_translate(temp.vrt, outputpath,
                     #a_nodata=prop@ebv_cube$fillvalue,
                     overwrite=overwrite,
                     co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                     a_ullr = c(extent[1], extent[4],
                            extent[2], extent[3]),
                     a_srs = a_srs,
                     ot=ot)
    } else {
      gdalUtils::gdal_translate(temp.vrt, outputpath,
                     #a_nodata=prop@ebv_cube$fillvalue,
                     co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                     a_ullr = c(extent[1], extent[4],
                                extent[2], extent[3]),
                     a_srs = a_srs,
                     overwrite=overwrite)
    }

    #delete temp file
    for (f in temps){
      if (file.exists(f)){
        file.remove(f)
      }
    }
    #remove multilayer vrt
    if (file.exists(temp.vrt)){
      file.remove(temp.vrt)
    }

  # write array or matrix ----
  }else if (class(data)=="array"| class(data)=="matrix"){
    #data from array/matrix - in memory

    crs <- paste(crs[5:length(crs)], collapse = ' ')

    if(length(dim(data))==2){
      #convert to raster
      r <- raster::raster(
        data,
        xmn=extent[1], xmx=extent[2],
        ymn=extent[3], ymx=extent[4],
        crs=crs
      )
    } else {
      #convert to raster
      r <-raster::brick(
        data,
        xmn=extent[1], xmx=extent[2],
        ymn=extent[3], ymx=extent[4],
        crs=crs
      )
    }

    #NAvalue(r) <- prop@ebv_cube_information@fillvalue
    #r<- raster::mask(r, r, maskvalue=prop@ebv_cube$fillvalue)

    #write raster to disk
    raster::writeRaster(r, outputpath, format = "GTiff", overwrite = overwrite,
                        datatype=type)
    return(outputpath)
  # write raster ----
  } else if(class(data)=="RasterLayer"|class(data)=='RasterBrick'){
    #mask out fillvalue
    #r<- raster::mask(data, data, maskvalue=prop@ebv_cube$fillvalue)
    #write raster to disk
    crs <- paste(crs[5:length(crs)], collapse = ' ')
    raster::crs(data) <- crs
    raster::writeRaster(data, outputpath, format = "GTiff", overwrite = overwrite,
                        datatype=type)
    return(outputpath)
  }else{
    #not implemented, tell user
    stop(paste0('Not implemented for class ', class(data), '.'))
  }

}
