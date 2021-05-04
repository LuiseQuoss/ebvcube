#' Write the data on your disk as a GeoTiff
#' @description After you extracted data from the EBV NetCDF and worked with it
#'   this funtion gives you the possibility to write it to disk as a GeoTiff.
#'   This functions writes temporary files on your disk. Speficy a directory for
#'   these setting via options('temp_directory'='/path/to/temp/directory').
#' @note Not yet implemented for subsets of the data (only whole spatial
#'   coverage of the corresponding EBV NetCDF).
#'
#' @param data Your data object. May be raster, array, DelayedMatrix or list of
#'   DelayedMatrix (see return values of [ebvnetcdf::ebv_data_read()])
#' @param filepath Path to the NetCDF file you read the data from. Used for the
#'   detection of properties as spatial extent and epsg.
#' @param datacubepath Path to the datacube you got the data from. Used for the
#'   detection of properties as data type and nodata value.
#' @param outputpath Set the path where you want to write the data to disk as a
#'   GeoTiff.
#' @param overwrite Default: FALSE. Set to TRUE to overwrite the outputfile
#'   defined by 'outputpath'.
#' @param verbose Logical. Turn on all warnings by setting it to TRUE.
#'
#' @return Returns the outputpath.
#' @export
#'
#' @examples
#' # file <- 'path/to/netcdf/file.nc'
#' # datacubes <- ebv_datacubepaths(file)
#' # data <- ebv_data_read(file, datacubes[1,1], 1)
#' # WORK WITH YOUR DATA
#' # out <- 'path/to/write/the/data.tif'
#' # ebv_data_write(data, file, datacubes[1,1], out)
ebv_data_write <- function(data, filepath, datacubepath, outputpath, overwrite=FALSE, verbose=FALSE){
  ####initial tests start ----
  # ensure file and all datahandles are closed on exit
  withr::defer(
    if(exists('hdf')){
      if(rhdf5::H5Iis_valid(hdf)==TRUE){rhdf5::H5Fclose(hdf)}
    }
  )

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
  if(missing(filepath)){
    stop('Filepath argument is missing.')
  }
  if(missing(datacubepath)){
    stop('Datacubepath argument is missing.')
  }
  #are all arguments given?
  if(missing(outputpath)){
    stop('Outputpath argument is missing.')
  }

  #turn off local warnings if verbose=TRUE
  if(checkmate::checkLogical(verbose) != TRUE){
    stop('Verbose must be of type logical.')
  }
  if(verbose){
    withr::local_options(list(warn = 0))
  }else{
    withr::local_options(list(warn = -1))
  }

  #check logical arguments
  if(checkmate::checkLogical(overwrite) != TRUE){
    stop('overwrite must be of type logical.')
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

  #variable check
  if (checkmate::checkCharacter(datacubepath) != TRUE){
    stop('Datacubepath must be of type character.')
  }
  hdf <- rhdf5::H5Fopen(filepath)
  if (rhdf5::H5Lexists(hdf, datacubepath)==FALSE){
    stop(paste0('The given variable is not valid:\n', datacubepath))
  }
  rhdf5::H5Fclose(hdf)

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

  #######initial test end ----

  #get properties
  prop <- ebv_properties(filepath, datacubepath, verbose)

  # write DelayedMatrix ----
  if (class(data) == "DelayedMatrix"){
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
    type.long <- prop@entity$type
    ot <- ebv_i_type_ot(type.long)

    #add CRS, shift to -180,90, add nodata value
    if(!is.null(ot)){
      gdalUtils::gdal_translate(temp.tif, outputpath, overwrite = overwrite,
                     a_ullr = c(prop@spatial$extent[1], prop@spatial$extent[4], prop@spatial$extent[2], prop@spatial$extent[3]),
                     a_srs = paste0('EPSG:', prop@spatial$epsg),
                     co = c('COMPRESS=DEFLATE', 'BIGTIFF=IF_NEEDED'),
                     a_nodata=prop@entity$fillvalue,
                     ot = ot)
    } else{
      gdalUtils::gdal_translate(temp.tif, outputpath, overwrite = overwrite,
                     a_ullr = c(prop@spatial$extent[1], prop@spatial$extent[4], prop@spatial$extent[2], prop@spatial$extent[3]),
                     a_srs = paste0('EPSG:', prop@spatial$epsg),
                     co = c('COMPRESS=DEFLATE', 'BIGTIFF=IF_NEEDED'),
                     a_nodata=prop@entity$fillvalue)
    }

    #delete temp file
    if (file.exists(temp.tif)){
      file.remove(temp.tif)
    }

    # write several DelayedMatrix (list) ----
  } else if(class(data)=='list'){
    #data from H5Array - on disk
    message('Note: Writing data from HDF5Array to disc. This may take a few minutes depending on the data dimensions.')

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
                     a_ullr = c(prop@spatial$extent[1], prop@spatial$extent[4], prop@spatial$extent[2], prop@spatial$extent[3]),
                     a_srs = paste0('EPSG:', prop@spatial$epsg))#,
      #a_nodata=prop@entity_information@fillvalue)
    }

    #merge all vrts to one vrt
    temp.vrt <- file.path(temp_path, 'temp_EBV_write_data.vrt')
    gdalUtils::gdalbuildvrt(temps, temp.vrt, separate=TRUE, overwrite=TRUE,
                            a_srs = paste0('EPSG:', prop@spatial$epsg),
                            te = c(prop@spatial$extent[1], prop@spatial$extent[3],
                                   prop@spatial$extent[2], prop@spatial$extent[4]))

    #get output type ot for gdal
    type.long <- prop@entity$type
    ot <- ebv_i_type_ot(type.long)

    a_srs <- sp::CRS(SRS_string = paste0('EPSG:', prop@spatial$epsg))
    #gdal translate: add fillvalue, add ot if given, output final tif
    if(!is.null(ot)){
      gdalUtils::gdal_translate(temp.vrt, outputpath,
                     a_nodata=prop@entity$fillvalue,
                     overwrite=overwrite,
                     co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                     a_ullr = c(prop@spatial$extent[1], prop@spatial$extent[4],
                            prop@spatial$extent[2], prop@spatial$extent[3]),
                     a_srs = a_srs,
                     ot=ot)
    } else {
      gdalUtils::gdal_translate(temp.vrt, outputpath,
                     a_nodata=prop@entity$fillvalue,
                     co = c('COMPRESS=DEFLATE','BIGTIFF=IF_NEEDED'),
                     a_ullr = c(prop@spatial$extent[1], prop@spatial$extent[4],
                                prop@spatial$extent[2], prop@spatial$extent[3]),
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

    if(length(dim(data))==2){
      #convert to raster
      r <- raster::raster(
        data,
        xmn=prop@spatial$extent[1], xmx=prop@spatial$extent[2],
        ymn=prop@spatial$extent[3], ymx=prop@spatial$extent[4],
        crs=prop@spatial$srs
      )
    } else {
      #convert to raster
      r <-raster::brick(
        data,
        xmn=prop@spatial$extent[1], xmx=prop@spatial$extent[2],
        ymn=prop@spatial$extent[3], ymx=prop@spatial$extent[4],
        crs=prop@spatial$srs
      )
    }

    #NAvalue(r) <- prop@entity_information@fillvalue
    r<- raster::mask(r, r, maskvalue=prop@entity$fillvalue)

    #write raster to disk
    raster::writeRaster(r, outputpath, format = "GTiff", overwrite = overwrite)
    return(outputpath)

  } else{
    #not implemented, tell user
    stop(paste0('Not implemented for class ', class(data), '.'))
  }

}
