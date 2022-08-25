#' Write the extracted data on your disk as a GeoTiff
#' @description After you extracted data from the EBV netCDF and worked with it
#'   this function gives you the possibility to write it to disk as a GeoTiff.
#'
#' @param data Your data object. May be SpatRaster, array, DelayedMatrix or list
#'   of DelayedMatrix (see return values of [ebvcube::ebv_read()])
#' @param epsg Integer. Default: 4326 (WGS84). Defines the coordinate reference
#'   system via the corresponding epsg code.
#' @param extent Numeric. Default: c(-180,180,-90,90). Defines the extent of the
#'   data: c(xmin, xmax, ymin, ymax).
#' @param type Character. Default is FLT8S Indicate the datatype of the GeoTiff
#'   file. Possible values: INT1S, INT2S, INT2U, INT4S, INT4U, FLT4S, FLT8S.
#' @param outputpath Character. Set the path where you want to write the data to
#'   disk as a GeoTiff. Ending needs to be *.tif.
#' @param overwrite Locigal. Default: FALSE. Set to TRUE to overwrite the
#'   outputfile defined by 'outputpath'.
#' @param verbose Logical. Default: FALSE. Turn on all warnings by setting it to
#'   TRUE.
#'
#' @note If the nodata value of your data is not detected correctly, this could
#'   be due to the wrong choice of the datatype (type argument).
#'
#' @return Returns the outputpath.
#' @export
#'
#' @examples
#' #set path to EBV netCDF
#' file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvcube")
#' #get all datacubepaths of EBV netCDF
#' datacubes <- ebv_datacubepaths(file)
#'
#' #read data
#' data <- ebv_read(filepath = file, datacubepath = datacubes[1,1], timestep = 1)
#' # HERE YOU CAN WORK WITH YOUR DATA
#'
#' #write data to disk as GeoTiff
#' out <- system.file(file.path("extdata","write_data.tif"), package="ebvcube")
#' # ebv_write(data = data, outputpath = out)
#'
#' #read a subset
#' # data_bb <- ebv_read_bb(filepath = file, datacubepath = datacubes[1,1],
#' #                        entity = NULL, timestep = 1:3, bb = c(5,15,47,55))
#'
#' #write subset to disk as GeoTiff
#' # ebv_write(data = data_bb, outputpath = out, extent = c(5,15,47,55), overwrite = TRUE)
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
  withr::defer(
    if(exists('temp.tif')){
      if(file.exists(temp.tif)){
        file.remove(temp.tif)
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

  #eval epsg, retrieve wkt crs
  crs <- ebv_i_eval_epsg(epsg)

  #check type
  if(! type %in% c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")){
    stop('The type needs to be one of the following values: INT1S, INT2S, INT2U, INT4S, INT4U, FLT4S, FLT8S.')
  }

  #######initial test end ----

  # write DelayedMatrix ----
  if (class(data) == "DelayedMatrix"){

    #data from H5Array - on disk
    print('Note: Writing data from HDF5Array to disc. All delayed operations are now executed. This may take a few minutes.')

    #derive other variables
    name <- stringr::str_remove(basename(outputpath),'.tif')
    temp.tif <- tempfile(fileext = '.tif')

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

    #read with terra and add missing georefence data ----
    temp_raster <- suppressWarnings(terra::rast(temp.tif)) #warning about missing extent
    terra::ext(temp_raster) <- extent
    terra::crs(temp_raster) <- crs

    print('Delayed operations are finished, writing file to disk.')
    terra::writeRaster(temp_raster, outputpath, datatype=type, overwrite = overwrite)

    #delete temp file
    if (file.exists(temp.tif)){
      t <- file.remove(temp.tif)
    }

    # write several DelayedMatrix (list) ----
  } else if(class(data)=='list'){

    #check temp directory
    temp_path <- tempdir()

    #data from H5Array - on disk
    print('Note: Writing data from HDF5Array to disc. All delayed operations are now executed. This may take a few minutes.')

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
    ot <- ebv_i_type_ot(type)

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

    #array/matrix to raster
    extent <- terra::ext(extent)
    r <- terra::rast(data, crs=crs, extent=extent)

    #write raster to disk
    terra::writeRaster(r, outputpath, filetype = "GTiff", overwrite = overwrite,
                        datatype=type)
    return(outputpath)
  # write raster ----
  } else if(class(data)=="SpatRaster"){
    #write raster to disk
    terra::crs(data) <- crs #ensure crs
    terra::writeRaster(data, outputpath, filetype = "GTiff", overwrite = overwrite,
                        datatype=type)
    return(outputpath)
  }else{
    #not implemented, tell user
    stop(paste0('Not implemented for class ', class(data), '.'))
  }

}
