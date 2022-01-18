#' Checks if a vector is empty
#' @param x A vector.
#' @return Logical.
#' @noRd
ebv_i_empty <- function(x){
 if(length(x)==0){
   return(TRUE)
 } else {
   return(FALSE)
 }
}

#' Checks the OS of the PC
#' @return Character. Name of OS.
#' @noRd
ebv_i_os <- function(){
  temp <- Sys.info()
  return(as.character(temp['sysname']))
}

#' Checks if the netCDF file is opened somewhere else.
#' @param filepath Path to NetCDF file.
#' @return Logical.
#' @noRd
ebv_i_file_opened <- function(filepath){
  if (ebv_i_os() =='Linux' | ebv_i_os() =='Darwin'){
    stdout <- paste0('lsof -t ', filepath, ' | wc -w')
    result <- system(stdout, intern=TRUE)
    if(result == '0'){
      return(FALSE)
    } else{
      stdout <- paste0('lsof -t ', filepath)
      result <- system(stdout, intern=TRUE)
      stdout <- paste0('ps -p ', as.integer(result))
      result <- system(stdout, intern=TRUE)
      if (grepl('qgis', result[2], fixed=TRUE)){
        stop(paste0('File ', basename(filepath), ' opened in QGIS and therefore cannot be processed. You must close the file.'))
      } else if (grepl('rsession', result[2], fixed=TRUE)) {
        stop(paste0('File ', basename(filepath), ' opened in R and therefore cannot be processed. You must close the file.'))
        #use restart()?
      } else {
        running <- stringr::str_split(result[2], ' ')[[1]][length(stringr::str_split(result[2], ' ')[[1]])]
        stop(paste0('File ', basename(filepath) ,' opend in different programm: ', running, '. You must close the file.'))
      }
    }
  } else if (ebv_i_os() == 'Windows') {
    withr::local_options(list(warn = -1))
    #check whether file can be accessed with writing permission
    cmd <- paste0("powershell $FileStream = [System.IO.File]::Open('",filepath,"','Open','Write')")
    out <- shell(cmd, intern=T,mustWork=T)
    #process out
    if(!ebv_i_empty(out)){
      stop('File opened in another application. Please close file and try again.')
    }
  } else {
    print('File opened check not implemented for this OS.')
  }
}

#' Checks current memory usage
#' @return Vector. First value: total RAM, second value: free RAM.
#' @noRd
ebv_i_ram <- function(){
  temp <- as.character(memuse::Sys.meminfo())
  #total ram
  total <- stringr::str_split(temp[1], ' ')[[1]][4]
  total <- round(as.double(stringr::str_remove_all(total, ',')),2)
  #free ram
  free <- stringr::str_split(temp[2], ' ')[[1]][4]
  free <- round(as.double(stringr::str_remove_all(free, ',')),2)
  return(c(total, free))
}

#' Turns hdf5 type of NetCDF into R type
#' @param type.long Character. Hdf5 type of NetCDF file - retrieved from
#'   [ebvcube::ebv_properties()].
#' @return Character. Short type: double or integer.
#' @noRd
ebv_i_type_r <- function(type.long){
  parts <- stringr::str_split(type.long, '_')
  if(parts[[1]][3] == "LDOUBLE"){
    type.short <- 'double'
  } else if(parts[[1]][3] == "LLONG"){
    type.short <- 'integer'
  } else if(parts[[1]][3] == "LONG"){
    type.short <- 'integer'
  } else if(stringr::str_starts(parts[[1]][3], 'I')){
    type.short <- 'integer'
  } else if (stringr::str_starts(parts[[1]][3], 'F')){
    type.short <- 'double'
  } else if (stringr::str_starts(parts[[1]][3], 'U')){
    if (stringr::str_detect(parts[[1]][3], 'CHAR')){
      type.short <- NA
    } else {
      type.short <- 'integer'
    }
  } else if(parts[[1]][3] == "DOUBLE"){
    type.short <- 'double'
  } else if(parts[[1]][3] == "SHORT"){
    type.short <- 'integer'
  } else if(stringr::str_starts(parts[[1]][3], 'B')){
    type.short <- 'integer'
  } else {
    type.short <- NA
  }
  return(type.short)
}

#' Turns hdf5 type of NetCDF into gdal type
#' @param type.long Character. Hdf5 type of NetCDF file - retrieved from
#'   [ebvcube::ebv_properties()].
#' @return Character. Gdal type, specified with 'ot' argument.
#' @noRd
ebv_i_type_ot <- function(type.long){
  #####all types for gdal
  ot.list <-c('Byte', 'UInt16', 'Int16', 'UInt32', 'Int32', 'Float32', 'Float64', 'CInt16', 'CInt32', 'CFloat32', 'CFloat64')

  ###H5Types sorted
  types.byte <- c("H5T_STD_B8BE","H5T_STD_B8LE","H5T_NATIVE_B8")
  types.uint <- c("H5T_STD_U8BE","H5T_STD_U8LE","H5T_STD_U16BE","H5T_STD_U16LE","H5T_STD_U32BE",
                  "H5T_STD_U32LE", "H5T_NATIVE_USHORT", "H5T_NATIVE_UINT",
                  "H5T_NATIVE_UINT8", "H5T_NATIVE_UINT_LEAST8", "H5T_NATIVE_UINT_FAST8",
                  "H5T_NATIVE_UINT16", "H5T_NATIVE_UINT_LEAST16", "H5T_NATIVE_UINT_FAST16",
                  "H5T_NATIVE_UINT32","H5T_NATIVE_UINT_LEAST32","H5T_NATIVE_UINT_FAST32")
  types.int <- c("H5T_STD_I8BE","H5T_STD_I8LE","H5T_STD_I16BE","H5T_STD_I16LE","H5T_STD_I32BE",
                 "H5T_STD_I32LE", "H5T_NATIVE_SHORT", "H5T_NATIVE_INT",
                 "H5T_NATIVE_INT8", "H5T_NATIVE_INT_LEAST8", "H5T_NATIVE_INT_FAST8", "H5T_NATIVE_INT16",
                 "H5T_NATIVE_INT_LEAST16", "H5T_NATIVE_INT_FAST16", "H5T_NATIVE_INT32",
                 "H5T_NATIVE_INT_LEAST32","H5T_NATIVE_INT_FAST32")
  types.float <- c("H5T_IEEE_F32BE","H5T_IEEE_F32LE", "H5T_NATIVE_FLOAT", "H5T_NATIVE_DOUBLE", "H5T_NATIVE_LDOUBLE", "H5T_IEEE_F64BE","H5T_IEEE_F64LE")
  types.notsupported <- c("H5T_NATIVE_CHAR","H5T_NATIVE_SCHAR","H5T_NATIVE_UCHAR","H5T_NATIVE_HADDR",
                          "H5T_NATIVE_OPAQUE", "H5T_NATIVE_HSIZE","H5T_NATIVE_HSSIZE","H5T_NATIVE_HERR",
                          "H5T_NATIVE_HBOOL", "H5T_C_S1", "H5T_FORTRAN_S1",
                          #all types referring to 64 bit and not beeing float: not supported, because not available as ot type
                          "H5T_NATIVE_B64", "H5T_STD_B64LE", "H5T_STD_B64BE","H5T_STD_U64BE",
                          "H5T_NATIVE_UINT64","H5T_NATIVE_UINT_LEAST64","H5T_NATIVE_UINT_FAST64",
                          "H5T_STD_U64LE", "H5T_STD_I64LE", "H5T_NATIVE_INT64",
                          "H5T_NATIVE_INT_LEAST64", "H5T_NATIVE_INT_FAST64", "H5T_STD_I64BE",
                          "H5T_NATIVE_ULONG", "H5T_NATIVE_ULLONG","H5T_NATIVE_LONG","H5T_NATIVE_LLONG",
                          #only byte: 8 bit supportet
                          "H5T_STD_B16BE","H5T_STD_B16LE","H5T_STD_B32BE","H5T_STD_B32LE","H5T_NATIVE_B16", "H5T_NATIVE_B32")


  if(type.long %in% types.int){
    ot = 'Int'
  } else if(type.long %in% types.uint){
    ot = 'UInt'
  } else if(type.long %in% types.byte){
    ot = 'Byte'
  } else if(type.long %in% types.float){
    ot = 'Float'
  } else {
    ot = NULL
  }

  if (!is.null(ot)){
    #float and int
    if (ot != 'Byte' & ot != 'UInt'){
      #get bytes
      bytes <- regmatches(type.long, gregexpr("[[:digit:]]+", type.long))[[1]][2]
      #bytes are declared in type
      if(! is.na(bytes)){
        #catch bytes smaller 16
        if(as.integer(bytes)<16){
          #for int paste 16 (smalles bytes available)
          if (ot == 'Int'){
            ot <- paste0(ot, '16')
          } else {
            #for float paste 32 (smalles bytes available)
            ot <- paste0(ot, '32')
          }
        } else{
          #paste 'real' bytes if bigger 16
          ot <- paste0(ot, bytes)
        }
      } else{
        #bytes not declared in type
        if (ot =='Float'){
          if (stringr::str_detect(type.long, 'DOUBLE')){
            ot <- paste0(ot, 64) #paste 64 bit for double
          } else {
            ot <- paste0(ot, 32) #paste 32 bit for float
          }

        } else {
          if (stringr::str_detect(type.long, 'SHORT')){
            ot <- paste0(ot, 16) #16 bit for short
          } else {
            ot <- paste0(ot, 32) #highest for other types
          }
        }
      }
    } else if (ot == 'UInt'){
      # check UInt types
      bytes <- regmatches(type.long, gregexpr("[[:digit:]]+", type.long))[[1]][2]
      if(! is.na(bytes)){
        if(as.integer(bytes)==16 | as.integer(bytes)==32){
          ot <- paste0(ot, bytes)
        } else if (as.integer(bytes)==8){
          ot <- 'UInt16'
        }
      } else {
        if (stringr::str_detect(type.long, 'SHORT')){
          ot <- 'UInt16' #16 bit for short
        } else {
          ot <- 'UInt32' #highest for other types
        }
      }
    }

    if (! ot %in% ot.list){
      ot = NULL
    }
  }

  return(ot)
}

#' Turns hdf5 type of NetCDF into raster type
#' @param type.long Character. Hdf5 type of NetCDF file - retrieved from
#'   [ebvcube::ebv_properties()].
#' @return Character. Raster type of raster package in R.
#' @noRd
ebv_i_type_raster <- function(datanotation, byteorder){
  types.raster <- c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')
  type.hdf <- ''
  #define FLOAT type
  if (substr(datanotation, 1,3)=='FLT'){
    type.hdf <- paste0(type.hdf, 'H5T_IEEE_F')
    #check bytes
    if (substr(datanotation, 4,4)=='4'){
      type.hdf <- paste0(type.hdf,'32')
    }else if (substr(datanotation, 4,4)=='8'){
      type.hdf <- paste0(type.hdf,'64')
    }
    #define INT type e.g. "H5T_STD_I8BE" OR "H5T_STD_U8BE"
  } else if (substr(datanotation, 1,3)=='INT'){
    #check signed or unsigned
    if (substr(datanotation, 5,5)=='S'){
      type.hdf <- paste0(type.hdf, 'H5T_STD_I')
    } else if (substr(datanotation, 5,5)=='U') {
      type.hdf <- paste0(type.hdf, 'H5T_STD_U')
    }
    #check bytes
    if (substr(datanotation, 4,4)=='1'){
      type.hdf <- paste0(type.hdf,'8')
    }else if (substr(datanotation, 4,4)=='2'){
      type.hdf <- paste0(type.hdf,'16')
    } else if (substr(datanotation, 4,4)=='4'){
      type.hdf <- paste0(type.hdf,'32')
    }else if (substr(datanotation, 4,4)=='8'){
      type.hdf <- paste0(type.hdf,'64')
    }
  }
  #check LE or BE
  if(byteorder=='little'){
    type.hdf <- paste0(type.hdf,'LE')
  } else if(byteorder=='big'){
    type.hdf <- paste0(type.hdf,'BE')
  }
  #check if type  is created correctly
  if (!type.hdf %in% rhdf5::h5const("H5T")){
    warning(paste0('Output type for HDF file not created correctly: ', type.hdf,
                   '\nPlease contact developer of package.\nInput: ',
                   datanotation, ' and ', byteorder, '\nDefault type will be used: H5T_NATIVE_DOUBLE.'))
    type.hdf <- 'H5T_NATIVE_DOUBLE'
  }
  return(type.hdf)
}

#' Transforms the bounding boxs to another epsg. Used in
#' [ebvcube::ebv_data_read_bb()].
#' @param bb Bounding box corresponding to src_epsg.
#' @param src_epsg Current epsg of the bounding box.
#' @param dest_epsg New epsg of the bounding box.
#' @return Vector. Bounding box values with dest_epsg.
#' @noRd
ebv_i_transform_bb <- function(bb, src_epsg, dest_epsg){
  #src epsg: epsg given for the bb
  #dest epsg: epsg of the nc --> epsg of the returned bb
  crs <- gdalUtils::gdalsrsinfo(paste0("EPSG:", src_epsg))
  crs_src <- sp::CRS(stringr::str_remove(paste(crs[2], collapse = ' '), 'PROJ.4 : '))
  crs <- gdalUtils::gdalsrsinfo(paste0("EPSG:", dest_epsg))
  crs_dest <- sp::CRS(stringr::str_remove(paste(crs[2], collapse = ' '), 'PROJ.4 : '))

  p1 <- matrix(data = c(bb[1],bb[3]), nrow = 1, ncol = 2)
  sp1 <- sp::SpatialPoints(p1, proj4string=crs_src)

  p2 <- matrix(data = c(bb[2],bb[4]), nrow = 1, ncol = 2)
  sp2 <- sp::SpatialPoints(p2, proj4string=crs_src)

  sp1_new <- sp::spTransform(sp1, crs_dest)
  sp2_new <- sp::spTransform(sp2, crs_dest)
  bb_new <- c(sp1_new@coords[1], sp2_new@coords[1], sp1_new@coords[2], sp2_new@coords[2])
  return(bb_new)
}

#' Checks if there is enough memory to load the specified array.
#' @param dims x and y dimensions.
#' @param timestep Timesteps indicated by user - uses the length of it.
#' @param type R type returned by [ebvcube::ebv_i_type_r()].
#' @return Throws an error if the RAM restrictions are surpassed.
#' @noRd
ebv_i_check_ram <- function(dims, timestep, entity, type){
  #amount of pixels
  size <- as.numeric(dims[1])*dims[2]*length(timestep)*length(entity)
  if(!is.na(ebv_i_type_r(type))){
    if(ebv_i_type_r(type)=='integer'){
      ###integer: size*4 = bytes
      bytes <- size * 4 + 224
      ram.var.gb <- bytes/(1024^3)
    }else if(ebv_i_type_r(type)=='double'){
      ###float: size*8 = bytes
      bytes <- size * 8 + 224
      ram.var.gb <- bytes/(1024^3)
    }
    #get ram pc
    ram.pc <- ebv_i_ram()
    ram.pc.free <- ram.pc[2]
    ram.pc.total <- ram.pc[1]
    #check if data too big
    if(ram.pc.free < ram.var.gb){
      stop(paste0('The space needed to read the data into memory is larger than the free RAM.\nFree RAM: ', ram.pc.free, '\nNeeded RAM: ', round(ram.var.gb,2)))
    }
    #at least 1 GB stays free
    if(ram.pc.free - ram.var.gb < 1){
      stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore_RAM = TRUE.')
    }
    # #at least 15% stay free
    # if((ram.pc.total*0.15) > ram.var.gb){
    #   stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore_RAM = TRUE.')
    # }
  } else{
    message('Invalid type. RAM check ignored.')
  }
}

#' Check if data in datacube is missing
#'
#' @param hdf datahandle of file
#' @param datacubepath character to datacube
#' @param timestep vector of integers. optional.
#' @return Returns vector of bands that are empty. Else returns empty vector.
#' @noRd
ebv_i_check_data <- function(hdf, datacubepath, entity_index, is_4D, timestep=NULL){
  if (is.null(timestep)){
    did <- hdf&datacubepath
    file_space <- rhdf5::H5Dget_space(did)
    timestep <- 1:rhdf5::H5Sget_simple_extent_dims(file_space)$size[3]
    rhdf5::H5Dclose(did)
  }
  result <- c()
  for (t in timestep){
    r <- tryCatch(
      {
        if(is_4D){
          r <- rhdf5::h5read(hdf, datacubepath, start = c(1,1,t,entity_index), count=c(1,1,1,1))
        }else{
          r <- rhdf5::h5read(hdf, datacubepath, start = c(1,1,t), count=c(1,1,1))
        }

      },
      error = function(e){
        print(e)
        #message(paste0('Timestep ', t, ' holds no data.'));
        r <- TRUE
      }
    )
    if(is.na(r)){

    }else if(r == TRUE){
      result <- c(result, t)
    }
  }
  return(result)
}

#' Adds uint attribute to a specified h5object.
#' @param h5obj H5object to write the attribute to, see
#'   \href{https://rdocumentation.org/packages/rhdf5/versions/2.16.0}{rhdf5
#'   package}.
#' @param name Characer. Name of the attribute.
#' @param data Numerical. Vaule to be written to Attribute.
#' @noRd
ebv_i_uint_att <- function(h5obj, name, data){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  withr::defer(
    if(exists('(sid)')){
      if(rhdf5::H5Iis_valid((sid))==TRUE){rhdf5::H5Sclose((sid))}
    }
  )
  # add attribute ----
  if(!rhdf5::H5Aexists(h5obj, name)){
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_UINT")
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
  }
  rhdf5::H5Awrite(aid, as.numeric(data))
  rhdf5::H5Aclose(aid)
}

#' Adds int attribute to a specified h5object.
#' @param h5obj H5object to write the attribute to, see
#'   \href{https://rdocumentation.org/packages/rhdf5/versions/2.16.0}{rhdf5
#'   package}.
#' @param name Characer. Name of the attribute.
#' @param data Numerical. Vaule to be written to Attribute.
#' @noRd
ebv_i_int_att <- function(h5obj, name, data){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  withr::defer(
    if(exists('(sid)')){
      if(rhdf5::H5Iis_valid((sid))==TRUE){rhdf5::H5Sclose((sid))}
    }
  )
  # add attribute ----
  if(!rhdf5::H5Aexists(h5obj, name)){
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_INT")
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
  }
  rhdf5::H5Awrite(aid, as.numeric(data))
  rhdf5::H5Aclose(aid)
}

#' Adds numerical attribute to a specified h5object.
#' @param h5obj H5object to write the attribute to, see
#'   \href{https://rdocumentation.org/packages/rhdf5/versions/2.16.0}{rhdf5
#'   package}.
#' @param name Characer. Name of the attribute.
#' @param data Numerical. Vaule to be written to Attribute.
#' @noRd
ebv_i_num_att <- function(h5obj, name, data){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  withr::defer(
    if(exists('(sid)')){
      if(rhdf5::H5Iis_valid((sid))==TRUE){rhdf5::H5Sclose((sid))}
    }
  )
  # add attribute ----
  if(!rhdf5::H5Aexists(h5obj, name)){
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_NATIVE_DOUBLE")
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid, sid)
    rhdf5::H5Sclose(sid)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
  }
  rhdf5::H5Awrite(aid, as.numeric(data))
  rhdf5::H5Aclose(aid)
}

#' Adds character attribute to a specified h5object.
#' @param h5obj H5object to write the attribute to, see
#'   \href{https://rdocumentation.org/packages/rhdf5/versions/2.16.0}{rhdf5
#'   package}.
#' @param name Characer. Name of the attribute.
#' @param data Characer. Vaule to be written to Attribute.
#' @noRd
ebv_i_char_att <- function(h5obj, name, data){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  withr::defer(
    if(exists('(sid)')){
      if(rhdf5::H5Iis_valid((sid))==TRUE){rhdf5::H5Sclose((sid))}
    }
  )
  # add attribute ----
  count <- 1+ length(charToRaw(data))
  # count <- 1
  # for (u in c('\ufc', '\uf6', '\ue4', '\udf', '\udc', '\uc4', '\ud6',
  #             '\u60', '\ub4')){
  #   count <- count + stringr::str_count(data, u)
  # }
  # count <- count + 10
  if(!rhdf5::H5Aexists(h5obj, name)){
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, count) #nchar(data)+count
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  } else {
    rhdf5::H5Adelete(h5obj, name)
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, count) #nchar(data)+count
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  }
  rhdf5::H5Awrite(aid, data)
  rhdf5::H5Aclose(aid)
}

#' Reads the attribute value of specified h5object and attribute.
#' @param h5obj H5object where the attribute is located, see
#'   \href{https://rdocumentation.org/packages/rhdf5/versions/2.16.0}{rhdf5
#'   package}.
#' @param name Characer. Name of the attribute.
#' @return Value of the attribute.
#' @noRd
ebv_i_read_att <-  function(h5obj, name){
  # ensure file and all datahandles are closed on exit ----
  withr::defer(
    if(exists('aid')){
      if(rhdf5::H5Iis_valid(aid)==TRUE){rhdf5::H5Aclose(aid)}
    }
  )
  # read attribute ----
  #check if attribute exists
  if(!rhdf5::H5Aexists(h5obj, name)){
    warning(paste0('The attribute ', name, ' does not exist. Or maybe wrong location in NetCDF?\n'))
    return(NULL)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
    attribute <- rhdf5::H5Aread(aid)
    rhdf5::H5Aclose(aid)
    return(attribute)
  }
}

#' Checks whether or not a file corresponding to the 4D or the 3D structure.
#'
#' @param filepath character. Path to netCDF file.
#' @return Logical. TRUE if 4D, FALSE if 3D.
#' @noRd
ebv_i_4D <- function(filepath){
  withr::local_options(list(warn = -1)) #suppress warning if attribute does not exist
  hdf <- rhdf5::H5Fopen(filepath, flags = "H5F_ACC_RDONLY")
  dims <- ebv_i_read_att(hdf, 'ebv_cube_dimensions')
  if (is.null(dims)){
    dim <- 3
  } else if(stringr::str_detect(dims,'entity')){
    dim <- 4
  } else{
    dim <- 3
  }
  rhdf5::H5Fclose(hdf)
  if(dim==3){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

#' Check given entity argument
#'
#' @param entity Integer or character value pointing to entity in cube
#' @param entity_names Vector of entity names (character) derived by ebv_properties
#'
#' @return Throws error if entity argument is not valid.
#' @noRd
ebv_i_entity <- function(entity, entity_names){
  if(!is.null(entity)){
    #integer inside the range?
    if(checkmate::checkIntegerish(entity, len=1) == TRUE){
      if(entity > length(entity_names)){
        stop(paste0('Given entity value (', entity,') bigger than available entities (',length(entity_names),').'))
      }
      #name correct?
    } else if (checkmate::checkCharacter(entity)==TRUE){
      if(!entity %in% entity_names){
        stop('Given entity name is not valid. Check ebv_properties(filepath)@general$entity_names for available entity names.')
      }
      #turn entity name into index number
      entity <- which(entity_names==entity)
    }else{
      stop('Entities must be of type integer or character.')
    }
  }
}

