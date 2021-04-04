#basics
is.empty <- function(x){
 if(length(x)==0){
   return(TRUE)
 }
}

ebv_i_os <- function(){
  temp <- Sys.info()
  return(as.character(temp[1]))
}

ebv_i_file_opened <- function(filepath){
  if (ebv_i_os() =='Linux'){
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
    print('not yet implemented for windows')
  } else {
    print('OS not supported.')
  }
}

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

#type functions
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

#transform bb and ram check
ebv_i_transform_bb <- function(bb, src_epsg, dest_epsg){
  #src epsg: epsg given for the bb
  #dest epsg: epsg of the nc --> epsg of the returned bb

  crs_src <- sp::CRS(paste0(SRS_string = "EPSG:", src_epsg))
  crs_dest <- sp::CRS(paste0(SRS_string = "EPSG:", dest_epsg))

  p1 <- matrix(data = c(bb[1],bb[3]), nrow = 1, ncol = 2)
  sp1 <- sp::SpatialPoints(p1, proj4string=crs_src)

  p2 <- matrix(data = c(bb[2],bb[4]), nrow = 1, ncol = 2)
  sp2 <- sp::SpatialPoints(p2, proj4string=crs_src)

  sp1_new <- sp::spTransform(sp1, crs_dest)
  sp2_new <- sp::spTransform(sp2, crs_dest)
  bb_new <- c(sp1_new@coords[1], sp2_new@coords[1], sp1_new@coords[2], sp2_new@coords[2])
  return(bb_new)
}

ebv_i_check_ram <- function(dims, timestep, type){
  #amount of pixels
  size <- dims[1]*dims[2]*length(timestep)
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
    #check if data too big
    if(ram.pc.free < ram.var.gb){
      stop(paste0('The RAM needed to read the data into memory is larger than the free RAM.\nFree RAM: ', ram.pc.free, '\nNeeded RAM: ', round(ram.var.gb,2)))
      #check that 1/2 GB RAM stay free
    }
    if(ram.pc.free - ram.var.gb < 0.5){
      stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore.RAM = TRUE.')
    }
    #check if reading huge dataset
    if((ram.pc.free/4) < ram.var.gb){
      stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore.RAM = TRUE.')
    }
  } else{
    message('Invalid type. RAM check ignored.')
  }
}

#attribute functions
ebv_i_uint_att <- function(h5obj, name, data){
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

ebv_i_int_att <- function(h5obj, name, data){
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

ebv_i_num_att <- function(h5obj, name, data){
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

ebv_i_char_att <- function(h5obj, name, data){
  count <- 1
  #data.split <- strsplit(data,'')[[1]]
  for (u in c('\ufc', '\uf6', '\ue4', '\udf', '\udc', '\uc4', '\ud6')){ #c('ü', 'ö', 'ä', 'ß', 'Ü', 'Ä', 'Ö')
    count <- count + stringr::str_count(data, u)
  }
  if(!rhdf5::H5Aexists(h5obj, name)){
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, nchar(data)+count)
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  } else {
    rhdf5::H5Adelete(h5obj, name)
    sid <- rhdf5::H5Screate_simple(c(1))
    tid <- rhdf5::H5Tcopy("H5T_C_S1")
    rhdf5::H5Tset_size(tid, nchar(data)+count)
    aid <- rhdf5::H5Acreate(h5obj, name = name, tid,sid)
    rhdf5::H5Sclose(sid)
  }
  rhdf5::H5Awrite(aid, data)
  rhdf5::H5Aclose(aid)
}

ebv_i_read_att <-  function(h5obj, name){
  #check if attribute exists
  if(!rhdf5::H5Aexists(h5obj, name)){
    warning(paste0('The attribute ', name, ' does not exist. Or maybe wrong location in NetCDF?'))
    return(NULL)
  } else {
    aid <- rhdf5::H5Aopen(h5obj, name)
    attribute <- rhdf5::H5Aread(aid)
    rhdf5::H5Aclose(aid)
    return(attribute)
  }
}


