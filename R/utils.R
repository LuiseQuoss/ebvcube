#basics
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

# ebv_i_type_ot
# ebv_i_type_raster



#transform bb and ram check

# ebv_i_check_ram <- function(dims, timestep, type){
#   #amount of pixels
#   size <- dims[1]*dims[2]*length(timestep)
#   if(!is.na(ebv_i_type_r(type))){
#     if(ebv_i_type_r(type)=='integer'){
#       ###integer: size*4 = bytes
#       bytes <- size * 4 + 224
#       ram.var.gb <- bytes/(1024^3)
#     }else if(ebv_i_type_r(type)=='double'){
#       ###float: size*8 = bytes
#       bytes <- size * 8 + 224
#       ram.var.gb <- bytes/(1024^3)
#     }
#     #get ram pc
#     ram.pc <- ebv_i_ram()
#     ram.pc.free <- ram.pc[2]
#     #check if data too big
#     if(ram.pc.free < ram.var.gb){
#       stop(paste0('The RAM needed to read the data into memory is larger than the free RAM.\nFree RAM: ', ram.pc.free, '\nNeeded RAM: ', round(ram.var.gb,2)))
#       #check that 1/2 GB RAM stay free
#     }
#     if(ram.pc.free - ram.var.gb < 0.5){
#       stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore.RAM = TRUE.')
#     }
#     #check if reading huge dataset
#     if((ram.pc.free/4) < ram.var.gb){
#       stop('Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore.RAM = TRUE.')
#     }
#   } else{
#     message('Invalid type. RAM check ignored.')
#   }
# }


#attribute functions

