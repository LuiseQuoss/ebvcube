ebv_i_os <- function(){
  temp <- Sys.info()
  return(as.character(temp[1]))
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

