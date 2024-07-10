# basic tests for ebv_download ----
#check URL
invalid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),FALSE,TRUE)
}

portal_down <- invalid_url('https://portal.geobon.org/api/v1/datasets')

if(portal_down){
  #expect an error
  expect_error(ebv_download(27, dir, verbose = FALSE))
  }else{
  #run 'normal' tests
  test_that("test ebv_download ID=numeric", {
    dir <- tempdir()
    data <- ebv_download(27, dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
  })

  test_that("test ebv_download ID=doi", {
    dir <- tempdir()
    data <- ebv_download('10.25829/f2rdp4', dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
  })

  test_that("test ebv_download ID=title", {
    dir <- tempdir()
    data <- ebv_download('Local bird diversity (cSAR/BES-SIM)', dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
  })
}


