# basic tests for ebv_download ----
#check url
portal_down <- ebv_i_check_url('https://portal.geobon.org/api/v1/datasets')

if(portal_down){
  #expect an error
  expect_error(ebv_download(27, dir, verbose = FALSE))
  }else{
  #run 'normal' tests
  test_that("test ebv_download ID=numeric", {
    dir <- tempdir()
    data <- ebv_download(27, dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
    #clean
    unlink(dir, recursive=TRUE)
  })

  test_that("test ebv_download ID=doi", {
    dir <- tempdir()
    data <- ebv_download('10.25829/f2rdp4', dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
    #clean
    unlink(dir, recursive=TRUE)
  })

  test_that("test ebv_download ID=title 1", {
    dir <- tempdir()
    data <- ebv_download('Local bird diversity (cSAR/BES-SIM)', dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
    #clean
    unlink(dir, recursive=TRUE)
  })

  test_that("test ebv_download ID=title 2", {
    dir <- tempdir()
    data <- ebv_download("Global trends in biodiversity (BES-SIM PREDICTS)", dir, verbose = FALSE)
    expect_true(basename(data) %in% list.files(dir))
    #clean
    unlink(dir, recursive=TRUE)
  })

}


