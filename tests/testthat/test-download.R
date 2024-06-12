# basic tests for ebv_download ----
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
