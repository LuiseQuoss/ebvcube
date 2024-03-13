# basic tests for ebv_write ----
test_that("test ebv_write bb", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_bb(file, 'metric_1/ebv_cube', 1, 1:3, c(-0,30,0, 10))
  tempfile <- tempfile(fileext='.tif')
  path <- ebv_write(data, tempfile, epsg = 4326, extent =  c(-10,0,-10, 0))
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
})

test_that("test ebv_write shp", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  data <- ebv_read_shp(file, 'metric_1/ebv_cube', entity=1, shp = shp, timestep = 2)
  ext <- as.numeric(terra::ext(data)[1:4])
  tempfile <- tempfile(fileext='.tif')
  path <- ebv_write(data, tempfile, epsg = 4326, extent =  ext)
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
})

test_that("test ebv_write", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read(file, 'metric_1/ebv_cube', entity=1, timestep = 6)
  ext <- as.numeric(terra::ext(data)[1:4])
  tempfile <- tempfile(fileext='.tif')
  path <- ebv_write(data, tempfile, epsg = 4326, extent =  ext)
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
})
