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

test_that("test ebv_write and ebv_read DelayedMatrix Array", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  #read data as array and delayedMatrix
  data_dm <- ebv_read(file, 'metric_1/ebv_cube', entity=1, timestep = 6, type='da')
  data_da <- ebv_read(file, 'metric_1/ebv_cube', entity=1, timestep = 6:7, type='da')
  data_a <- ebv_read(file, 'metric_1/ebv_cube', entity=1, timestep = 6:7, type='a')
  values_dm <- as.numeric(data_dm[44:50,50])
  values_a1 <- as.numeric(data_a[44:50,50,1])
  values_a2 <- as.numeric(data_a[44:50,50,2])
  values_da <- as.numeric(data_da[44:50,50,2])
  ext <- ebv_properties(file, verbose=FALSE)@spatial$extent
  #check values and the classes
  expect_equal(class(data_a),'array')
  expect_equal(as.character(class(data_dm)), 'DelayedMatrix')
  expect_equal(as.character(class(data_da)), 'DelayedArray')
  expect_equal(values_dm, values_a1)
  expect_equal(values_da, values_a2)
  #tempfile
  tempfile <- tempfile(fileext='.tif')
  #write delayedMatrix
  path <- ebv_write(data_dm, tempfile, epsg = 4326, extent =  ext, verbose=FALSE)
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
  #write delayedArray
  path <- ebv_write(data_da, tempfile, epsg = 4326, extent =  ext, verbose=FALSE)
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
  #write array
  path <- ebv_write(data_a, tempfile, epsg = 4326, extent =  ext, verbose=FALSE)
  expect_true(basename(path) %in% list.files(dirname(tempfile)))
  file.remove(tempfile)
})

