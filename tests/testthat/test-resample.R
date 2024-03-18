# basic tests for ebv_resample ----
test_that("test ebv_resample single timestep, given resolution", {
  file <- system.file(file.path("extdata", "martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_resample(file, 'metric_1/ebv_cube', 1, 1, c(0.33, 0.33, 4326),
                       file.path(tempdir(), 'test.tif'), return_raster=TRUE,
                       verbose = FALSE)
  file.remove(file.path(tempdir(), 'test.tif'))
  value <- as.numeric(data[40, 45])
  expect_equal(value, 0.02706515417)
  expect_equal(dim(data), c(258, 273, 1))
})

test_that("test ebv_resample multiple timesteps, given resolution, metric", {
  file <- system.file(file.path("extdata", "martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_resample(file, entity= 'forest bird species', timestep=2:3, resolution =c(0.33, 0.33, 4326),
                       outputpath = file.path(tempdir(), 'test.tif'), return_raster=TRUE,
                       metric=2,verbose = FALSE)
  file.remove(file.path(tempdir(), 'test.tif'))
  value <- as.numeric(data[40, 45])
  expect_equal(value, c(0.0307007413357496, 0.057515699416399))
  expect_equal(dim(data), c(258, 273, 2))
})

test_that("test ebv_resample single timestep, given second netCDF", {
  file_src <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  file_dest <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  data <- ebv_resample(file_src, 'metric_1/ebv_cube', 1, 1, file_dest,
                       file.path(tempdir(), 'test_1.tif'), return_raster=T,
                       verbose=FALSE)
  file.remove(file.path(tempdir(), 'test_1.tif'))
  value <- as.numeric(data[40,45])
  expect_equal(value, -0.003191974014)
  expect_equal(dim(data), c(85, 90,  1))
})
