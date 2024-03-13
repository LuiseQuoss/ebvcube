# basic tests for ebv_resample ----
test_that("test ebv_resample single timestep, given resolution", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_resample(file, 'metric_1/ebv_cube', 1, 1, c(0.33,0.33, 4326),
                       file.path(tempdir(), 'test.tif'), return_raster=T)
  file.remove(file.path(tempdir(), 'test.tif'))
  value <- as.numeric(data[40,45])
  expect_equal(value, 0.02706515417)
  expect_equal(dim(data), c(258, 273, 1))
})


# test_that("test ebv_resample single timestep, given second netCDF", {
#   file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#   data <- ebv_resample(file, 'metric_1/ebv_cube', 1, 1, c(0.33,0.33, 4326),
#                        file.path(tempdir(), 'test.tif'), return_raster=T)
#   file.remove(file.path(tempdir(), 'test.tif'))
#   value <- as.numeric(data[40,45])
#   expect_equal(value, 0.03)
#   expect_equal(dim(data), c(258, 273, 1))
# })
