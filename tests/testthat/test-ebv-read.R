# basic tests for ebv_read* ----
test_that("test ebv_read", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read(file, 'metric_1/ebv_cube', 1, 1)
  value <- as.numeric(data[40,45])
  expect_equal(value, -0.003191974)
})


test_that("test ebv_read_bb", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_bb(file, 'metric_1/ebv_cube', 1, 1, c(-20,0,-30, -20))
  dims <- dim(data)
  expect_equal(dims, c(10,20,1))
})


test_that("test ebv_read_shp", {
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_shp(file, 'metric_1/ebv_cube', entity=1, shp = shp, timestep = 3)
  dims <- dim(data)
  value <- as.numeric(data[5,5])
  expect_equal(dims, c(12,9,1))
  expect_equal(value, 0.79184753)
})
