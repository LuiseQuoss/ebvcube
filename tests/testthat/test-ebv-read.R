# basic tests for ebv_read* ----
test_that("test ebv_read single timestep", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read(file, 'metric_1/ebv_cube', 1, 1, verbose = FALSE)
  value <- as.numeric(data[40,45])
  expect_equal(value, -0.003191974)
})

test_that("test ebv_read multiple timestep", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read(file, 'metric_1/ebv_cube', 1, 1:3, verbose = FALSE)
  value <- as.numeric(data[40,45,3])
  expect_equal(value, 0.01546155)
})

test_that("test ebv_read ISO date + metric&scenario", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read(file, entity = 1, timestep = c("2000-01-01", "2010-01-01"),
                   metric= 2, verbose = FALSE)
  value <- as.numeric(data[40,45])
  expect_equal(value, c(3.05687881, 3.60405803))
})

test_that("test ebv_read_bb single timestep", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_bb(file, 'metric_1/ebv_cube', 1, 1, c(-20,0,-30, -20), verbose = FALSE)
  dims <- dim(data)
  expect_equal(dims, c(10,20,1))
})

test_that("test ebv_read_bb multiple timesteps", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_bb(file, 'metric_1/ebv_cube', 1, 1:3, c(-10,0,-10, 0), verbose = FALSE)
  dims <- dim(data)
  expect_equal(dims, c(10,10,3))
})

test_that("test ebv_read_bb ISO date + metric&scenario", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_bb(file, entity = 1, timestep = c("1950-01-01", "1960-01-01"),
                      bb=c(-20,0,-30, -20), metric = 2, verbose = FALSE)
  dims <- dim(data)
  expect_equal(dims, c(10,20,2))
})

test_that("test ebv_read_shp single timestep", {
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_shp(file, 'metric_1/ebv_cube', entity=1, shp = shp,
                       timestep = 3, verbose = FALSE)
  dims <- dim(data)
  value <- as.numeric(data[5,5])
  expect_equal(dims, c(12,9,1))
  expect_equal(value, 0.79184753)
})

test_that("test ebv_read_shp multiple timesteps", {
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_shp(file, 'metric_1/ebv_cube', entity='forest bird species',
                       shp = shp, timestep = 1:3, verbose = FALSE)
  dims <- dim(data)
  value <- as.numeric(data[5,5,1])
  expect_equal(dims, c(12,9,3))
  expect_equal(value, 0.188491091)
})

test_that("test ebv_read_shp ISO date + metric&scenario", {
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_read_shp(file, entity = 1, timestep = c("2000-01-01", "2010-01-01"),
                   metric= 2, shp = shp, verbose = FALSE)
  dims <- dim(data)
  value <- as.numeric(data[5,5,1])
  expect_equal(dims, c(12,9,2))
  expect_equal(value, 14.0923309)
})
