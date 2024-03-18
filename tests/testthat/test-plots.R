# basic tests for ebv_trend ----
test_that("test ebv_trend mean", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_trend(file, 'metric_1/ebv_cube', 1, 'mean', verbose=FALSE)
  result <- c(0.1942107659, 0.4044708142, 0.6872220579, 1.1244213666, 1.9991231704,
              2.7869646729, 3.0676681087, 3.2548067363, 3.5242074793, 3.7865669360,
              3.9818150659, 4.1326983586)

  expect_equal(data, result)
})

test_that("test ebv_trend mean shp", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  data <- ebv_trend(file, 'metric_1/ebv_cube', 1, 'mean', subset=shp, verbose=FALSE)

  result <- c(0.09400171525, 0.21479445399, 0.39628551204, 0.72306281071,
              1.53970290600, 1.38165782585, 1.48357049249, 1.65766914770,
              1.70345697817, 1.64006227166, 1.36429025353, 1.43566100246)

  expect_equal(data, result)
})


test_that("test ebv_trend min", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_trend(file, 'metric_1/ebv_cube', 1, 'min', verbose=FALSE)
  result <- c(-0.1370557100, -0.1924628168, -0.1929529756, -0.2531685531, -0.3318841159,
              -1.4445482492, -2.0090060234, -2.6203408241, -2.9994468689,
              -3.0023336411, -7.9151682854, -6.7608814240)

  expect_equal(data, result)
})

test_that("test ebv_trend max", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_trend(file, 'metric_1/ebv_cube', 'forest bird species', 'max', verbose=FALSE)
  result <- c(1.921286941,  3.377684593,  5.048823833,  7.500643253, 15.945466995,
              19.009712219, 19.526184082, 19.517824173, 19.440151215, 19.550523758,
              19.569660187, 19.569736481)

  expect_equal(data, result)
})

test_that("test ebv_trend boxplot", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_trend(file, 'metric_1/ebv_cube', 'forest bird species', 'boxplot', verbose=FALSE)
  result <- c(1.921286941,  3.377684593,  5.048823833,  7.500643253, 15.945466995,
              19.009712219, 19.526184082, 19.517824173, 19.440151215, 19.550523758,
              19.569660187, 19.569736481)

  expect_s3_class(data, 'gg')
})

test_that("test ebv_trend metric", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_trend(file, entity= 'forest bird species', method='mean',
                    metric = 2, verbose=FALSE)
  result <- c(0.6140730526,  1.2972443999,  2.2045309696,  3.6016083142,
              6.4691829863,  8.8957375043,  9.7711291175, 10.3299345397,
              10.9822654002, 11.5685089538, 11.9421034293, 12.3869482054)

  expect_equal(data, result)
})

# basic tests for ebv_map ----
test_that("test ebv_map 1", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  ebv_map(file, 'metric_1/ebv_cube', 1, 3, classes=5)
  p <- ggplot2::last_plot()
  expect_true(ggplot2::is.ggplot(p))
  expect_error(print(p), NA)
  expect_identical(p$labels$y, "longitude")
  expect_identical(p$labels$title, "Local bird diversity (cSAR/BES-SIM)")
  expect_identical(p$labels$subtitle, "Relative change in the number of species (%) - forest bird species (1920-01-01)")
  expect_identical(p$scales$scales[[1]]$labels, c(-0.1930,  0.0413,  0.2370,  0.5950,  1.0500,  5.0500))
  # expect_identical(p$scales$scales[[1]]$guide$title, "Percentage\npoints")
})

test_that("test ebv_map 2", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  ebv_map(filepath = file, entity = 'all bird species', timestep = "1950-01-01",
          metric = 'Relative change in the number of species (%)',
          classes = 7, verbose = FALSE)
  p <- ggplot2::last_plot()
  expect_true(ggplot2::is.ggplot(p))
  expect_error(print(p), NA)
  expect_identical(p$labels$y, "longitude")
  expect_identical(p$labels$title, "Local bird diversity (cSAR/BES-SIM)")
  expect_identical(p$labels$subtitle, "Relative change in the number of species (%) - all bird species (1950-01-01)")
  expect_identical(p$scales$scales[[1]]$labels, c(-1.69, 0.358, 0.788,  2.05, 3.11, 4.05, 5.91, 19.4))
  # expect_identical(p$scales$scales[[1]]$guide$title, "Percentage\npoints")
})
