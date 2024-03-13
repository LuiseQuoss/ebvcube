# basic tests for ebv_analyse ----
test_that("test ebv_analyse", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_analyse(file, 'metric_1/ebv_cube', 1, 1)
  result <- list('min'=-0.13705571,
                 'q25'=0.0154907643,
                 'q50'=0.0907911,
                 'mean'=0.194210766,
                 'q75'=0.208051734,
                 'max'=1.92128694,
                 'std'=0.35596055,
                 'n'=7650,
                 'NAs'=4935

                 )

  expect_equal(data, result)
})

test_that("test ebv_analyse bb", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  data <- ebv_analyse(file, 'metric_1/ebv_cube', 1, 1, subset = c(0, 20, -40, -30))
  result <- list('min'=1.271487,
                 'q25'=1.6479932,
                 'q50'=1.70210266,
                 'mean'=1.6817669,
                 'q75'=1.77333808,
                 'max'=1.8359524,
                 'std'=0.149520647,
                 'n'=200,
                 'NAs'=187

  )

  expect_equal(data, result)
})

test_that("test ebv_analyse shp", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  shp <- system.file(file.path('extdata','cameroon.shp'), package="ebvcube")
  data <- ebv_analyse(file, 'metric_1/ebv_cube', 'forest bird species', 1, subset = shp)
  result <- list('min'=-0.0031939454,
                 'q25'=0.045318592,
                 'q50'=0.087303236,
                 'mean'=0.094001715,
                 'q75'=0.142445557,
                 'max'=0.213913336,
                 'std'=0.060112968,
                 'n'=108,
                 'NAs'=49

  )

  expect_equal(data, result)
})
