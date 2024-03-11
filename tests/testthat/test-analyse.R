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

