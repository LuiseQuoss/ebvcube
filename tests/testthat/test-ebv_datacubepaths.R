test_that("datacubepaths martins_comcom_id1_20220208_v1.nc", {
  file <- system.file(file.path("extdata","martins_comcom_id1_20220208_v1.nc"), package="ebvcube")
  result <- data.frame(c('metric_1/ebv_cube', 'metric_2/ebv_cube'))
  result <- cbind(result, c('Relative change in the number of species (%)','Absolute change in the number of species'))
  colnames(result) <- c('datacubepaths', 'metric_names')
  datacubes <- ebv_datacubepaths(file)
  expect_equal(datacubes, result)
})
