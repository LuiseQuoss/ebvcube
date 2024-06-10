test_that("ebv_datacubepaths martins_comcom_id1_20220208_v1.nc", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  result <- data.frame(c('metric_1/ebv_cube', 'metric_2/ebv_cube'))
  result <- cbind(result, c('Relative change in the number of species (%)','Absolute change in the number of species'))
  colnames(result) <- c('datacubepaths', 'metric_names')
  datacubes <- ebv_datacubepaths(file, verbose=FALSE)
  expect_equal(datacubes, result)
})

test_that("ebv_datacubepaths pereira_csar_bes_sim_20220830_4d.nc", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  result <- data.frame(c('scenario_1/metric_1/ebv_cube', 'scenario_1/metric_2/ebv_cube',
                         'scenario_1/metric_3/ebv_cube',
                         'scenario_2/metric_1/ebv_cube', 'scenario_2/metric_2/ebv_cube',
                         'scenario_2/metric_3/ebv_cube',
                         'scenario_3/metric_1/ebv_cube', 'scenario_3/metric_2/ebv_cube',
                         'scenario_3/metric_3/ebv_cube'))
  result <- cbind(result, c('SSP1-RCP1.5 LU','SSP1-RCP1.5 LU','SSP1-RCP1.5 LU',
                            'SSP3-RCP6.0 LU','SSP3-RCP6.0 LU','SSP3-RCP6.0 LU',
                            'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU'))
  result <- cbind(result, c('Species richness (S)','Relative species richness change (Delta_S)',
                            'Diversity-weighted relative species richness change (Delta_SS)'))
  colnames(result) <- c('datacubepaths', 'scenario_names', 'metric_names')
  datacubes <- ebv_datacubepaths(file, verbose=FALSE)
  #compare results - leave out scenario 2 as its attributes get changed in another test
  expect_equal(dim(datacubes), dim(result))
  expect_equal(datacubes[1:3,], result[1:3,])
  expect_equal(datacubes[7:9,], result[7:9,])
})
