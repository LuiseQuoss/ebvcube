test_that("test ebv_create", {
  #basic paths
  root <- system.file(file.path("extdata/testdata"), package="ebvcube")
  json <- file.path(root, 'cSAR-iDiv.json')
  csv <- file.path(root, 'entities.csv')
  out <- file.path(system.file(package="ebvcube"), "extdata/testdata", 'test_create.nc')

  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(1, 1)
  fillvalue <- -3.40282e+38
  prec <- 'float'
  epsg <- 4326
  sep <- ','
  timesteps <- c('1900', '2015', '2050')

  #create empty file
  expect_silent(ebv_create(jsonpath = json,
             outputpath = out,
             entities = csv,
             epsg = epsg,
             extent = extent,
             timesteps = timesteps,
             fillvalue = fillvalue,
             prec = prec,
             sep = sep,
             force_4D = TRUE,
             overwrite = TRUE,
             verbose = FALSE))

  expect_equal(as.Date(file.info(out)$mtim), Sys.Date())

})
