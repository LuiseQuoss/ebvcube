test_that("test ebv_create_taxonomy", {
  #test ebv_create_taxonomy without lsid ----

  #basic paths
  root <-'I:\\biocon\\Quoss_Luise\\netcdf\\taxonomy'
  json <- file.path(root, '35.json')
  taxonomy <- file.path(root, 'List_reptiles_20052022_function.csv')
  file <- tempfile(fileext='.nc')

  #spatial info
  epsg = 4326
  extent = c(-180.0000000000000000,179.9999999999999432,-89.9999999999999716, 90.0000000000000000)
  resolution = c(0.4166666666666666297, 0.4166666666666666297)
  fillvalue = 0
  prec = 'byte'
  sep=';'

  #create empty file
  expect_silent(ebv_create_taxonomy(jsonpath = json,
                     outputpath = file,
                     taxonomy = taxonomy,
                     lsid=FALSE,
                     epsg = epsg,
                     extent = extent,
                     timesteps = '2020-12-31',
                     fillvalue = fillvalue,
                     prec = prec,
                     sep = sep,
                     force_4D = TRUE,
                     overwrite = TRUE,
                     verbose = FALSE))

  #test the elements of the taxonomy
  hdf <- rhdf5::H5Fopen(file)

  #entity_list
  entity_list.id <- rhdf5::H5Dopen(hdf, 'entity_list')
  expect_equal(trimws(paste0(entity_list.id[1,1,], collapse = '')), "")
  expect_equal(trimws(paste0(entity_list.id[2,1,], collapse = '')), "Ablepharus")
  expect_equal(trimws(paste0(entity_list.id[3,1,], collapse = '')), "SCINCIDAE")
  expect_equal(trimws(paste0(entity_list.id[4,1,], collapse = '')), "SAURIA")
  expect_equal(trimws(paste0(entity_list.id[5,1,], collapse = '')), "Least Concern")
  rhdf5::H5Dclose(entity_list.id)

  #no lsid created
  expect_equal(rhdf5::H5Lexists(hdf, 'entity_lsid'), FALSE)

  #taxon level names
  entity_levels.id <- rhdf5::H5Dopen(hdf, 'entity_levels')
  expect_equal(trimws(paste0(entity_levels.id[1,], collapse = '')), "Species")
  expect_equal(trimws(paste0(entity_levels.id[2,], collapse = '')), "Genus")
  expect_equal(trimws(paste0(entity_levels.id[3,], collapse = '')), "Family")
  expect_equal(trimws(paste0(entity_levels.id[4,], collapse = '')), "Order")
  expect_equal(trimws(paste0(entity_levels.id[5,], collapse = '')), "RedlistCategory")
  rhdf5::H5Dclose(entity_levels.id)

  rhdf5::H5Fclose(hdf)

  #remove file
  file.remove(file)


})
