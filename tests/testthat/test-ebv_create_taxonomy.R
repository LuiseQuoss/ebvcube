test_that("test ebv_create_taxonomy no lsid", {
  #test ebv_create_taxonomy without lsid ----

  #basic paths
  root <- system.file(file.path("extdata/testdata"), package="ebvcube")
  json <- file.path(root, '5.json')
  taxonomy <- file.path(root, 'id5_entities.csv')
  file <- tempfile(fileext='.nc')

  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(0.25, 0.25)
  fillvalue <- -127
  prec <- 'float'
  epsg <- 4326
  sep <- ','

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
  expect_equal(trimws(paste0(entity_list.id[1,1,], collapse = '')), "Hipposideros curtus")
  expect_equal(trimws(paste0(entity_list.id[2,1,], collapse = '')), "Hipposideridae")
  expect_equal(trimws(paste0(entity_list.id[3,1,], collapse = '')), "Chiroptera")
  rhdf5::H5Dclose(entity_list.id)

  #no lsid created
  expect_equal(rhdf5::H5Lexists(hdf, 'entity_lsid'), FALSE)

  #taxon level names
  entity_levels.id <- rhdf5::H5Dopen(hdf, 'entity_levels')
  expect_equal(trimws(paste0(entity_levels.id[1,], collapse = '')), "binomial")
  expect_equal(trimws(paste0(entity_levels.id[2,], collapse = '')), "family")
  expect_equal(trimws(paste0(entity_levels.id[3,], collapse = '')), "order")
  rhdf5::H5Dclose(entity_levels.id)

  rhdf5::H5Fclose(hdf)

  #test ebv_properties taxonomy
  taxonomy_table <- ebv_properties(file, verbose = FALSE)@general$taxonomy
  expect_equal(names(taxonomy_table), c("binomial", "family", "order"))
  expect_equal(unlist(taxonomy_table[1,1]), "Hipposideros curtus")
  expect_equal(unlist(taxonomy_table[1,2]), "Hipposideridae")
  expect_equal(unlist(taxonomy_table[1,3]), "Chiroptera")
  taxonomy_lsid <- ebv_properties(file, verbose = FALSE)@general$taxonomy_lsid
  expect_equal(taxonomy_lsid, NA)

  #remove file
  file.remove(file)


})


test_that("test ebv_create_taxonomy with lsid", {
  #test ebv_create_taxonomy with lsid ----

  #basic paths
  root <- system.file(file.path("extdata/testdata"), package="ebvcube")
  json <- file.path(root, '5_single_date.json')
  taxonomy <- file.path(root, 'id5_taxid.csv')
  file <- tempfile(fileext='.nc')

  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(0.25, 0.25)
  fillvalue <- -127
  prec <- 'float'
  epsg <- 4326
  sep <- ','

  #create empty file
  expect_silent(ebv_create_taxonomy(jsonpath = json,
                                    outputpath = file,
                                    taxonomy = taxonomy,
                                    lsid=TRUE,
                                    epsg = epsg,
                                    extent = extent,
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
  expect_equal(trimws(paste0(entity_list.id[1,2,], collapse = '')), "Microcebus rufus")
  expect_equal(trimws(paste0(entity_list.id[2,2,], collapse = '')), "Cheirogaleidae")
  expect_equal(trimws(paste0(entity_list.id[3,2,], collapse = '')), "Primates")
  rhdf5::H5Dclose(entity_list.id)

  #check lsid created
  expect_equal(rhdf5::H5Lexists(hdf, 'entity_lsid'), TRUE)
  entity_lsid.id <- rhdf5::H5Dopen(hdf, 'entity_lsid')
  expect_equal(trimws(paste0(entity_lsid.id[1,], collapse = '')), "10125")
  rhdf5::H5Dclose(entity_lsid.id)

  #taxon level names
  entity_levels.id <- rhdf5::H5Dopen(hdf, 'entity_levels')
  expect_equal(trimws(paste0(entity_levels.id[1,], collapse = '')), "binomial")
  expect_equal(trimws(paste0(entity_levels.id[2,], collapse = '')), "family")
  expect_equal(trimws(paste0(entity_levels.id[3,], collapse = '')), "order")
  rhdf5::H5Dclose(entity_levels.id)

  rhdf5::H5Fclose(hdf)

  #test ebv_properties taxonomy
  taxonomy_table <- ebv_properties(file, verbose = FALSE)@general$taxonomy
  expect_equal(names(taxonomy_table), c("binomial", "family", "order"))
  expect_equal(unlist(taxonomy_table[1,1]), "Hipposideros curtus")
  expect_equal(unlist(taxonomy_table[1,2]), "Hipposideridae")
  expect_equal(unlist(taxonomy_table[1,3]), "Chiroptera")
  taxonomy_lsid <- ebv_properties(file, verbose = FALSE)@general$taxonomy_lsid
  expect_equal(taxonomy_lsid[1:3], c('10125', '13324', '18296'))

  #remove file
  file.remove(file)


})
