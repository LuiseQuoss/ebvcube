test_that("test ebv_metadata scenario&metric only mandatory", {
  #create temp json output file
  out <- tempfile(fileext='.json')

  #run ebv_metadata
  warnings <- capture_warnings(ebv_metadata(outputpath=out,
               overwrite = TRUE,
               title = 'Not a real title',
               summary = 'Summary summary summary',
               source = 'this was created by doing...',
               date_created = as.Date('2024-07-10'),
               creator_name = 'Name Name',
               creator_institution = 'lame name',
               license = 'CC0',
               ebv_class = 'Genetic composition',
               ebv_name = 'Intraspecific genetic diversity',
               ebv_spatial_scope = 'National',
               ebv_spatial_description = 'Finland',
               ebv_domain = 'Terrestrial',
               ebv_entity_type = 'Species',
               ebv_entity_scope = '50 mammal species',
               coverage_content_type = c('modelResult'),
               time_coverage_start = as.Date('1900-01-01'),time_coverage_end =as.Date('1950-01-01'), time_coverage_resolution = 'P0010-00-00',
               metric = list(list(standard_name='relative change of habitat', long_name='relative change to year 1800', units='percentage'),
                             list(standard_name='absolute change habitat', long_name='absolute change since year 1800', units='square kilometers')),
               scenario = list(list(standard_name='SSP1', long_name='description of SSP1'),
                               list(standard_name='SSP2', long_name='description of SSP2')),
               verbose = TRUE
               ))
  expect_equal(length(warnings), 3)

  #test if json works for creation
  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(1, 1)
  fillvalue <- 0
  prec <- 'byte'
  epsg <- 4326
  sep <- ','
  timesteps <- c('1900', '2015', '2050')
  out2 <- tempfile(fileext='.nc')

  #create empty file
  expect_silent(ebv_create(jsonpath = out,
                           outputpath = out2,
                           entities = 'All taxa',
                           epsg = epsg,
                           extent = extent,
                           timesteps = timesteps,
                           fillvalue = fillvalue,
                           prec = prec,
                           sep = sep,
                           force_4D = TRUE,
                           overwrite = TRUE,
                           verbose = FALSE))

  #remove files
  file.remove(out)
  file.remove(out2)


})

test_that("test ebv_metadata metric only mandatory", {
  #create temp json output file
  out <- tempfile(fileext='.json')

  #run ebv_metadata
  ebv_metadata(outputpath=out,
               overwrite = TRUE,
               title = 'Not a real title',
               summary = 'Summary summary summary',
               references = 'https://doi.org/345dy',
               source = 'this was created by doing...',
               date_created = as.Date('2024-07-10'),
               creator_name = 'Name Name',
               creator_email = 'this@that.com',
               creator_institution = 'lame name',
               contributor_name = c('me', 'you', 'her'),
               license = 'CC BY',
               ebv_entity_type = 'Species',
               ebv_entity_scope = '50 mammal species',
               comment = 'anything else you have to say',
               ebv_class = 'Species populations',
               ebv_name = 'Species distributions',
               ebv_spatial_scope = 'Global',
               ebv_domain = c('Terrestrial','Marine'),
               coverage_content_type = c('modelResult'),
               time_coverage_start = as.Date('1900-01-01'), time_coverage_end =as.Date('1950-01-01'), time_coverage_resolution = 'Irregular',
               metric = list(standard_name='relative change of habitat', long_name='relative change to year 1800', units='percentage')
  )

  #test if json works for creation
  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(1, 1)
  fillvalue <- 0
  prec <- 'byte'
  epsg <- 4326
  sep <- ','
  timesteps <- c('1900', '2015', '2050')
  out2 <- tempfile(fileext='.nc')

  #create empty file
  expect_silent(ebv_create(jsonpath = out,
                           outputpath = out2,
                           entities = c('birds', 'fishes'),
                           epsg = epsg,
                           extent = extent,
                           timesteps = timesteps,
                           fillvalue = fillvalue,
                           prec = prec,
                           sep = sep,
                           force_4D = TRUE,
                           overwrite = TRUE,
                           verbose = FALSE))

  #remove files
  file.remove(out)
  file.remove(out2)


})

test_that("test ebv_metadata scenario&metric with optional args", {
  #create temp json output file
  out <- tempfile(fileext='.json')

  #run ebv_metadata
  warnings <- capture_warnings(ebv_metadata(outputpath=out,
               overwrite = TRUE,
               title = 'Not a real title',
               summary = 'Summary summary summary',
               references = c('https://doi.org/345dy', 'https://doi.org/sdf739'),
               source = 'this was created by doing...',
               project_name = 'bets project',
               project_url = 'www.best-proj.de',
               date_created = as.Date('2024-07-10'),
               creator_name = 'Name Name',
               creator_email = 'this@that.com',
               creator_institution = 'lame name',
               license = 'CC0',
               comment = 'anything else you have to say',
               ebv_class = 'Ecosystem services',
               ebv_name = 'other',
               ebv_spatial_scope = 'Global',
               ebv_spatial_description = 'this is ignored and thows warning',
               ebv_domain = 'Terrestrial',
               ebv_entity_type = 'Species',
               ebv_entity_scope = '50 mammal species',
               coverage_content_type = c('modelResult'),
               time_coverage_start = as.Date('1900-01-01'),time_coverage_end =as.Date('1950-01-01'), time_coverage_resolution = 'P0010-00-00',
               metric = list(list(standard_name='relative change of habitat', long_name='relative change to year 1800', units='percentage'),
                             list(standard_name='absolute change habitat', long_name='absolute change since year 1800', units='square kilometers')),
               # scenario = list(standard_name='SSP1', long_name='description of SSP1')
               scenario = list(list(standard_name='SSP1', long_name='description of SSP1'),
                               list(standard_name='SSP2', long_name='description of SSP2')),
               verbose = TRUE,
               contributor_name = c('he', 'she', 'it'),
               ebv_scenario_classification_name = 'Classification tiptop',
               ebv_scenario_classification_version = 'V1.1.0',
               ebv_scenario_classification_url = 'which-this-existed.com',
               ebv_entity_classification_name = 'Classified by educated guess',
               ebv_entity_classification_url = 'click-here.de'

  ))
  expect_equal(length(warnings), 1)
  expect_true(stringr::str_detect(warnings, 'spatial description'))


  #test if json works for creation
  #spatial info
  extent <- c(-180, 180, -90, 90)
  res <- c(1, 1)
  fillvalue <- 0
  prec <- 'byte'
  epsg <- 4326
  sep <- ','
  timesteps <- c('1900', '2015', '2050')
  out2 <- tempfile(fileext='.nc')

  #create empty file
  expect_silent(ebv_create(jsonpath = out,
                           outputpath = out2,
                           entities = 'All taxa',
                           epsg = epsg,
                           extent = extent,
                           timesteps = timesteps,
                           fillvalue = fillvalue,
                           prec = prec,
                           sep = sep,
                           force_4D = TRUE,
                           overwrite = TRUE,
                           verbose = FALSE))

  #remove files
  file.remove(out)
  file.remove(out2)


})

