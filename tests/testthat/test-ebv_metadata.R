test_that("test ebv_metadata scenario&metric", {
  #create temp json output file
  out <- tempfile(fileext='.json')

  #run ebv_metadata
  ebv_metadata(outputpath=out,
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
               contributor_name = c('me', 'you', 'her'),
               publisher_name = 'mainainer name',
               publisher_email = 'maintainer@this.com',
               publisher_institution = 'm institution',
               license = 'https://creativecommons.org/licenses/by/4.0',
               comment = 'anything else you have to say',
               ebv_name = 'None',
               ebv_class = 'None',
               ebv_spatial_scope = 'Continental',
               ebv_spatial_description = 'Finland', ebv_domain = c('Terrestrial','test'),
               coverage_content_type = c('modelResult'),
               time_coverage_start = as.Date('1900-01-01'),time_coverage_end =as.Date('1950-01-01'), time_coverage_resolution = 'P0010-00-00',
               metric = list(list(standard_name='relative change of habitat', long_name='relative change to year 1800', units='percentage'),
                             list(standard_name='absolute change habitat', long_name='absolute change since year 1800', units='square kilometers')),
               scenario = list(list(standard_name='SSP1', long_name='description of SSP1'),
                               list(standard_name='SSP2', long_name='description of SSP2'))
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

