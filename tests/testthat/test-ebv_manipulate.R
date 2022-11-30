test_that("test ebv_manipulate", {
  #test ebv_create ----
  #basic paths
  root <- system.file(file.path("extdata/testdata"), package="ebvcube")
  json <- file.path(root, 'cSAR-iDiv.json')
  csv <- file.path(root, 'entities.csv')
  file <- tempfile(fileext='.nc')

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
             outputpath = file,
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

  #test ebv_attribute: keyword modification  ----
  domain_old <- ebv_properties(file)@general$ebv_domain
  if(domain_old=='Marine'){
    ebv_attribute(file, 'ebv_domain', 'Terrestrial')
    prop <- ebv_properties(file)@general
    expect_equal(prop$ebv_domain, 'Terrestrial')
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic/phylogenetic diversity, ebv_domain: Terrestrial, ebv_spatial_scope: Global, ebv_entity_type: Communities, ebv_scenario_classification_name: SSP-RCP")
  }else{
    ebv_attribute(file, 'ebv_domain', 'Marine')
    prop <- ebv_properties(file)@general
    expect_equal(prop$ebv_domain, 'Marine')
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic/phylogenetic diversity, ebv_domain: Marine, ebv_spatial_scope: Global, ebv_entity_type: Communities, ebv_scenario_classification_name: SSP-RCP")
  }

  #test ebv_attribute: modification of metric attribute  ----
  sn_old <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube', FALSE)@metric$name
  if(sn_old=="Species richness (S)"){
    ebv_attribute(file, 'standard_name', "Species richness (ST)", 'scenario_1/metric_1', verbose=FALSE)
    prop <- ebv_properties(file,'scenario_1/metric_1/ebv_cube', FALSE)
    expect_equal(prop@metric$name, "Species richness (ST)")
    hdf <- rhdf5::H5Fopen(file)
    did <- rhdf5::H5Dopen(hdf, 'scenario_2/metric_1/ebv_cube')
    ebv_cube_name <- ebv_i_read_att(did, 'long_name', FALSE)
    rhdf5::H5Dclose(did)
    rhdf5::H5Fclose(hdf)
    expect_equal(ebv_cube_name, "Species richness (ST)")
  }else{
    ebv_attribute(file, 'standard_name', "Species richness (S)", 'scenario_1/metric_1', verbose=FALSE)
    prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube', FALSE)
    expect_equal(prop@metric$name, "Species richness (S)")
    hdf <- rhdf5::H5Fopen(file)
    did <- rhdf5::H5Dopen(hdf, 'scenario_2/metric_1/ebv_cube')
    ebv_cube_name <- ebv_i_read_att(did, 'long_name', FALSE)
    rhdf5::H5Dclose(did)
    rhdf5::H5Fclose(hdf)
    expect_equal(ebv_cube_name, "Species richness (S)")
  }

  #test ebv_attribute: modification of scenario attribute  ----
  sn_old <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario$name
  if(sn_old=='SSP3-RCP6.0 LU'){
    ebv_attribute(file, 'standard_name', 'SSP3-RCP6.0 LULC', 'scenario_2')
    prop <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario
    expect_equal(prop$name, 'SSP3-RCP6.0 LULC')
  }else{
    ebv_attribute(file, 'standard_name', 'SSP3-RCP6.0 LU', 'scenario_2')
    prop <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario
    expect_equal(prop$name, 'SSP3-RCP6.0 LU')  }

  #test ebv_attribute: modification of global attribute  ----
  title_old <- ebv_properties(file)@general$title
  if(title_old=="Local bird diversity (cSAR/BES-SIM)"){
    ebv_attribute(file, 'title', "Local birds diversity (cSAR/BES-SIM)")
    prop <- ebv_properties(file)@general
    expect_equal(prop$title, "Local birds diversity (cSAR/BES-SIM)")
  }else{
    ebv_attribute(file, 'title', "Local bird diversity (cSAR/BES-SIM)")
    prop <- ebv_properties(file)@general
    expect_equal(prop$title, "Local bird diversity (cSAR/BES-SIM)")
  }

  #test ebv_attribute: modification of ebv_cube attribute  ----
  cct_old <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@ebv_cube$coverage_content_type
  if(cct_old=="modelResult"){
    ebv_attribute(file, 'coverage_content_type', "modelsResult", 'scenario_1/metric_1/ebv_cube', FALSE)
    prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@ebv_cube
    expect_equal(prop$coverage_content_type, "modelsResult")
  }else{
    ebv_attribute(file, 'coverage_content_type', "modelResult", 'scenario_1/metric_1/ebv_cube', FALSE)
    prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@ebv_cube
    expect_equal(prop$coverage_content_type, "modelResult")
    }

  #remove file
  file.remove(file)

})
