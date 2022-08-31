test_that("test attribute: keyword modification", {
  file <- system.file(file.path("extdata/testdata","test_att.nc"), package="ebvcube")
  domain_old <- ebv_properties(file)@general$ebv_domain
  if(domain_old=='Marine'){
    ebv_attribute(file, 'ebv_domain', 'Terrestrial')
    prop <- ebv_properties(file)@general
    expect_equal(prop$ebv_domain, 'Terrestrial')
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic and phylogenetic diversity, ebv_domain: Terrestrial, ebv_spatial_scope: Global, ebv_entity_type: Communities")
  }else{
    ebv_attribute(file, 'ebv_domain', 'Marine')
    prop <- ebv_properties(file)@general
    expect_equal(prop$ebv_domain, 'Marine')
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic and phylogenetic diversity, ebv_domain: Marine, ebv_spatial_scope: Global, ebv_entity_type: Communities")
  }
})

test_that("test attribute: modification of metric attribute", {
  file <- system.file(file.path("extdata/testdata","test_att.nc"), package="ebvcube")
  sn_old <- ebv_properties(file, 'metric_1/ebv_cube')@metric$name
  if(sn_old=="Relative change in the number of species (%)"){
    ebv_attribute(file, 'standard_name', "Relativ change in the number of species (%)", 'metric_1')
    prop <- ebv_properties(file,'metric_1/ebv_cube')
    expect_equal(prop@metric$name, "Relativ change in the number of species (%)")
    hdf <- rhdf5::H5Fopen(file)
    did <- rhdf5::H5Dopen(hdf, 'metric_1/ebv_cube')
    ebv_cube_name <- ebv_i_read_att(did, 'long_name')
    rhdf5::H5Dclose(did)
    rhdf5::H5Fclose(hdf)
    expect_equal(ebv_cube_name, "Relativ change in the number of species (%)")
  }else{
    ebv_attribute(file, 'standard_name', "Relative change in the number of species (%)", 'metric_1')
    prop <- ebv_properties(file, 'metric_1/ebv_cube')
    expect_equal(prop@metric$name, "Relative change in the number of species (%)")
    hdf <- rhdf5::H5Fopen(file)
    did <- rhdf5::H5Dopen(hdf, 'metric_1/ebv_cube')
    ebv_cube_name <- ebv_i_read_att(did, 'long_name')
    rhdf5::H5Dclose(did)
    rhdf5::H5Fclose(hdf)
    expect_equal(ebv_cube_name, "Relative change in the number of species (%)")
    }
})

test_that("test attribute: modification of scenario attribute", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  sn_old <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario$name
  if(sn_old=='SSP3-RCP6.0 LU'){
    ebv_attribute(file, 'standard_name', 'SSP3-RCP6.0 LULC', 'scenario_2')
    prop <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario
    expect_equal(prop$name, 'SSP3-RCP6.0 LULC')
  }else{
    ebv_attribute(file, 'standard_name', 'SSP3-RCP6.0 LU', 'scenario_2')
    prop <- ebv_properties(file, 'scenario_2/metric_1/ebv_cube')@scenario
    expect_equal(prop$name, 'SSP3-RCP6.0 LU')  }
})

test_that("test attribute: modification of global attribute", {
  file <- system.file(file.path("extdata/testdata","test_att.nc"), package="ebvcube")
  title_old <- ebv_properties(file)@general$title
  if(title_old=="Local bird diversity (cSAR/BES-SIM)"){
    ebv_attribute(file, 'title', "Local birds diversity (cSAR/BES-SIM)")
    prop <- ebv_properties(file)@general
    expect_equal(prop$title, "Local birds diversity (cSAR/BES-SIM)")
  }else{
    ebv_attribute(file, 'title', "Local bird diversity (cSAR/BES-SIM)")
    prop <- ebv_properties(file)@general
    expect_equal(prop$title, "Local bird diversity (cSAR/BES-SIM)")  }
})

test_that("test attribute: modification of ebv_cube attribute", {
  file <- system.file(file.path("extdata/testdata","test_att.nc"), package="ebvcube")
  cct_old <- ebv_properties(file, 'metric_1/ebv_cube')@ebv_cube$coverage_content_type
  if(cct_old=="modelResult"){
    ebv_attribute(file, 'coverage_content_type', "modelsResult", 'metric_1/ebv_cube')
    prop <- ebv_properties(file, 'metric_1/ebv_cube')@ebv_cube
    expect_equal(prop$coverage_content_type, "modelsResult")
  }else{
    ebv_attribute(file, 'coverage_content_type', "modelResult", 'metric_1/ebv_cube')
    prop <- ebv_properties(file, 'metric_1/ebv_cube')@ebv_cube
    expect_equal(prop$coverage_content_type, "modelResult")  }
})
