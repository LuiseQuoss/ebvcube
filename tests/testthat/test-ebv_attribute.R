test_that("test attribute: keyword modification", {
  file <- system.file(file.path("extdata/testdata","test_att.nc"), package="ebvcube")
  domain_old <- ebv_properties(file)@general$ebv_domain
  if(domain_old=='Marine'){
    ebv_attribute(file, 'ebv_domain', 'Terrestrial')
    prop <- ebv_properties(file)@general
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic and phylogenetic diversity, ebv_domain: Terrestrial, ebv_spatial_scope: Global, ebv_entity_type: Communities")
  }else{
    ebv_attribute(file, 'ebv_domain', 'Marine')
    prop <- ebv_properties(file)@general
    expect_equal(prop$keywords, "ebv_class: Community composition, ebv_name: Taxonomic and phylogenetic diversity, ebv_domain: Marine, ebv_spatial_scope: Global, ebv_entity_type: Communities")
  }
})
