test_that("ebv_properties: check S4", {
  file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
  expect_type(ebv_properties(file), 'S4')
})

test_that("ebv_properties: check spatial", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file)@spatial
  expect_equal(prop$wkt2, "GEOGCRS[\"WGS 84\", DATUM[\"World Geodetic System 1984\", ELLIPSOID[\"WGS 84\",6378137,298.257223563, LENGTHUNIT[\"metre\",1]]], PRIMEM[\"Greenwich\",0, ANGLEUNIT[\"degree\",0.0174532925199433]], CS[ellipsoidal,2], AXIS[\"geodetic latitude (Lat)\",north, ORDER[1], ANGLEUNIT[\"degree\",0.0174532925199433]], AXIS[\"geodetic longitude (Lon)\",east, ORDER[2], ANGLEUNIT[\"degree\",0.0174532925199433]], USAGE[ SCOPE[\"Horizontal component of 3D system.\"], AREA[\"World.\"], BBOX[-90,-180,90,180]], ID[\"EPSG\",4326]]")
  expect_equal(prop$epsg,"4326")
  expect_equal(prop$extent,c(-180, 180, -90, 90))
  expect_equal(prop$resolution, c(1,1))
  expect_equal(prop$crs_units,'degree')
  expect_equal(prop$dimensions,c(180, 360, 3, 3))
  expect_equal(prop$scope,'Global')
  expect_equal(prop$description,'N/A')
})

test_that("ebv_properties: check general", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file)@general
  expect_equal(prop$title,"BES-SIM cSAR-iDiv")
  expect_equal(prop$description,'Projections from the cSAR-iDiv model from 1900-2050 using LUH2 and SSPs-RCPs, done in the BES-SIM inter-model comparison for IPBES.')
  expect_equal(prop$ebv_class,'Community composition')
  expect_equal(prop$ebv_name,'Taxonomic/phylogenetic diversity')
  expect_equal(prop$ebv_domain,'Terrestrial')
  expect_equal(prop$references,'N/A')
  expect_equal(prop$source,'cSAR-iDiv model team, 14 December 2018')
  expect_equal(prop$project_name,'BES-SIM')
  expect_equal(prop$project_url,'https://ipbes.net/scenarios-models/expert-group-activities')
  expect_equal(prop$creator_name,'Inês Martins')
  expect_equal(prop$creator_institution,'iDiv - German Centre for Integrative Biodiversity Research / Martin-Luther University of Halle-Wittenberg')
  expect_equal(prop$creator_email,'istmartins@gmail.com')
  expect_equal(prop$contributor_name,'Henrique M. Pereira')
  expect_equal(prop$publisher_name,'Henrique M. Pereira')
  expect_equal(prop$publisher_institution,'iDiv')
  expect_equal(prop$publisher_email,'hpereira@idiv.de')
  expect_equal(prop$comment,'Delta_S (original name: CC-DC) for the two time periods and S for 2015 (original name: Sp) were provided by cSAR team. Delta_SS and values of S for 1900 and 2050 calculated by H.M. Pereira on 20.3.2022.')
  expect_equal(prop$keywords,'ebv_class: Community composition, ebv_name: Taxonomic/phylogenetic diversity, ebv_domain: Terrestrial, ebv_spatial_scope: , ebv_entity_type: Communities, ebv_scenario_classification_name: SSP-RCP')
  expect_equal(prop$id,'pending')
  expect_equal(prop$history,'EBV netCDF created using ebvcube, 2022-08-31')
  expect_equal(prop$licence,'CC-BY')
  expect_equal(prop$conventions,'CF-1.8, ACDD-1.3, EBV-1.0')
  expect_equal(prop$naming_authority,'The German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig')
  expect_equal(prop$date_created,'2022-02-16')
  expect_equal(prop$date_issued,'pending')
  expect_equal(prop$entity_names,c("All birds", "Forest birds", "Non-forest birds"))
  expect_equal(prop$entity_type,'Communities')
  expect_equal(prop$entity_scope,'Bird species (forest and non-forest)')
  expect_equal(prop$entity_classification_name,'N/A')
  expect_equal(prop$entity_classification_url,'N/A')

})

test_that("ebv_properties: check temporal", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file)@temporal
  expect_equal(prop$resolution,'N/A')
  expect_equal(prop$units,'days since 1860-01-01 00:00:00.0')
  expect_equal(prop$timesteps, array(c(14610,56613, 69397)))
  expect_equal(prop$dates[1], as.Date("1900-01-01"))
  expect_equal(prop$dates[2], as.Date("2015-01-01"))
  expect_equal(prop$dates[3], as.Date("2050-01-01"))
  expect_equal(dim(prop$dates), 3)
  })

test_that("ebv_properties: check metric", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@metric
  expect_equal(prop$name,'Species richness (S)')
  expect_equal(prop$description,'Species richness per cell')
})


test_that("ebv_properties: check scenario", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@scenario
  expect_equal(prop$name,'SSP1-RCP1.5 LU')
  expect_equal(prop$description,'Global Sustainability (SSP1-RCP1.5), with only effects of land-use.')
  expect_equal(prop$scenario_classification_name,'SSP-RCP')
  expect_equal(prop$scenario_classification_url,'https://en.wikipedia.org/wiki/Shared_Socioeconomic_Pathways')
  expect_equal(prop$scenario_classification_version,'LUH2')
})

test_that("ebv_properties: check ebv_cube", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file, 'scenario_1/metric_1/ebv_cube')@ebv_cube
  expect_equal(prop$units,'Number of species')
  expect_equal(prop$coverage_content_type,'modelResult')
  expect_equal(prop$fillvalue,array(-3.40282e+38))
  expect_equal(prop$type,'H5T_IEEE_F32LE')
})

test_that("ebv_properties: check def of datacubepath by scenario and metric", {
  file <- system.file(file.path("extdata/testdata","pereira_csar_bes_sim_20220830_4d.nc"), package="ebvcube")
  prop <- ebv_properties(file, scenario = 2, metric=3, verbose=FALSE)
  expect_equal(prop@scenario$name,"SSP3-RCP6.0 LULC")
  expect_equal(prop@metric$name,"Diversity-weighted relative species richness change (Delta_SS)")
})

