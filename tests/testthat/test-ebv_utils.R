#CRS related tests----

#test ebv_i_get_epsg----
test_that("test ebv_i_get_epsg 4326", {
  crs_epsg <- ebv_i_get_epsg('GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]')
  expect_equal(crs_epsg, 4326)
})

test_that("test ebv_i_get_epsg ESRI:54009", {
  crs_epsg <- ebv_i_get_epsg('PROJCRS[\"World_Mollweide\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"World_Mollweide\",\n        METHOD[\"Mollweide\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Not known.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"ESRI\",54009]]')
  expect_equal(crs_epsg, 'ESRI:54009')
})

#test ebv_i_eval_epsg----
test_that("test ebv_i_eval_epsg 4326", {
  crs_wkt <- ebv_i_eval_epsg(4326)
  expect_equal(ebv_i_get_epsg(crs_wkt),4326)
})

test_that("test ebv_i_eval_epsg ESRI:54009", {
  crs_wkt <- ebv_i_eval_epsg('ESRI:54009')
  expect_equal(ebv_i_get_epsg(crs_wkt),'ESRI:54009')
})

test_that("test ebv_i_eval_epsg 4326 return proj", {
  crs_proj <- ebv_i_eval_epsg(4326, T)
  expect_equal(crs_proj, '+proj=longlat +datum=WGS84 +no_defs')
})

test_that("test ebv_i_eval_epsg ESRI:54009 return proj", {
  crs_proj <- ebv_i_eval_epsg('ESRI:54009', T)
  expect_equal(crs_proj, '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
})

#test eval_wkt----
test_that("test ebv_i_eval_wkt for ESRI:54009 - old WKT", {
  wkt <- 'PROJCS["World_Mollweide",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mollweide"],PARAMETER["central_meridian",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["ESRI","54009"]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, F)
})

test_that("test ebv_i_eval_wkt for ESRI:54009 - new WKT", {
  wkt <- 'PROJCRS["World_Mollweide",BASEGEOGCRS["WGS 84",DATUM["World Geodetic System 1984",ELLIPSOID["WGS 84",6378137,298.257223563,LENGTHUNIT["metre",1]]],PRIMEM["Greenwich",0,ANGLEUNIT["Degree",0.0174532925199433]]],CONVERSION["World_Mollweide",METHOD["Mollweide"],PARAMETER["Longitude of natural origin",0,ANGLEUNIT["Degree",0.0174532925199433],ID["EPSG",8802]],PARAMETER["False easting",0,LENGTHUNIT["metre",1],ID["EPSG",8806]],PARAMETER["False northing",0,LENGTHUNIT["metre",1],ID["EPSG",8807]]],CS[Cartesian,2],AXIS["(E)",east,ORDER[1],LENGTHUNIT["metre",1]],AXIS["(N)",north,ORDER[2],LENGTHUNIT["metre",1]],USAGE[SCOPE["Not known."],AREA["World."],BBOX[-90,-180,90,180]],ID["ESRI",54009]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, T)
})

test_that("test ebv_i_eval_wkt for EPSG:22032 - old wkt", {
  wkt <- 'PROJCS["Camacupa 1948 / UTM zone 32S",GEOGCS["Camacupa 1948",DATUM["Camacupa_1948",SPHEROID["Clarke 1880 (RGS)",6378249.145,293.465],TOWGS84[-50.9,-347.6,-231,0,0,0,0]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4220"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",9],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",10000000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","22032"]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, F)
})

test_that("test ebv_i_eval_wkt for EPSG:22032 - new WKT", {
  wkt <- 'PROJCRS["Camacupa 1948 / UTM zone 32S",BASEGEOGCRS["Camacupa 1948",DATUM["Camacupa 1948",ELLIPSOID["Clarke 1880 (RGS)",6378249.145,293.465,LENGTHUNIT["metre",1]]],PRIMEM["Greenwich",0,ANGLEUNIT["degree",0.0174532925199433]],ID["EPSG",4220]],CONVERSION["UTM zone 32S",METHOD["Transverse Mercator",ID["EPSG",9807]],PARAMETER["Latitude of natural origin",0,ANGLEUNIT["degree",0.0174532925199433],ID["EPSG",8801]],PARAMETER["Longitude of natural origin",9,ANGLEUNIT["degree",0.0174532925199433],ID["EPSG",8802]],PARAMETER["Scale factor at natural origin",0.9996,SCALEUNIT["unity",1],ID["EPSG",8805]],PARAMETER["False easting",500000,LENGTHUNIT["metre",1],ID["EPSG",8806]],PARAMETER["False northing",10000000,LENGTHUNIT["metre",1],ID["EPSG",8807]]],CS[Cartesian,2],AXIS["(E)",east,ORDER[1],LENGTHUNIT["metre",1]],AXIS["(N)",north,ORDER[2],LENGTHUNIT["metre",1]],USAGE[SCOPE["Engineering survey, topographic mapping."],AREA["Angola - Angola proper - offshore - west of 12°E."],BBOX[-17.26,8.2,-6.03,12]],ID["EPSG",22032]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, T)
})

test_that("test ebv_i_eval_wkt for EPSG:4326 - old wkt", {
  wkt <- 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, F)
})

test_that("test ebv_i_eval_wkt for EPSG:4326 - new WKT", {
  wkt <- 'GEOGCRS["WGS 84",ENSEMBLE["World Geodetic System 1984 ensemble",MEMBER["World Geodetic System 1984 (Transit)"],MEMBER["World Geodetic System 1984 (G730)"],MEMBER["World Geodetic System 1984 (G873)"],MEMBER["World Geodetic System 1984 (G1150)"],MEMBER["World Geodetic System 1984 (G1674)"],MEMBER["World Geodetic System 1984 (G1762)"],MEMBER["World Geodetic System 1984 (G2139)"],ELLIPSOID["WGS 84",6378137,298.257223563,LENGTHUNIT["metre",1]],ENSEMBLEACCURACY[2.0]],PRIMEM["Greenwich",0,ANGLEUNIT["degree",0.0174532925199433]],CS[ellipsoidal,2],AXIS["geodetic latitude (Lat)",north,ORDER[1],ANGLEUNIT["degree",0.0174532925199433]],AXIS["geodetic longitude (Lon)",east,ORDER[2],ANGLEUNIT["degree",0.0174532925199433]],USAGE[SCOPE["Horizontal component of 3D system."],AREA["World."],BBOX[-90,-180,90,180]],ID["EPSG",4326]]'
  result <- ebv_i_eval_wkt(wkt)
  expect_equal(result, T)
})

#ENTITY related test----
test_that("test ebv_i_entity with integer", {
  expect_null(ebv_i_entity(1, c("All birds", "Forest birds", "Non-forest birds")))
})

test_that("test ebv_i_entity with string", {
  entity <- ebv_i_entity("Forest birds", c("All birds", "Forest birds", "Non-forest birds"))
  expect_equal(entity, 2)
})

test_that("test ebv_i_entity wrong string", {
  expect_error(ebv_i_entity("Forest birddds", c("All birds", "Forest birds", "Non-forest birds")), 'Given entity name is not valid.')
})

test_that("test ebv_i_entity wrong integer", {
  expect_error(ebv_i_entity(4, c("All birds", "Forest birds", "Non-forest birds")), 'bigger than available entities')
})

test_that("test ebv_i_entity negative integer", {
  expect_error(ebv_i_entity(-1, c("All birds", "Forest birds", "Non-forest birds")), 'negative value')
})

#transform bb test----

test_that("test ebv_i_transform_bb", {
  new_bb <- ebv_i_transform_bb(c(-50,50,20,40), 4326, 3857)
  expect_equal(new_bb, c(-5565974.540,  5565974.540,  2273030.927,  4865942.280))
})

#date detection test ----

test_that("test ebv_i_date integer success", {
  dates_all <- 1:12
  dates_indice <- ebv_i_date(1:3, dates_all)
  expect_equal(dates_indice, 1:3)
})

test_that("test ebv_i_date single integer success", {
  dates_all <- 1:13
  dates_indice <- ebv_i_date(13, dates_all)
  expect_equal(dates_indice, 13)
})

test_that("test ebv_i_date integer error", {
  dates_all <- 1:12
  expect_error(ebv_i_date(12:14, dates_all))
})

test_that("test ebv_i_date single integer error", {
  dates_all <- 1:12
  expect_error(ebv_i_date(14, dates_all))
})

test_that("test ebv_i_date iso-strings success", {
  dates_all <- paste0(as.character(seq(1900,2010,10)), '-01-01')
  dates_indice <- ebv_i_date(c("1940-01-01", "1950-01-01", "1960-01-01"), dates_all)
  expect_equal(dates_indice, 5:7)
})

test_that("test ebv_i_date single iso-string success", {
  dates_all <- paste0(as.character(seq(1900,2010,10)), '-01-01')
  dates_indice <- ebv_i_date(c("1940-01-01"), dates_all)
  expect_equal(dates_indice, 5)
})

test_that("test ebv_i_date iso-strings error", {
  dates_all <- paste0(as.character(seq(1900,1940,10)), '-01-01')
  expect_error(ebv_i_date(c("1940-01-01", "1950-01-01", "1960-01-01"), dates_all))
})

test_that("test ebv_i_date single iso-string error", {
  dates_all <- paste0(as.character(seq(1900,1940,10)), '-01-01')
  expect_error(ebv_i_date("1960-01-01", dates_all))
})

# test scenario and metric definition ----
#ebv_i_datacubepath(scenario, metric, datacubepaths, verbose)

test_that("test ebv_i_datacubepath 2 string values success", {
  datacubepaths <- data.frame(c('scenario_1/metric_1/ebv_cube', 'scenario_1/metric_2/ebv_cube',
                         'scenario_1/metric_3/ebv_cube',
                         'scenario_2/metric_1/ebv_cube', 'scenario_2/metric_2/ebv_cube',
                         'scenario_2/metric_3/ebv_cube',
                         'scenario_3/metric_1/ebv_cube', 'scenario_3/metric_2/ebv_cube',
                         'scenario_3/metric_3/ebv_cube'))
  datacubepaths <- cbind(datacubepaths, c('SSP1-RCP1.5 LU','SSP1-RCP1.5 LU','SSP1-RCP1.5 LU',
                            'SSP3-RCP6.0 LU','SSP3-RCP6.0 LU','SSP3-RCP6.0 LU',
                            'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU'))
  datacubepaths <- cbind(datacubepaths, c('Species richness (S)','Relative species richness change (Delta_S)',
                            'Diversity-weighted relative species richness change (Delta_SS)'))
  colnames(datacubepaths) <- c('datacubepaths', 'scenario_names', 'metric_names')

  result <- ebv_i_datacubepath('SSP3-RCP6.0 LU', 'Species richness (S)', datacubepaths, FALSE)
  expect_equal(result, "scenario_2/metric_1/ebv_cube")
})

test_that("test ebv_i_datacubepath string values error", {
  datacubepaths <- data.frame(c('scenario_1/metric_1/ebv_cube', 'scenario_1/metric_2/ebv_cube',
                                'scenario_1/metric_3/ebv_cube',
                                'scenario_2/metric_1/ebv_cube', 'scenario_2/metric_2/ebv_cube',
                                'scenario_2/metric_3/ebv_cube',
                                'scenario_3/metric_1/ebv_cube', 'scenario_3/metric_2/ebv_cube',
                                'scenario_3/metric_3/ebv_cube'))
  datacubepaths <- cbind(datacubepaths, c('SSP1-RCP1.5 LU','SSP1-RCP1.5 LU','SSP1-RCP1.5 LU',
                                          'SSP3-RCP6.0 LU','SSP3-RCP6.0 LU','SSP3-RCP6.0 LU',
                                          'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU'))
  datacubepaths <- cbind(datacubepaths, c('Species richness (S)','Relative species richness change (Delta_S)',
                                          'Diversity-weighted relative species richness change (Delta_SS)'))
  colnames(datacubepaths) <- c('datacubepaths', 'scenario_names', 'metric_names')

  expect_error(ebv_i_datacubepath('SSP3-RCP9.0 LU', 'Species richness (S)', datacubepaths, FALSE))
})

test_that("test ebv_i_datacubepath no string values success", {
  datacubepaths <- data.frame(c('scenario_1/metric_1/ebv_cube', 'scenario_1/metric_2/ebv_cube',
                                'scenario_1/metric_3/ebv_cube',
                                'scenario_2/metric_1/ebv_cube', 'scenario_2/metric_2/ebv_cube',
                                'scenario_2/metric_3/ebv_cube',
                                'scenario_3/metric_1/ebv_cube', 'scenario_3/metric_2/ebv_cube',
                                'scenario_3/metric_3/ebv_cube'))
  datacubepaths <- cbind(datacubepaths, c('SSP1-RCP1.5 LU','SSP1-RCP1.5 LU','SSP1-RCP1.5 LU',
                                          'SSP3-RCP6.0 LU','SSP3-RCP6.0 LU','SSP3-RCP6.0 LU',
                                          'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU'))
  datacubepaths <- cbind(datacubepaths, c('Species richness (S)','Relative species richness change (Delta_S)',
                                          'Diversity-weighted relative species richness change (Delta_SS)'))
  colnames(datacubepaths) <- c('datacubepaths', 'scenario_names', 'metric_names')

  result <- ebv_i_datacubepath(1, 3, datacubepaths, FALSE)
  expect_equal(result, "scenario_1/metric_3/ebv_cube")
})


test_that("test ebv_i_datacubepath no string values error", {
  datacubepaths <- data.frame(c('scenario_1/metric_1/ebv_cube', 'scenario_1/metric_2/ebv_cube',
                                'scenario_1/metric_3/ebv_cube',
                                'scenario_2/metric_1/ebv_cube', 'scenario_2/metric_2/ebv_cube',
                                'scenario_2/metric_3/ebv_cube',
                                'scenario_3/metric_1/ebv_cube', 'scenario_3/metric_2/ebv_cube',
                                'scenario_3/metric_3/ebv_cube'))
  datacubepaths <- cbind(datacubepaths, c('SSP1-RCP1.5 LU','SSP1-RCP1.5 LU','SSP1-RCP1.5 LU',
                                          'SSP3-RCP6.0 LU','SSP3-RCP6.0 LU','SSP3-RCP6.0 LU',
                                          'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU', 'SSP5-RCP8.5 LU'))
  datacubepaths <- cbind(datacubepaths, c('Species richness (S)','Relative species richness change (Delta_S)',
                                          'Diversity-weighted relative species richness change (Delta_SS)'))
  colnames(datacubepaths) <- c('datacubepaths', 'scenario_names', 'metric_names')

  expect_error(ebv_i_datacubepath(1, 5, datacubepaths, FALSE))
})
