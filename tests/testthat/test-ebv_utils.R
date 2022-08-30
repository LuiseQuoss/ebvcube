test_that("test ebv_i_transform_bb", {
  new_bb <- ebv_i_transform_bb(c(-50,50,20,40), 4326, 3857)
  expect_equal(new_bb, c(-5565974.540,  5565974.540,  2273030.927,  4865942.280))
})

test_that("test ebv_i_eval_epsg 4326", {
  crs_wkt <- ebv_i_eval_epsg(4326)
  expect_equal(crs_wkt, 'GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]')
})

test_that("test ebv_i_eval_epsg ESRI:54009", {
  crs_wkt <- ebv_i_eval_epsg('ESRI:54009')
  expect_equal(crs_wkt, 'PROJCRS[\"World_Mollweide\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"World_Mollweide\",\n        METHOD[\"Mollweide\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Not known.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"ESRI\",54009]]')
})

test_that("test ebv_i_eval_epsg 4326 return proj", {
  crs_proj <- ebv_i_eval_epsg(4326, T)
  expect_equal(crs_proj, '+proj=longlat +datum=WGS84 +no_defs')
})

test_that("test ebv_i_eval_epsg ESRI:54009 return proj", {
  crs_proj <- ebv_i_eval_epsg('ESRI:54009', T)
  expect_equal(crs_proj, '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
})

test_that("test ebv_i_get_epsg 4326", {
  crs_epsg <- ebv_i_get_epsg('GEOGCRS[\"WGS 84\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]')
  expect_equal(crs_epsg, 4326)
})

test_that("test ebv_i_get_epsg ESRI:54009", {
  crs_epsg <- ebv_i_get_epsg('PROJCRS[\"World_Mollweide\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"World_Mollweide\",\n        METHOD[\"Mollweide\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Not known.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"ESRI\",54009]]')
  expect_equal(crs_epsg, 'ESRI:54009')
})
