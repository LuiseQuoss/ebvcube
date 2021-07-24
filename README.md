
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ebvnetcdf package

<!-- badges: start -->
<!-- badges: end -->

This package can be used to easily access the data of the EBV netCDFs
which can be downloaded from the [Geobon
Portal](https://portal.geobon.org/). It also provides some basic
visualization. Advanced users can build their own netCDFs with the EBV
standard.

## 1. EBVs - Essential Biodiversity Variables

The EBV netCDF standard is designed to hold Essential Biodiversity
Variables. This concept is further described
[here](https://geobon.org/ebvs/what-are-ebvs/). An important core
element of the EBV netCDFs is their nested structure. All datacubes in
the netCDF are assigned to one metric. But this metric can have several
entities. On top of this hierarchy there can be several scenarios. The
following block displays an abstract exhausted hierarchy.

``` bash
├── scenario1
│   └── metric1
│       ├── entity1
│       ├── entity2
│       ├── ...
│       └── entity999
└── scenario2
    └── metric2
        ├── entity1
        ├── entity2
        ├── ...
        └── entity999
```

The following is a practical example of the netCDF structure. Basis is
the [global habitat availability for mammals
dataset](https://portal.geobon.org/ebv-detail?id=5).

``` bash
├── SSP1-RCP2.6
│   └── absolute values per 5 years and species (km2)
│       ├── Hipposideros calcaratus
│       ├── Hipposideros fulvus
│       ├── ...
│       └── Habromys lepturus
│
├── SSP2-RCP4.5
│   └── absolute values per 5 years and species (km2)
│       ├── ...
│       └── Habromys lepturus
├── ...
│
└── SSP5-RCP8.5
    └── absolute values per 5 years and species (km2)
        ├── ...
        └── Habromys lepturus    
```

Just keep in mind: All EBV netCDF always have a metric. But they may or
may not have a scenario and/or entity. The resulting datacubes are going
to be accesses.

## 2. Installation

You can install the ebvnetcdf packages with:

``` r
#currently:
# 1. pull git repo
# 2. open ebvnetcdf.RProj
# 3. install(devtools)
# 4. install(dependencies=T)

#future: when repo is public (Does not work right now.)
devtools::install_gitlab("lq39quba/ebvnetcdf", host='git.idiv.de')
```

This packages uses GDAL tools (GDAL version: 3.1.4). You need a GDAL
installation on your machine. One possibility to install GDAL is the
OSGeo4W Network installer <https://trac.osgeo.org/osgeo4w/>. Check GDAL
when running the installation! If you have QGIS on your machine, GDAL
should be included. To check that, install the gdalUtils package in R
and run gdalUtils::gdal_setInstallation(). It takes quite a while the
first time as it looks for a GDAL installation. Now
getOption(‘gdalUtils_gdalPath’) should return the GDAL installation. If
you have problems you can set the GDAL related paths by hand using the
following lines of code. Your paths may differ! First check your GDAL
installation.

``` r
#add GDAL path to the existing paths
Sys.getenv("PATH")
#> [1] "C:\\rtools40\\usr\\bin;C:\\OSGeo4W64\\bin;C:\\OSGeo4W64\\bin;C:\\rtools40\\usr\\bin;C:\\Users\\lq39quba\\Documents\\R\\R-4.0.3\\bin\\x64;C:\\Program Files\\Common Files\\Oracle\\Java\\javapath_target_16854906;C:\\Windows\\System32;C:\\Windows;C:\\Windows\\System32\\wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Windows\\System32\\OpenSSH;C:\\Program Files\\Git\\cmd;C:\\Program Files\\PuTTY;C:\\Users\\lq39quba\\AppData\\Local\\Programs\\MiKTeX\\miktex\\bin\\x64;C:\\Users\\lq39quba\\AppData\\Local\\Programs\\Python\\Python39\\Scripts;C:\\Users\\lq39quba\\AppData\\Local\\Programs\\Python\\Python39;C:\\Users\\lq39quba\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\lq39quba\\AppData\\Local\\Programs\\MiKTeX\\miktex\\bin\\x64;C:\\Users\\lq39quba\\AppData\\Local\\Pandoc"
Sys.setenv(PATH = paste0('C:\\OSGeo4W64\\bin;',Sys.getenv("PATH")))
#check and change path for proj_lib, gdal_data and gdal_driver_path
Sys.getenv("PROJ_LIB") 
#> [1] "C:\\OSGeo4W64\\share\\proj"
Sys.getenv("GDAL_DATA") 
#> [1] "C:\\OSGeo4W64\\share\\gdal"
Sys.getenv("GDAL_DRIVER_PATH") 
#> [1] "C:\\OSGeo4W64\\bin\\gdalplugins"
Sys.setenv(PROJ_LIB = 'C:\\OSGeo4W64\\share\\proj')
Sys.setenv(GDAL_DATA = 'C:\\OSGeo4W64\\share\\gdal')
Sys.setenv(GDAL_DRIVER_PATH = 'C:\\OSGeo4W64\\bin\\gdalplugins')
```

## 3. Working with the package - a quick intro

### 3.1 Take a very first look at the file

With the following two functions you get the core information about the
data from a specific EBV netCDF. First we take a look at some basic
metadata of that file.

``` r
library(ebvnetcdf)

file <- system.file(file.path("extdata","cSAR_idiv_v1.nc"), package="ebvnetcdf")
prop.file <- ebv_properties(file)

#take a look at the general properties of the dataset - there are more properties to discover!
prop.file@general
#> $title
#> [1] "Changes in local bird diversity (cSAR)"
#> 
#> $description
#> [1] "Changes in bird diversity at the grid cell level caused by land-use, estimated by the cSAR model (Martins & Pereira, 2017). It reports changes in species number (percentage and absolute), relative to 1900, for all bird species, forest bird species, and non-forest bird species in each cell. Uses the LUH 2.0 projections for land-use, and the PREDICTS coefficients for bird affinities to land-uses."
#> 
#> $ebv_class
#> [1] "Community composition"
#> 
#> $ebv_name
#> [1] "Species diversity"
#> 
#> $ebv_subgroups
#> [1] "scenario" "metric"   "entity"  
#> 
#> $creator
#> [1] "Ines Martins"
slotNames(prop.file)
#> [1] "general"  "spatial"  "temporal" "metric"   "scenario" "entity"
```

Now let’s get the paths to all possible datacubes. The resulting
dataframe includes the paths and also descriptions of the metric and/or
scenario and/or entity.

``` r
datacubes <- ebv_datacubepaths(file)
datacubes
#>   datacubepaths    scenario_names metric_names             entity_names
#> 1   past/mean/0 past: 1900 - 2015         mean non forest birds species
#> 2   past/mean/A past: 1900 - 2015         mean         all brid species
#> 3   past/mean/F past: 1900 - 2015         mean      forest bird species
```

In the next step we will get the properties of one specific datacube -
fyi: the result also holds the general file properties from above. This
time you get the warning that the value_range does not exists. So don’t
take the displayed value_range seriously.

``` r
prop.dc <- ebv_properties(file, datacubes[1,1], verbose=T)
#> Warning in h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native
#> = native): An open HDF5 file handle exists. If the file has changed on disk
#> meanwhile, the function may not work properly. Run 'h5closeAll()' to close all
#> open HDF5 object handles.

#> Warning in h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native
#> = native): An open HDF5 file handle exists. If the file has changed on disk
#> meanwhile, the function may not work properly. Run 'h5closeAll()' to close all
#> open HDF5 object handles.

#> Warning in h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native
#> = native): An open HDF5 file handle exists. If the file has changed on disk
#> meanwhile, the function may not work properly. Run 'h5closeAll()' to close all
#> open HDF5 object handles.

#> Warning in h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native
#> = native): An open HDF5 file handle exists. If the file has changed on disk
#> meanwhile, the function may not work properly. Run 'h5closeAll()' to close all
#> open HDF5 object handles.
#> Warning in ebv_i_read_att(hdf, "value_range"): The attribute value_range does not exist. Or maybe wrong location in NetCDF?
prop.dc@entity
#> $description
#> [1] "Changes in bird diversity at the grid cell level caused by land-use, estimated by the cSAR model (Martins & Pereira, 2017). It reports changes in species number (percentage and absolute), relative to 1900, for all bird species, forest bird species, and non-forest bird species in each cell. Uses the LUH 2.0 projections for land-use, and the PREDICTS coefficients for bird affinities to land-uses."
#> 
#> $standard_name
#> [1] "non forest birds species"
#> 
#> $unit
#> [1] "mean change of species diversity per area (pixel size) to baseline 1900 "
#> 
#> $type
#> [1] "H5T_IEEE_F32LE"
#> 
#> $fillvalue
#> [1] -3.4e+38
#> 
#> $value_range
#> [1] NA
```

### 3.2 Plot the data to get a better impression

To discover the spatial distribution of the data you can plot a map of
the datacube that we just looked at. It has 12 timesteps. Here we look
at the sixth one.

``` r
#plot the global map
dc <- datacubes[1,1]
ebv_map(file, dc, timestep = 6)
#> Quantiles based on all layers.
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
# What was the data about again? Check the properties!
prop.dc@general$title
#> [1] "Changes in local bird diversity (cSAR)"
# And the datacube?
prop.dc@entity$standard_name
#> [1] "non forest birds species"
#What time is the sixth timestep representing?
prop.dc@temporal$timesteps_natural[6]
#> [1] "1960-01-01"
```

It’s nice to see the global distribution, but how is the change of that
datacube (non forest birds) over time? Let’s take a look at the average.
The function returns the values, catch them!

``` r
#get the averages and plot
averages <- ebv_indicator(file, dc)
#> [1] "calculating timesteps..."
#> ================================================================================
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
averages
#>  [1] 0.4363360 0.4363360 0.8548256 1.2845690 1.7785023 2.2481552 2.8426603
#>  [8] 3.1457002 3.3955101 3.6569338 3.8845097 4.0115587
```

It would be cool to have that for other indicators as well? Well you
have to wait for an update of the package. Or maybe implement it
yourself using the functions coming up next?

### 3.3 Read the data from the files to start working

Before you actually load the data it may be nice to get an impression of
the value range and other basic measurements.

``` r
#info for whole dataset
measurements <- ebv_analyse(file, dc)
#see the included measurements
names(measurements)
#> [1] "min"  "q25"  "q50"  "mean" "q75"  "max"  "std"  "n"    "NAs"
#how many pixels are included?
measurements$n
#> [1] 64800
measurements$mean
#> [1] 0.436336

#info for a subset defined by a bounding box (roughly(!) Germany)
bb <- c(5,15,47,55)
measurements.bb <- ebv_analyse(file, dc, bb)
#> Error in rgdal::make_EPSG(): PROJ 6 database empty
#how many pixels are now included?
measurements.bb$n
#> Error in eval(expr, envir, enclos): Objekt 'measurements.bb' nicht gefunden
measurements.bb$mean
#> Error in eval(expr, envir, enclos): Objekt 'measurements.bb' nicht gefunden
```

To access the data you can use the following:

``` r
#load whole data as array for two timesteps
data <- ebv_read(file, dc, c(1,2), delayed = F)
dim(data)
#> [1] 180 360   2
```

To subset the data using a shapefile you need to indicate a directory
for temporarily created files.

``` r
#load subset from shapefile (Germany)
shp <- system.file(file.path('extdata','subset_germany.shp'), package="ebvnetcdf")
#define directory for temporary files
options('ebv_temp'=system.file("extdata/", package="ebvnetcdf"))
data.shp <- ebv_read_shp(file, dc, shp, NULL, c(1,2,3))
#> Error in rgdal::make_EPSG(): PROJ 6 database empty
dim(data.shp)
#> Error in eval(expr, envir, enclos): Objekt 'data.shp' nicht gefunden
#very quick plot of the resulting raster plus the shapefile
shp.data <- rgdal::readOGR(shp)
#> OGR data source with driver: ESRI Shapefile 
#> Source: "C:\Users\lq39quba\AppData\Local\Temp\RtmpOGmMe8\temp_libpathda8236c44a0\ebvnetcdf\extdata\subset_germany.shp", layer: "subset_germany"
#> with 1 features
#> It has 94 fields
#> Integer64 fields read as strings:  POP_EST NE_ID
raster::spplot(data.shp[[1]], sp.layout = list(shp.data, first=FALSE))
#> Error in h(simpleError(msg, call)): Fehler bei der Auswertung des Argumentes 'obj' bei der Methodenauswahl für Funktion 'spplot': Objekt 'data.shp' nicht gefunden
```

Imagine you have a very large dataset but only limited memory. The
package provides the possibility to load the data as a DelayedArray. A
second function helps you to write that data back on disk properly. Look
into the manual to obtain more information.

### 3.4 Take a peek on the creation of an EBV netCDF

#### a. Create an empty EBV netCDF (with metadata)

This process is still work in progress. Right now you’ll have to insert
all the metadata in the Geobon Portal and then use the resulting json
file to create an empty netCDF file which complies to the EBV netCDF
standard. It has the correct structure and holds the metadata.
Additionally to that json file the function needs the amount of entities
the netCDF will encompass and the coordinate reference system.

The example is based on the [Global habitat availability for
mammals](https://portal.geobon.org/ebv-detail?id=5). As its ID in the
geoportal is 5 the json file is just called 5.

``` r
#paths
json <- system.file(file.path('extdata','5.json'), package="ebvnetcdf")
newNc <- file.path(system.file(package="ebvnetcdf"),'extdata','mammals.nc')
#defining the fillvalue - optional
fv <- -3.4e+38
#lets say it has 5 entities, which is not true in reality!
ebv_create(json, newNc, 5, overwrite=T,fillvalue = fv, prec='float')
#> Error in rgdal::make_EPSG(): PROJ 6 database empty
#check out the general propeties of our newly created file
print(ebv_properties(newNc)@general)
#> Error in ebv_properties(newNc): File does not exist.
#> C:/Users/lq39quba/AppData/Local/Temp/RtmpOGmMe8/temp_libpathda8236c44a0/ebvnetcdf/extdata/mammals.nc
#check out the (still empty) datacubes
dc.new <- ebv_datacubepaths(newNc)
#> Error in ebv_datacubepaths(newNc): File does not exist.
#> C:/Users/lq39quba/AppData/Local/Temp/RtmpOGmMe8/temp_libpathda8236c44a0/ebvnetcdf/extdata/mammals.nc
print(dc.new[c(1,5,6),])
#> Error in print(dc.new[c(1, 5, 6), ]): Objekt 'dc.new' nicht gefunden
```

Hint: You can always take a look at your netCDF in
[Panoply](https://www.giss.nasa.gov/tools/panoply/) provided by NASA.
That’s very helpful to understand the structure.

#### b. Add your data to the EBV NetCDF

In the next step you can add your data to the netCDF from GeoTiff files.
You need to indicate the datacubepath the data belongs to. You can add
your data timestep per timestep, in slices or all at once. You can
simply add more data to the same datacube by changing the timestep
definition.

``` r
#path to tif with data
tif <- system.file(file.path('extdata','mammals_ts123.tif'), package="ebvnetcdf") 
#adding the data
ebv_add_data(newNc, tif, datacubepath=dc.new[1,1], timestep=c(1,2,3), band=c(1,2,3))
#> Error in ebv_add_data(newNc, tif, datacubepath = dc.new[1, 1], timestep = c(1, : NetCDF File does not exist.
#> C:/Users/lq39quba/AppData/Local/Temp/RtmpOGmMe8/temp_libpathda8236c44a0/ebvnetcdf/extdata/mammals.nc
```

#### c. Add missing attributes to datacube

Now there are still a information missing about the data you just added:
the standard_name and description of each entity. The following function
makes it possible to add the information. Ups! So you did a mistake and
want to change the attribute?! No problem. Just use the same function to
change it again.

``` r
ebv_attribute(newNc, attribute_name='standard_name', value='Eumops auripendulus', levelpath=dc.new[1,1])
#> Error in ebv_attribute(newNc, attribute_name = "standard_name", value = "Eumops auripendulus", : NetCDF file does not exist.
#> C:/Users/lq39quba/AppData/Local/Temp/RtmpOGmMe8/temp_libpathda8236c44a0/ebvnetcdf/extdata/mammals.nc
#check the properties one more time - perfect!
print(ebv_properties(newNc, dc.new[1,1])@entity$standard_name)
#> Error in ebv_properties(newNc, dc.new[1, 1]): File does not exist.
#> C:/Users/lq39quba/AppData/Local/Temp/RtmpOGmMe8/temp_libpathda8236c44a0/ebvnetcdf/extdata/mammals.nc
```

In this case the levelpath corresponds to the datacube path. But you can
also alter attributes at the metric or scenario level. See the manual
for more info.

## 4. Cite package

``` r
citation('ebvnetcdf')
#> 
#> To cite ebvnetcdf in publications use:
#> 
#> Quoß L, Pereira H, Fernández N (2021). _ebvnetcdf: Working with netCDF
#> for Essential Biodiversity Variables_. German Centre for Integrative
#> Biodiversity Research (iDiv) Halle-Jena-Leipzig, Germany. R package
#> version 0.0.1, <URL: https://git.idiv.de/lq39quba/ebvnetcdf>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ebvnetcdf: Working with netCDF for Essential Biodiversity Variables},
#>     author = {Luise Quoß and Henrique Miguel Pereira and Néstor Fernández},
#>     year = {2021},
#>     note = {R package version 0.0.1},
#>     organization = {German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig},
#>     address = {Germany},
#>     url = {https://git.idiv.de/lq39quba/ebvnetcdf},
#>   }
```
