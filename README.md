
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ebvnetcdf package

<!-- badges: start -->

<!-- badges: end -->

This package can be used to easily access the data of the EBV NetCDFs
which can be downloaded from the [Geobon
Portal](https://portal.geobon.org/). It also provides some basic
visualization. Advanced users can build their own NetCDFs with the EBV
standard.

## EBVs - Essential Biodiversity Variables

The EBV NetCDF standard is designed to hold Essential Biodiversity
Variables. This concept is further described
[here](https://geobon.org/ebvs/what-are-ebvs/). An important core
element of the EBV NetCDFs is their nested structure. All datacubes in
the NetCDF are assigned to one metric. But this metric can have several
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

The following is a practical example of the NetCDF structure. Basis is
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

Just keep in mind: All EBV NetCDF always have a metric. But they may or
may not have a scenario and/or entity. The resulting entities hold the
datacubepaths we are going to access.

## Installation

You can install the ebvnetcdf packages with:

``` r
#visibility needs to be set to public first! Does not work right now. 
devtools::install_gitlab("lq39quba/ebvnetcdf", host='git.idiv.de') 
```

## Working with the package - a quick intro

### Take a very first look at the file

With the following two functions you get a basic impression what data
you can get from the EBV NetCDF. First we take a look at some basic
properties of that file.

``` r
library(ebvnetcdf)
#> Warning: replacing previous import 'colorspace::RGB' by 'raster::RGB' when
#> loading 'ebvnetcdf'
#> Warning: replacing previous import 'raster::quantile' by 'stats::quantile' when
#> loading 'ebvnetcdf'

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
#> 
#> $value_range
#> [1] NA
slotNames(prop.file)
#> [1] "general"  "spatial"  "temporal" "metric"   "scenario" "entity"
```

Now let’s get the paths to all possible datacubes. The resulting
dataframe includes the paths and also descriptions of e.g. metric and or
scenario - take a look\!

``` r
datacubes <- ebv_datacubepaths(file)
datacubes
#>         paths scenario_longnames metric_longnames         entity_longnames
#> 1 past/mean/0  past: 1900 - 2015             mean non forest birds species
#> 2 past/mean/A  past: 1900 - 2015             mean         all brid species
#> 3 past/mean/F  past: 1900 - 2015             mean      forest bird species
```

We will get the properties of one specific datacube - fyi: the result
also holds the general file properties from above. This time you get the
warning that the value\_range does not exists. So don’t take the
displayed value\_range seriously.

``` r
prop.dc <- ebv_properties(file, datacubes[1,1], verbose=T)
#> Warning in ebv_i_read_att(hdf, "value_range"): The attribute value_range does not exist. Or maybe wrong location in NetCDF?
prop.dc@entity
#> $long_name
#> [1] "Changes in local bird diversity (cSAR)"
#> 
#> $label
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
```

### Plot the data to get a better impression

Plot a map of the datacube that we just looked at - it has 12 timesteps,
mabe look at the sixth one?

``` r
#plot the global map
dc <- datacubes[1,1]
ebv_plot_map(file, dc, timestep = 6)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r

# What was the data about again? Check the properties!
prop.dc@general$title
#> [1] "Changes in local bird diversity (cSAR)"
# And the datacube?
prop.dc@entity$label
#> [1] "non forest birds species"
#What time is the sixth timestep representing?
prop.dc@temporal$timesteps.natural[6]
#> [1] "1960-01-01"
```

It’s nice to see the global distribution, but how is the change of that
datacube (non forest birds) over time? Let’s take a look at the average.
The function returns the values, catch them\!

``` r
#get the averages and plot
averages <- ebv_plot_indicator(file, dc)
#> [1] "calculating timesteps..."
#> ================================================================================
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

It would be cool to have that for other indicators as well? Well you
have to wait for an update of the package. Or maybe implement it
yourself using the functions coming up next?

### Read the data from the files to start working

Before you actually load the data it may be nice to get an impression of
the value range and other basic measurements.

``` r
#info for whole dataset
measurements <- ebv_data_analyse(file, dc)
#see the included measurements
names(measurements)
#> [1] "min"  "q25"  "q50"  "mean" "q75"  "max"  "std"  "n"    "NAs"
#how many pixels are included?
measurements$n
#> [1] 64800

#info for a subset defined by a bounding box (roughly(!) Germany)
bb <- c(5,15,47,55)
measurements.bb <- ebv_data_analyse(file, dc, bb)
#how many pixels are now included?
measurements.bb$n
#> [1] 80
```

To access the data ampoong other things you can use the following.
Subsetting the data using a shapefile needs a directory for temporarly
created files.

``` r
#load whole data set for two timesteps
data <- ebv_data_read(file, dc, c(1,2), delayed = F)
dim(data)
#> [1] 180 360   2

#load subset from shapefile (Germany)
shp <- system.file(file.path('extdata','ne_10m_admin_0_countries_subset_germany.shp'), package="ebvnetcdf")
#define directory for temporary files
options('temp_directory'=system.file("extdata/", package="ebvnetcdf"))
data.shp <- ebv_data_read_shp(file, dc, shp, NULL, c(1,2,3))
dim(data.shp)
#> [1]  9 11  3
#very quick plot of the resulting raster plus the shapefile
shp.data <- rgdal::readOGR(shp)
#> OGR data source with driver: ESRI Shapefile 
#> Source: "/tmp/Rtmp227rMD/temp_libpath27b2276726b9/ebvnetcdf/extdata/ne_10m_admin_0_countries_subset_germany.shp", layer: "ne_10m_admin_0_countries_subset_germany"
#> with 1 features
#> It has 94 fields
#> Integer64 fields read as strings:  POP_EST NE_ID
raster::spplot(data.shp[[1]], sp.layout = list(shp.data, first=FALSE))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Add Delayed Data, write data?

### Take a peek on the creation of an EBV NetCDF

This process is still work in progress. Right now you’ll have to insert
all the metadata in the Geobon Portal and then use the resulting json
file to create an empty NetCDF file which complies to the EBV NetCDF
standard. It has the correct structure and holds the metadata.
Additionally to that json file the function needs the amount of entities
the NetCDF will encompass.

The example is based on the [Global habitat availability for
mammals](https://portal.geobon.org/ebv-detail?id=5). As its ID in the
geoportal is 5 the json file is just called 5.

``` r
json <- system.file(file.path('extdata','5.json'), package="ebvnetcdf")
newNc <- file.path(system.file(package="ebvnetcdf"),'extdata','mammals.nc')
#lets say it has 5 entities, which is not true in reality!
ebv_ncdf_create(json, newNc, 5, overwrite=T)
#check out the general propeties of our newly created file
print(ebv_properties(newNc)@general)
#> $title
#> [1] "Global habitat availability for mammals from 2015-2055"
#> 
#> $description
#> [1] "Global habitat availability for 5,090 mammals in 5 year intervals (subset from 2015 to 2055)."
#> 
#> $ebv_class
#> [1] "Species populations"
#> 
#> $ebv_name
#> [1] "Species distributions"
#> 
#> $ebv_subgroups
#> [1] "scenario" "metric"   "entity"  
#> 
#> $creator
#> [1] "Daniele Baisero"
#> 
#> $value_range
#> [1] NA
#it has no datacubepaths yet - makes sense. we haven't added any data yet...
print(ebv_datacubepaths(newNc))
#> data frame with 0 columns and 0 rows
```

Tip: You can always take a look at your NetCDF in
[Panoply](https://www.giss.nasa.gov/tools/panoply/) provided by NASA.
That’s very helpful to understand the structure.

In the next step you can add your data to the NetCDF from GeoTiff files.
You need to indicate which scenario and/or metric and/or entity the data
belongs to. You can add your data timestep per timestep, in slices or
all at once (see the vignette for detailed info).

You can simply add more data to the same datacube by changing the
timestep
definition.

``` r
tif <- system.file(file.path('extdata','mammals_ts123.tif'), package="ebvnetcdf") 
ebv_ncdf_add_data(newNc, tif, metric=1, scenario=1, entity=1, timestep=c(1,2,3), band=c(1,2,3))
#now we have a datacubepath we can access
dc.new <- ebv_datacubepaths(newNc)
dc.new
#>                          paths scenario_longnames metric_longnames
#> 1 scenario00/metric00/entity00        SSP1-RCP2.6              km2
#>   entity_longnames
#> 1          default
```

Now there are still a few information missing about the data you just
added. The following function makes it possible to add the
information.

``` r
#taking a look at the properties first - uff it's mostely default values (also the fillvalue!)
print(ebv_properties(newNc, dc.new[1,1])@entity)
#> $long_name
#> [1] "default"
#> 
#> $label
#> [1] "default"
#> 
#> $unit
#> [1] "default"
#> 
#> $type
#> [1] "H5T_IEEE_F32LE"
#> 
#> $fillvalue
#> [1] 999
#adding the correct infos:
ebv_ncdf_entity_attributes(newNc, dc.new[1,1], long_name='Data on Area Of Habitat (AOH)', label='Eumops auripendulu', units='land-use of mammals calculated in km2', fillvalue=-3.4e+38)
#rechecking properties - now it looks good! but there is a typo for the label..
print(ebv_properties(newNc, dc.new[1,1])@entity)
#> $long_name
#> [1] "Data on Area Of Habitat (AOH)"
#> 
#> $label
#> [1] "Eumops auripendulu"
#> 
#> $unit
#> [1] "land-use of mammals calculated in km2"
#> 
#> $type
#> [1] "H5T_IEEE_F32LE"
#> 
#> $fillvalue
#> [1] -3.4e+38
```

Ups\! So you did a mistake and want to change the attribute?\! No
problem:

``` r
ebv_ncdf_write_attribute(newNc, attribute_name='label', value='Eumops auripendulus', levelpath=dc.new[1,1])
#check the properties one more time - perfect!
print(ebv_properties(newNc, dc.new[1,1])@entity)
#> $long_name
#> [1] "Data on Area Of Habitat (AOH)"
#> 
#> $label
#> [1] "Eumops auripendulus"
#> 
#> $unit
#> [1] "land-use of mammals calculated in km2"
#> 
#> $type
#> [1] "H5T_IEEE_F32LE"
#> 
#> $fillvalue
#> [1] -3.4e+38
```

In this case the levelpath corresponds to the datacube path. But you can
also alter attributes at the metric or scenario level. See the vignette
for more info.