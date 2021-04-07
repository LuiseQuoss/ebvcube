
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ebvnetcdf

<!-- badges: start -->

<!-- badges: end -->

This package can be used to easily access the data of the EBV NetCDFs
which can be downloaded from the [Geobon
Portal](https://portal.geobon.org/). It also provides some basic
visualization. Advanced users can build their own NetCDFs with the EBV
standard.

## Installation

The ebvnetcdf packages is not yet released on CRAN. You can install it
with:

``` r
#visibility needs to be set to public first! Does not work right now. 
devtools::install_gitlab("lq39quba/ebvnetcdf", host='git.idiv.de') 
```

## Working with the package - a quick intro

### Take a very first look at the file

With these two functions you get a basic impression what data you can
get from the EBV NetCDF.

``` r
library(ebvnetcdf)

file <- paste0(path.package("ebvnetcdf"),"/extdata/cSAR_idiv_v1.nc")

#take a look at some basic properties of that file
#prop.file <- ebv_properties(file)

#get all possible paths to the datacubes - dataframe including the paths and also descriptions of e.g. metric and or scenario - take a look!
#datacubes <- ebv_datacubepaths(file)

#get properties of one specific datacube - also holds the general file properties from above
#this time you get the warning that the value_range does not exists. So don't take the displayed value_range seriously.
#prop.non_forest_birds <- ebv_properties(file, datacubes[1,1], verbose=T)
```

### Plot some data to get a better impression

### Get some data from the files to start working

### Take a peak on the creation of an EBV NetCDF
