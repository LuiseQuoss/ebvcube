# ebvcube 0.2.1
## Bug fixes
- remove old url of codecov from badge

# ebvcube 0.2.0

## Major changes
- all (affected) functions: define timestep(s) also as an ISO formatted date
- add new arguments: define metric and scenario by name & index instead of the datacubepath
- new function: ebv_create_taxonomy including new utils function ebv_i_char_variable
- add taxonomy-info the ebv_properties
- ebv_create(_taxonomy): if one timestep but start and end date differ, apply end date

## Bug fixes
- ebv_map: remove outdated stuff from guide_bins
- ebv_write: DelayedArray 'name' variable was not defined

# ebvcube 0.1.7

## Major changes
- ebv_download: download via DOI
- ebv_create: lon and lat units changed (see below)
- EBVcube format: geospatial units: 'degrees'/'meters' to 'degree'/'meter'
- ebv_download: return path of netCDF file also if it is already downloaded 

## Bug fixes
- remove ebv_i_file_opened from the functions
- ebv_read: read-access only

# ebvcube 0.1.6

## Major changes
- ebv_resample: added more methods
- ebv_create: lower compression to 5
- ebv_download: return path
- ebv_map: col_rev does not reverse the numbers in the legend

## Bug fixes
- ebv_download: fix the internet check
- ebv_add_data: array data error (transpose)
- ebv_attribtue: change 'coordinate' to 'coordinates' (black-list attribute)

# ebvcube 0.1.4
## Major changes
- ebv_properties: speed up for nc with many entities

## Bug fixes
- ebv_datacubepaths: implement CRAN feedback - read-only flag was missing at one point

# ebvcube 0.1.3
## Major changes
- ebv_trend: alter trend-plot (dashed-lines, subtitle)
- ebv_map: correct the display of binary maps
- ebv_create: implemented paleo and irregular dates netCDFs (terranova)
- ebv_create: correct typo: coordinates instead of coordinate for entity coordinate
- ebv_properties: remove backwards compatibility to old standard (before ACDD)
- replaced h5ls by h5dump to make ebv_properties and ebv_datacubepaths faster
- ebv_properties: rename 'timesteps_natural' to 'dates' (changes across functions)

## Bug fixes
- ebv_create: contributor_name, content coverage type and domain corrected (comma + white space)
- ebv_i_file_opened: solve error on MAC OS (CRAN)
- ebv_i_eval_epsg: suppress warning by terra when assigning a crs (test error). [#23](https://github.com/LuiseQuoss/ebvcube/issues/23) by Roger Bivand

# ebvcube 0.1.2
## Major changes
- implement CRAN feedback (ebv_i_file_opened (lsof to fuser), examples and test (writing to user library))

## Bug fixes
- ebv_create: keywords for spatial_scope
- ebv_attribute: ebv_cube attributes connected to metric (changes over all scenarios)

# ebvcube 0.1.1
## Major changes
- implement CRAN feedback (examples(dontrun))

## Bug fixes

# ebvcube 0.1.0
## Major changes
- using ggplot2 for plots
- using terra for geospatial operations (removing dependencies: gdalUtils, rgdal, raster and sp)

## Bug fixes


# ebvcube 0.0.1
First version of the package
