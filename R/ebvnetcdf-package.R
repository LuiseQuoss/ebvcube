#' Working with EVB NetCDFs
#'
#'
#' This package can be used to easily access the data of the EBV NetCDFs which
#' can be downloaded from the \href{portal.geobon.org}{Geobon Portal}. It also
#' provides some basic visualization of the data.  Advanced users can build
#' their own NetCDFs with the EBV standard using this package.
#'
#' This package contains three main usecases: accessing the data and visualising
#' the data from the portal and creating your own data in the EBV NetCDF
#' standard. All function have a corresponding naming pattern: ebv_data_ for
#' data reading, ebv_plot_ for visualisation and ebv_netcdf_ for creating a
#' NetCDF.
#'
#' @import checkmate colorspace gdalUtils HDF5Array jsonlite lattice memuse
#'   methods ncdf4 raster rhdf5 rgdal sp stringr withr
#'
#' @docType package
#' @name ebvnetcdf
NULL
