% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ebv_trend.R
\name{ebv_trend}
\alias{ebv_trend}
\title{Plot the trend of an EBV netCDF}
\usage{
ebv_trend(
  filepath,
  datacubepath = NULL,
  entity = NULL,
  method = "mean",
  subset = NULL,
  color = "dodgerblue4",
  touches = TRUE,
  scenario = NULL,
  metric = NULL,
  verbose = TRUE
)
}
\arguments{
\item{filepath}{Character. Path to the netCDF file.}

\item{datacubepath}{Character. Optional. Default: NULL. Path to the datacube
(use \code{\link[=ebv_datacubepaths]{ebv_datacubepaths()}}). Alternatively, you can use the
scenario and metric argument to define which cube you want to access.}

\item{entity}{Character or Integer. Default is NULL. If the structure is 3D,
the entity argument is set to NULL. Else, a character string or single
integer value must indicate the entity of the 4D structure of the EBV
netCDFs.}

\item{method}{Character. Default: mean. Choose one of the following options
for different plots: mean, min, max, boxplot. See \strong{Note} for more}

\item{subset}{Character. Default: NULL. If you want to look at the trend for
a spatial subset, define the path to the shapefile encompassing the area.
Ending needs to be *.shp.}

\item{color}{Character. Default: dodgerblue4. Change to any color known by R
\code{\link[grDevices:colors]{grDevices::colors()}}}

\item{touches}{Logical. Optional. Default: TRUE. Only relevant if the subset is
indicated by a shapefile. See \code{\link[=ebv_read_shp]{ebv_read_shp()}}.}

\item{scenario}{Character or integer. Optional. Default: NULL. Define the
scenario you want to access. If the EBV netCDF has no scenarios, leave the
default value (NULL). You can use an integer value defining the scenario or
give the name of the scenario as a character string. To check the available
scenarios and their name or number (integer), use
\code{\link[=ebv_datacubepaths]{ebv_datacubepaths()}}.}

\item{metric}{Character or integer. Optional. Define the metric you want to
access. You can use an integer value defining the metric or give the name
of the scenario as a character string. To check the available metrics and
their name or number (integer), use \code{\link[=ebv_datacubepaths]{ebv_datacubepaths()}}.}

\item{verbose}{Logical. Default: TRUE. Turn off additional prints by setting
it to FALSE.}
}
\value{
Returns plots and eventually values based on the \code{method} argument.
See \strong{Note} for more information
}
\description{
Plot the trend of one datacube of a EBV netCDF over time
(x-axis). Different options can be chosen based on the \code{method} argument.
}
\note{
More information on the \code{method} argument: using \code{mean} will result in
a plot of the mean over time, additionally a vector of the mean values is
returned. If the data encompasses only one timestep a single mean is
returned. Corresponding behavior can be expected for \code{min} and \code{max}. The
\code{boxplot} option results in boxplots over time (no values are returned).
}
\examples{
#set path to EBV netCDF
file <- system.file(file.path("extdata","martins_comcom_subset.nc"), package="ebvcube")
#get all datacubepaths of EBV netCDF
datacubes <- ebv_datacubepaths(file, verbose=FALSE)

\donttest{
#plot the change of the mean over time of the first datacube
ebv_trend(filepath = file, datacubepath = datacubes[1,1], entity = 1)
}
}
