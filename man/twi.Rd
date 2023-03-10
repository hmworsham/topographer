% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twi.R
\name{twi}
\alias{twi}
\title{Topographic Wetness Index (TWI)}
\usage{
twi(x, calc.res = 100, out.res = 10, fill.sinks = T, deg = 0.1)
}
\arguments{
\item{x}{A terra SpatRaster or raster RasterLayer object}

\item{calc.res}{Positive integer. Resolution for computing upslope area
and localslope.  Single value asserts equal horz/vert length.
Or two integers for different horz/vert lengths.}

\item{out.res}{Positive integer. Desired resolution of output raster. Single
value asserts equal horz/vert length. Or two integers for
different horz/vert lengths.}

\item{fill.sinks}{Logical. If TRUE (default) then run a sinkfill before
calculating the upslope area and TWI.}

\item{deg}{Threshold intercell slope to determine sinks}
}
\value{
A raster comprising topographic wetness index values
}
\description{
calculates Topographic Wetness Index on a raster DEM
}
\examples{
\dontrun{
library(topographer)
crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
y <- raster(crb)
values(y) <- values(crb)
x.twi <- twi(crb, calc.res=100, out.res=10, fill.sinks=T)
y.twi <- twi(y, calc.res=100, out.res=1000, fill.sinks=T)
plot(x.twi)
plot(y.twi)
}
}
