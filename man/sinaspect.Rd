% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sin.aspect.R
\name{sinaspect}
\alias{sinaspect}
\title{Cosine-transform aspect}
\usage{
sinaspect(x, unit = c("rad", "deg"))
}
\arguments{
\item{x}{A vector, dataframe, or raster of aspect values}

\item{unit}{Character. Units that aspect are in natively, either degrees or radians}
}
\value{
A vector, dataframe, or raster of sin-transformed aspect values
}
\description{
Transforms simple cardinal aspect values into their sines
}
\examples{
\dontrun{
library(terra)
crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
a <- terra::terrain(crb, v='aspect')
u <- 'deg'
s.aspect <- sinaspect(a, u)
plot(s.aspect)
}
}
