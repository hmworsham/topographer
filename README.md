# topographer (dev 0.0.2.2)
<!-- badges: start -->
[![CRAN status](http://www.r-pkg.org/badges/version/topographer)](https://cran.r-project.org/package=topographer)
[![R-CMD-check](https://github.com/hmworsham/topographer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hmworsham/topographer/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->
**topographer** R package with utilities to support spatial data ingest, manipulation, and transformation.
	Designed to operate on scalars, vectors, dataframes, `terra:SpatRaster`, and outmoded `raster:RasterLayer` objects.
	Functions include resolution transformations and calculations for a handful of topographic parameters: curvature,
	various transformed aspects, heat load, topographic position index, topographic wetness index, upslope 
	contributing area. Also includes functions to retrieve zonal statistics from rasters using `sf` objects or 
	point coordinates. Integrates and adapts some functions from Jeffrey Evans's R package `spatialEco` and Paul Smith's implementation of `topmodel`.

## Note
`topographer` is in dev mode. At this point I've developed it mostly for use in analyses for my own research projects and those of a few collaborators. Most of the functions are derived from other sources and R packages, just structured and/or integrated in a way that makes them easier for me to use. I haven't yet seen a case for publishing the package to CRAN, but may do so in future. 

## Available functions in topographer 0.0.2.0

| `topographer` Function       | Description                                                                             |
|:-----------------------------|:----------------------------------------------------------------------------------------|
| `aspect.transform`           | Transforms simple cardinal aspect values to ecologically meaningful values  |
| `change.res`                 | Changes raster resolution via aggregation or disaggregation  |
| `curvature`                  | Calculate surface curvature |
| `d2r`                        | Convert degrees to radians  |
| `r2d`                        | Convert radians to degrees  |
| `get.rasters`                | Ingest tif files as terra::SpatRaster objects and store in list  |
| `heatload`                   | Calculate total heat load  | 
| `tpi`                        | Calculate topographic position index (TPI) on a specified resolution and window size  |
| `twi`                        | Calculate topographic wetness index (TWI) on a specified resolution  | 
| `uca`                        | Calculate upslope contributing area  | 
| `zonals`                     | Retrieve zonal statistics for a shapefile or coordinate input  | 


## Installation
To install `topographer` dev version run:
`remotes::install_github(hmworsham/topographer)`

## Dependencies
- terra
- raster
- topmodel

## Bugs
Bugs: Users are encouraged to report bugs directly in GitHub. Select **Issues** in the menu above, and create a **New issue** to start a new bug report, documentation correction, or feature request. Questions to worsham@berkeley.edu.
