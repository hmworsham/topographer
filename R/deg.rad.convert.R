#' Convert degrees to radians and radians to degrees

#' @title Convert degrees to radians
#' @description calculates zonal statistics for raster inputs given an input shapefile or coordinates
#' @param a a float angle in degrees or radians
#' @return the angle converted into degrees or radians
#' @export d2r
#' @export r2d
#' @examples
#' \dontrun{
#' d <- 25.5
#' r <- d2r(d)
#' d <- r2d(r)
#' }

d2r <- function(a) a * pi / 180

#' @title Convert radians to degrees
#' @description calculates zonal statistics for raster inputs given an input shapefile or coordinates
#' @param a a float angle in degrees or radians
#' @return the angle converted into degrees or radians
#' @export r2d
#' @examples
#' \dontrun{
#' d <- 25.5
#' r <- d2r(d)
#' d <- r2d(r)
#' }

r2d <- function(a) a * 180 / pi
