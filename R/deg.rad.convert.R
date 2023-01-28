#' Convert degrees to radians or radians to degrees
#'
#' @description calculates zonal statistics for raster inputs given an input shapefile or coordinates
#' @param a a float angle in degrees or radians
#' @return the angle converted into degrees or radians
#' @export
#'
#' @examples
#' \dontrun{
#' d <- 25.5
#' r <- d2r(d)
#' d <- r2d(r)
#' }

# Degrees to radians
d2r <- function(a) a * pi / 180

# Radians to degrees
r2d <- function(a) a * 180 / pi