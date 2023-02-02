#' Cosine-transform aspect
#'
#' @description Transforms simple cardinal aspect values into their sines
#' @param x A vector, dataframe, or raster of aspect values
#' @param unit Character. Units that aspect are in natively, either degrees or radians
#' @return A vector, dataframe, or raster of sin-transformed aspect values
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
#' a <- terra::terrain(crb, v='aspect')
#' u <- 'deg'
#' s.aspect <- sinaspect(a, u)
#' plot(s.aspect)
#' }

# Sine transform aspect
sinaspect <- function(x, unit=c('rad', 'deg')) {
  match.arg(unit)
  deg=F
  if(unit=='deg') {
    x = d2r(x)
    deg = T
  }
  sin.asp <- sin(x)
  names(sin.asp) <- 'usgs_cosaspect_100m'
  if (deg) {return(r2d(sin.asp))
  } else {return(sin.asp)}
}
