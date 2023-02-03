#' Facing aspect
#'
#' @description Transforms simple cardinal aspect values into values relative to a focal direction
#' @param slope A vector, dataframe or raster of slope values
#' @param aspect A vector, dataframe, or raster of aspect values
#' @param focal Integer. A focal angle for transforming aspect into a 'facing' value
#' @param unit Character. Units that aspect are in natively, either degrees or radians
#' @param fold Numeric. Angle along which aspect should be folded
#' @return A vector, dataframe, or raster of facing-transformed aspect values
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
#' s <- terra::terrain(crb, v='slope')
#' a <- terra::terrain(crb, v='aspect')
#' f <- 180
#' u <- 'deg'
#' facing.270 <- facing(s,a,270,u)
#' plot(facing.270)
#' }

# Which direction a slope is facing
facing <- function(slope, aspect, focal=180, unit='rad') {
  if (unit %in% c('rad','deg')) {
    deg = F
    if (unit=='deg') {
      slope = d2r(slope)
      aspect = d2r(aspect)
      deg = T
    }
    aspect = d2r(focal) - aspect
    return(sin(slope) * cos(aspect))
  } else print('unit must be rad or deg')
}
