#' Cosine-transform aspect
#'
#' @description Transforms simple cardinal aspect values into their cosines
#' @param x A vector, dataframe, or raster of aspect values
#' @param unit Character. Units that aspect are in natively, either degrees or radians
#' @return A vector, dataframe, or raster of cosine-transformed aspect values
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
#' a <- terra::terrain(crb, v='aspect')
#' u <- 'deg'
#' c.aspect <- cosaspect(a, u)
#' plot(c.aspect)
#' }

# Cosine transform aspect
cosaspect <- function(x, unit=c('rad','deg')) {
  if (unit %in% c('rad','deg')) {
    deg = F
    if (unit=='deg') {
      x = d2r(x)
      deg = T
    }
    cos.asp <- cos(x)
    names(cos.asp) <- 'usgs_cosaspect_100m'
    if (deg) {return(r2d(cos.asp))
    } else {return(cos.asp)}
  } else print('unit must be rad or deg')
}

