#' Upslope contributing area
#'
#' @description calculates upslope contributing area per pixel on a raster DEM
#' @param x A terra SpatRaster or raster RasterLayer object
#' @param calc.res Positive integer. Resolution for computing upslope area 
#'  and localslope.  Single value asserts equal horz/vert length. 
#'  Or two integers for different horz/vert lengths. 
#' @param out.res Positive integer. Desired resolution of output raster. Single
#' value asserts equal horz/vert length. Or two integers for
#' different horz/vert lengths. 
#' @param fill_sinks Logical. If TRUE (default) then run a sinkfill before 
#'  calculating the upslope area and TWI.
#' @param deg Threshold intercell slope to determine sinks
#' @return A raster comprising topographic wetness index values
#' @export uca
#'
#' @examples
#' \dontrun{
#' library(topographer)
#' crb <- readRDS(file.path('topographer', 'data', 'crb.rds'))
#' crb <- terra::unwrap(crb)
#' crb <- raster(crb)
#' y <- raster(crb)
#' values(y) <- values(crb)
#' x.uca <- uca(crb, calc.res=100, out.res=10, fill.sinks=T)
#' y.uca <- uca(y, calc.res=100, out.res=1000, fill.sinks=T)
#' plot(x.uca)
#' plot(y.uca)
#' }

uca <- function(x, calc.res=100, out.res=10, fill.sinks=T, deg=0.1) {
  if (!(inherits(x, "SpatRaster") | inherits(x, "RasterLayer")))
    stop(deparse(substitute(x)), " must be a SpatRaster or RasterLayer object")
  rl = F
  if (inherits(x, "RasterLayer")) {
    x = rast(x)
    rl = T
  }
  if (all(res(x) <rep(calc.res,2))) {
    x = change.res(x, targ.res=calc.res, method='median')
  } else if (all(res(x)>rep(calc.res,2))) {
    x = change.res(x, targ.res=calc.res, method='bilinear')
  }
  topmod = upslope.area(raster(x), atb=T, fill.sinks = fill.sinks, deg=deg)
  x = topmod[[1]]
  if (all(res(x)<rep(out.res,2))) {
    x = change.res(x, targ.res=out.res, method='median')
  } else if (all(res(x)>rep(out.res, 2))) {
    x = change.res(x, targ.res=out.res, method='bilinear')
  }
  
  if (!rl) x = rast(x)
  
  return(x)
}
