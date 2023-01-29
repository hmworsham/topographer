#' Topographic Wetness Index (TWI)
#'
#' @description calculates Topographic Wetness Index on a raster DEM
#' @importFrom dynatopmodel build_layers
#' @param x A terra SpatRaster or raster RasterLayer object
#' @param in.res Positive integer. Resolution for computing upslope area 
#'  and localslope.  Single value asserts equal horz/vert length. 
#'  Or two integers for different horz/vert lengths. 
#' @param out.res Positive integer. Desired resolution of output raster. Single
#' value asserts equal horz/vert length. Or two integers for
#' different horz/vert lengths. 
#' @param fill_sinks Logical. If TRUE (default) then run a sinkfill before 
#'  calculating the upslope area and TWI.
#' @param deg Threshold intercell slope to determine sinks
#' @return A raster comprising topographic wetness index values
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rast()
#' values(x) <- 1:ncell(x)
#' y <- raster(x)
#' x.twi <- twi(x, in.res=100, out.res=10, fill.sinks=T)
#' y.twi <- twi(y, in.res=100, out.res=1000, fill.sinks=T)
#' plot(x.twi)
#' }

twi <- function(x, in.res=100, out.res=10, fill.sinks=T, deg=0.1) {
  if (!(inherits(x, "SpatRaster") | inherits(x, "RasterLayer")))
    stop(deparse(substitute(x)), " must be a SpatRaster or RasterLayer object")
  rl = F
  if (inherits(x, "RasterLayer")) {
    x = rast(x)
    rl = T
  }
  if (all(res(x) <rep(in.res,2))) {
    x = change.res(x, targ.res=in.res, method='median')
  } else if (all(res(x)>rep(in.res,2))) {
    x = change.res(x, targ.res=in.res, method='bilinear')
  }
  topmod = dynatopmodel::build_layers(raster(x), fill.sinks = T, deg=0.1)
  x = topmod[[3]]
  if (all(res(x)<rep(out.res,2))) {
    x = change.res(x, targ.res=out.res, method='median')
  } else if (all(res(x)>rep(out.res, 2))) {
    x = change.res(x, targ.res=out.res, method='bilinear')
  }
  
  if (!rl) x = rast(x)
  
  return(x)
}