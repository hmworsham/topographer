#' Change resolution of a raster
#'
#' @description Changes the resolution of a raster through dis-/aggregation
#' @param x A terra SpatRaster or raster RasterLayer object
#' @param targ.res Positive integer. Desired resolution of output raster. Single
#'                 value asserts equal horz/vert length. Or two integers for
#'                 different horz/vert lengths.
#' @param method method for dis-/aggregating. one of c('mean', 'max', 'min',
#'               'median', 'sum', 'modal') for aggregation or one of c('near',
#'               'bilinear') for disaggregation
#' @return A raster of the resolution stated in target.res
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rast()
#' values(x) <- 1:ncell(x)
#' y <- raster(x)
#' x <- changeres(x, 20, method='median')
#' y <- changeres(y, 20, method='median')
#' x <- changeres(x, 1, method='bilinear')
#' y <- changeres(y, 1, method='bilinear')
#' }

change.res <- function(x, targ.res=NULL,
                      method=c('mean', 'max', 'min', 'median', 'sum', 
                               'modal', 'near', 'bilinear')) {
  
  if (!(inherits(x, "SpatRaster") | inherits(x, "RasterLayer")))
    stop(deparse(substitute(x)), " must be a SpatRaster or RasterLayer object")
  
  rl = F
  if (inherits(x, "RasterLayer")) {
    x = rast(x)
    rl = T
    }
  
  if (length(targ.res==1)) targ.res=rep(targ.res, 2)
  
  if(all(targ.res>res(x))) {
    match.arg(method, c('mean', 'max', 'min', 
                        'median', 'sum', 'modal'))
    fact = targ.res/res(x)
    x = aggregate(x, fact, method)
    } else {
    match.arg(method, c('near', 'bilinear'))
    fact = res(x)/targ.res
    x = disagg(x, fact, method)
    }
  
  if (rl) x = raster(x)
  
  return(x)
}
