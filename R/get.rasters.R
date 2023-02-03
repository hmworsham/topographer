#' Ingest rasters from data directory
#'
#' @description Sources rasters from a local directory using terra::rast and
#'   stores in a list of SpatRaster objects.
#' @importFrom terra rast
#' @param x Character vector. Search pattern for the raster data of interest
#' @param path Character vector. Directory path where rasters are stored
#' @return a list of SpatRaster objects for serial processing
#' @export
#'
#' @examples
#' \dontrun{
#' rasdir <- './data'
#' ras.ls <- get.rasters('raster', rasdir)
#' }

get.rasters = function(x, path){
  list.files(path,
             paste0('*', x, '.tif', collapse='|'),
             full.names=T)
  xras = lapply(xtif, rast)
  return(xras)
}
