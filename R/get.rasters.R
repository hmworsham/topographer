#' Source rasters from local data directory
#'
#' @description
#' Sources rasters from a local directory using terra::rast and stores in a list of SpatRaster objects.
#' @importFrom terra rast
#' @param x a string containing a search pattern for a subdirectory name indicating the 
#' raster data of interest
#' @param dir a string containing a directory path where rasters are stored
#' @return a list of SpatRaster objects for serial processing
#' @export
#'
#' @examples
#' \dontrun{
#' rasdir <- './data'
#' ras.ls <- get.rasters('raster', rasdir)
#' }

get.rasters = function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, rast)
  return(xras)
}