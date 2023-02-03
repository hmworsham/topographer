#' Source rasters from local data directory
#'
#' @description calculates zonal statistics for raster inputs given an input shapefile or
#'   coordinates
#' @import sf
#' @importFrom terra rast
#' @importFrom terra extract
#' @param input a shapefile or dataframe of coordinates
#' @param ras.source a file path to raster source directory
#' @param topo.inputs a vector of strings describing raster data of interest
#' @param type if 'coord' will calculate for a coordinate and specified buffer radius; if
#' 'sf' will calculate for shapefile
#' @param radius radius of buffer for 'coord' inputs
#' @param shape shape of buffer, defaults to rectangle
#' @return a list of SpatRaster objects for serial processing
#' @export
#' @examples
#' \dontrun{
#' TODO
#' }

zonals <- function(input, ras.source, topo.inputs, type=c('coord', 'sf'), radius, shape='rectangle') {

  toporasters = flatten(lapply(topo.inputs, get.rasters, ras.source))

  # If it's a coordinate or list of coordinates...
  if(type == 'coord'){
    xy = input[,c(2,3)]
    xy = st_as_sf(xy, coords=c('Longitude', 'Latitude'), crs=4326)
    sites = st_transform(xy, crs(toporasters[[1]]))

    # Create polygons with polys
    ID = input$Site_ID
    sites = st_buffer(sites, dist=radius, endCapStyle = 'SQUARE', joinStyle = 'MITRE')
    sites = vect(sites)
    site_names = input$Site_ID
  }

  # If it's a shapefile...
  else if(type == 'sf'){
    sites = vect(input)
    sites = terra::project(sites, crs(toporasters[[1]]))
    site_names = sites$PLOT_ID
  }

  # Plot the coordinates on top of the DEM
  #plot(toporasters[[2]])
  #plot(sites, color='blue', size=20, add = T)

  # Extract values from specified factors
  topovals = lapply(toporasters, terra::extract, sites, fun = mean)

  # Return the values
  topovals.df = do.call('cbind', (lapply(topovals, '[', 2)))
  colnames(topovals.df) <- sapply(toporasters, names)
  rownames(topovals.df) <- site_names

  return(topovals.df)
}
