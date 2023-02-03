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

zonals <- function(zones, rasters, type=c('coord', 'sf'), radius, shape='rectangle') {

  # If it's a coordinate or list of coordinates...
  if(type == 'coord'){
    xy = zones[,c(2,3)]
    xy = st_as_sf(xy, coords=c('Longitude', 'Latitude'), crs=4326)
    sites = st_transform(xy, crs(rasters[[1]]))

    # Create polygons with polys
    ID = zones[,1]
    sites = st_buffer(sites, dist=radius, endCapStyle = 'SQUARE', joinStyle = 'MITRE')
    sites = vect(sites)
    site_names = ID
  }

  # If it's a shapefile...
  else if(type == 'sf'){
    sites = vect(zones)
    sites = terra::project(sites, crs(rasters[[1]]))
    site_names = sites$PLOT_ID
  }

  # Plot the coordinates on top of the DEM
  #plot(rasters[[2]])
  #plot(sites, color='blue', size=20, add = T)

  # Extract values from specified factors
  topovals = lapply(rasters, terra::extract, sites, fun = mean)

  # Return the values
  topovals.df = do.call('cbind', (lapply(topovals, '[', 2)))
  colnames(topovals.df) <- sapply(rasters, names)
  topovals.df$Location_ID <- site_names
  topovals.df <- topovals.df[c(length(topovals.df), 2:length(topovals.df)-1)]

  return(topovals.df)
}

