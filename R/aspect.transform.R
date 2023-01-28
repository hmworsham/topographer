#' Transform aspect
#'
#' @description transforms simple cardinal aspect values into ecologically meaningful values
#' @param slope vector, dataframe or raster of slope values
#' @param aspect vector, dataframe, or raster of aspect values
#' @param focal focal angle for facingness
#' @param unit units that aspect are in, either degrees or radians
#' @param fold angle along which aspect should be folded
#' @return transformed aspect values
#' @export
#'
#' @examples
#' \dontrun{
#' d <- 25.5
#' r <- d2r(d)
#' d <- r2d(r)
#' }


# Which direction a slope is facing
facing <- function(slope, aspect, focal=180, unit='rad') {
  if (unit %in% c('rad','deg')) {
    if (unit=='deg') {
      slope <- d2r(slope)
      aspect <- d2r(aspect)
    }
    aspect <- d2r(focal) - aspect
    return(sin(slope) * cos(aspect))
  } else print('unit must be rad or deg')
}

# Eastness, northness, southness
eastness <- function(slope,aspect, unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope,aspect, unit='deg') facing(slope,aspect,focal=0,unit=unit)
southness <- function(slope,aspect, unit='deg') facing(slope,aspect,focal=180,unit=unit)
adjsouthness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=205, unit=unit)

# Folded aspect
foldaspect <- function(x, f=fold) abs(180-abs(x-f))
