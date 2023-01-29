#' Transform aspect
#'
#' @description transforms simple cardinal aspect values into ecologically meaningful values
#' @param slope vector, dataframe or raster of slope values
#' @param aspect vector, dataframe, or raster of aspect values
#' @param focal focal angle for facingness
#' @param unit units that aspect are in, either degrees or radians
#' @param fold angle along which aspect should be folded
#' @return transformed aspect values
#' @export facing
#' @export eastness
#' @export northness
#' @export southness
#' @export adjsouthness
#' @export fold.aspect
#' @export cos.aspect
#' @export sin.aspect
#'
#' @examples
#' \dontrun{
#' s <- 23
#' a <- 1:360
#' f <- 180
#' u <- 'deg'
#' e.facing <- eastness(s,a,u)
#' adjs.facing <- adjsouthness(s,a,f,u)
#' f.aspect <- fold.aspect(a, fold=215)
#' s.aspect <- sin.aspect(a)
#' c.aspect <- cos.aspect(a)
#' plot(a, type='l')
#' lines(a, e.facing, col='red')
#' lines(a, f.aspect, col='blue')
#' lines(a, s.aspect, col='orange')
#' lines(a, c.aspect, col='purple')
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
eastness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=0,unit=unit)
southness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=180,unit=unit)
adjsouthness <- function(slope, aspect, focal, unit='deg') facing(slope,aspect,focal,unit=unit)

# Folded aspect
fold.aspect <- function(x, fold=180) abs(180-abs(x-f))

# Cosine transform aspect
cos.aspect <- function(x) {
  cos.asp <- cos(x)
  names(cos.asp) <- 'usgs_cosaspect_100m'
  return(cos.asp)
}

# Sine transform aspect
sin.aspect <- function(x) {
  sin.asp <- sin(x)
  names(sin.asp) <- 'usgs_cosaspect_100m'
  return(sin.asp)
}
