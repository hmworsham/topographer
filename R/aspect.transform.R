#' Transform aspect
#'
#' @description Transforms simple cardinal aspect values into ecologically meaningful values
#' @param slope A vector, dataframe or raster of slope values
#' @param aspect A vector, dataframe, or raster of aspect values
#' @param focal Integer. A focal angle for transforming aspect into a 'facing' value
#' @param unit Character. Units that aspect are in natively, either degrees or radians
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
#' crb <- readRDS(file.path('data', 'crb.rds'))
#' s <- terra::terrain(crb, v='slope')
#' a <- terra::terrain(crb, v='aspect')
#' f <- 180
#' u <- 'deg'
#' e.facing <- eastness(s,a,u)
#' adjs.facing <- adjsouthness(slope=s,aspect=a,focal=f,unit=u)
#' f.aspect <- fold.aspect(a, fold=215)
#' s.aspect <- sin.aspect(a)
#' c.aspect <- cos.aspect(a)
#' plot(a)
#' plot(e.facing)
#' plot(f.aspect)
#' }

# Which direction a slope is facing
facing <- function(slope, aspect, focal=180, unit='rad') {
  if (unit %in% c('rad','deg')) {
    deg = F
    if (unit=='deg') {
      slope = d2r(slope)
      aspect = d2r(aspect)
      deg = T
    }
    aspect = d2r(focal) - aspect
    return(sin(slope) * cos(aspect))
  } else print('unit must be rad or deg')
}

# Eastness, northness, southness
eastness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=0,unit=unit)
southness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=180,unit=unit)
adjsouthness <- function(slope, aspect, focal, unit='deg') facing(slope,aspect,focal,unit=unit)

# Folded aspect
fold.aspect <- function(x, fold=180) abs(180-abs(x-fold))

# Cosine transform aspect
cos.aspect <- function(x, unit=c('rad','deg')) {
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

# Sine transform aspect
sin.aspect <- function(x, unit=c('rad', 'deg')) {
  match.arg(unit)
  deg=F
  if(unit=='deg') {
    x = d2r(x)
    deg = T
  }
  sin.asp <- sin(x)
  names(sin.asp) <- 'usgs_cosaspect_100m'
  if (deg) {return(r2d(sin.asp))
  } else {return(sin.asp)}
}
