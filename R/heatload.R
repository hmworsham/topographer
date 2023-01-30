#' Heat load
#'
#' @description calculates heat load from input slope and aspect values
#' @param L float value for latitude of site 
#' @param S vector, dataframe or raster of slope values
#' @param A vector, dataframe, or raster of aspect values
#' @param unit units that aspect are in, either degrees or radians
#' @param fold angle along which aspect should be folded
#' @return total heat load value 
#' 
#' @note 
#' Heat load is a unitless index of heat load, rather than a direct estimate of 
#' incident radiation. The approach implements Equation 3 from McCune and Keon (2002),
#' using inputs of latitude. The equation holds between latitudes of ±30-60º and 
#' slope angles <= 60º. Their 3rd equation is valid for the narrowest input ranges 
#' but has the highest R^2 and is suitable for many settings. I may implement
#' equations 1 and 2 in the future. NOTE: the estimates are somewhat rudimentary 
#' and do not take into account cloud cover, regional differences in the atmospheric 
#' coefficient, or shading by adjacent topography.
#' 
#' @references
#' McCune, B. and D. Keon (2002) Equations for potential annual direct incident
#'   radiation and heat load. Journal of Vegetation Science 13:603-606.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' l <- 38.58
#' a <- 0:360
#' s <- 12.6
#' heatload <- thl(l, a, s, 'deg', 180)
#' plot(A,thl(L,A,S,fold=225))
#' points(A,thl(L,A,S,fold=225))
#' }

# Total potential heat load
thl <- function(L, A, S, unit='deg', fold=180) {
  d2r <- function(x) 2*pi*x/360
  r2d <- function(x) 360*x/(2*pi)
  if (unit=='deg') {
    A <- fold.aspect(A,fold)
    L <- d2r(L)
    A <- d2r(A)
    S <- d2r(S)
  } else {
    atmp <- r2d(A)
    atmp <- fold.aspect(atmp,fold)
    A <- d2r(atmp)
  }
  # EQN 3, McCune and Keon 2002 JVS, latitude > 30‚ slope < 60°
  return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))
}

