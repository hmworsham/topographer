#' Heat load
#'
#' @description Calculates heat load from input slope and aspect values
#' @param L Float value for latitude of site
#' @param S Vector, dataframe or raster of slope values in degrees or radians
#' @param A Vector, dataframe, or raster of aspect values in degrees or radians
#' @param unit Character vector. Units that aspect are in, either degrees or radians
#' @param fold Numeric. Angle along which aspect should be folded
#' @param equation Integer vector. The equation number from McCune and Keon 2002. Default is 3.
#'   There three equations have slightly different uses. *Eq.1* is broadest in application,
#'   covering slopes <=90º at latitudes 0-60º N, but has the lowest R^2 when compared with
#'   instrumental radiation observations. *Eq. 2.* has higher agreement but excludes slopes >60º.
#'   *Eq. 3* holds between latitudes of ±30-60º and slope angles <= 60º and has highest R^2.
#' @return total heat load value
#' @note
#' Heat load is a unitless index of heat load, rather than a direct estimate of
#'   incident radiation. Their 3rd equation is valid for the narrowest input ranges
#'   but has the highest R^2 and is suitable for many settings. The estimates are somewhat
#'   rudimentary and do not take into account cloud cover, regional differences in the atmospheric
#'   coefficient, or shading by adjacent topography.
#' @references
#' McCune, B. and D. Keon (2002) Equations for potential annual direct incident
#'   radiation and heat load. Journal of Vegetation Science 13:603-606.
#' @export
#' @examples
#' \dontrun{
#' # Vector example
#' l <- 38.58
#' a <- 0:360
#' s <- 12.6
#' heatload <- thl(l, a, s, 'deg', 180)
#' plot(a, thl(l, a, s, fold=180, equation=2))
#' points(a,thl(l, a, s, fold=225, equation=1))
#'
#' # Raster example
#' #' library(terra)
#' crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
#' l <- 38.58
#' a <- terra::terrain(crb, v='aspect')
#' s <- terra::terrain(crb, v='slope')
#' heatload <- thl(l, a, s, 'deg', fold=205, equation=3)
#' plot(heatload)
#' }

# Total potential heat load
thl <- function(L, A, S, unit=c('deg', 'rad'), fold=180, equation=c(1,2,3)) {
  #d2r <- function(x) 2*pi*x/360
  #r2d <- function(x) 360*x/(2*pi)
  if (unit=='deg') {
    L <- d2r(L)
    A <- fold.aspect(A,fold)
    A <- d2r(A)
    S <- d2r(S)
    A[S==0] <- 0
  } else {
    atmp <- r2d(A)
    atmp <- fold.aspect(atmp,fold)
    A <- d2r(atmp)
  }

  # Equations, McCune and Keon 2002 JVS
  switch(equation,
         E1={return(exp(-1.467 +1.582*cos(L)*cos(S)-1.500*cos(A)*sin(S)*sin(L)-0.262*sin(L)*sin(S)+0.607*sin(A)*sin(S)))},
         E2={return(exp(-1.236 +1.350*cos(L)*cos(S)-1.376*cos(A)*sin(S)*sin(L)-0.331*sin(L)*sin(S)+0.375*sin(A)*sin(S)))},
         E3={return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))}
  )
}


# TODO: limitation and error handling in 3 equations

# .mk.eq1 <- function(L, A, S) {
#   if (any(values(slope)>1.570796, na.rm=T)) {
#     warning('Warning: Slope data contains values greater than 90º. Results may be invalid for these areas.')
#   }
#   return(exp(-1.467 +1.582*cos(L)*cos(S)-1.500*cos(A)*sin(S)*sin(L)-0.262*sin(L)*sin(S)+0.607*sin(A)*sin(S)))
# }
#
# .mk.eq2 <- function(L, A, S) {
#   if (any(values(slope)>1.570796, na.rm=T)) {
#     stop('Error: Slope must be <=60 for Equation 2.')
#   } else {
#     return(exp(-1.236 +1.350*cos(L)*cos(S)-1.376*cos(A)*sin(S)*sin(L)-0.331*sin(L)*sin(S)+0.375*sin(A)*sin(S)))
#   }
# }
#
# .mk.eq3 <- function(L, A, S) {
#   if (any(values(slope)>1.570796, na.rm=T)) {
#     stop('Error: Slope must be <=60 for Equation 3.')
#   } else if ((L<30) | (L>60)) {
#     stop('Error: Latitude must be >=30 and <=60 for Equation 3.')
#   } else {
#     return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))
#   }
# }



