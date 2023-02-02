#' Fold aspect about a given focal angle
#' 
#' @description Transforms simple cardinal aspect values into values folded along a selected axis
#' @param x A vector, dataframe, or raster of aspect values
#' @param fold Numeric. The angle along which aspect should be folded, default=180
#' @return A vector, dataframe, or raster of folded aspect values
#' @export
#' @examples
#' \dontrun{
#' library(terra)
#' crb <- readRDS(system.file(file.path('data', 'crb.rds'), package='topographer'))
#' a <- terra::terrain(crb, v='aspect')
#' f <- 225
#' f.aspect <- fold.aspect(a, f)
#' plot(f.aspect)
#' }

# Folded aspect
fold.aspect <- function(x, fold=180) abs(180-abs(x-fold))