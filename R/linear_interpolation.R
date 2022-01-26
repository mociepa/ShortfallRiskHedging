#' @title Linear interpolation
#'
#' @description
#' The linear interpolation function, interpolate value of the function at point x.
#' @usage linear_interpolation(x, x1, x2, y1, y2)
#'
#' @param x numeric value, argument in the searched function value.
#' @param x1 numeric value, argument of the function x1 <= x.
#' @param x2 numeric value, actual argument of the function x <= x2.
#' @param y1 numeric value, the value of the function at the point x1.
#' @param y2 numeric value, the value of the function at the point x2.
#' @return A numeric value, interpolated value of the function at the point x.
#'
#' @examples
#' linear_interpolation(3, 2, 3.5, 5, 7)
#'
#' @export

linear_interpolation <- function(x, x1, x2, y1, y2){
  if (x1 > x | x2 < x){
    stop("wrong arguments")
  }
  a <- (y1 - y2) / (x1 - x2)
  return( y1 + (x - x1)*a )
}
