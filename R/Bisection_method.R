#' @title Bisection method
#'
#' @description
#' The Bisection_method function is used to find const L in modified call and put option.
#'
#' @usage Bisection_method(option_value, a0, b0, FUN, ..., epsilon = 1e-7)
#'
#' @param option_value numeric value, price of the modified option.
#' @param a0 numeric value, lower boundary of the range.
#' @param b0 numeric value, upper boundary of the range.
#' @param FUN the function to be applied, explicit formula for the option price.
#' @param ... arguments to FUN.
#' @param epsilon calculation error.
#' @return A numeric value, a constant for which the price of the modified option is equal to option_value.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Bisection_method}
#'
#' @examples
#' Bisection_method(5, 100, 500, call_price_linear, asset = 100, strike = 100, rate = 0, vol = 0.3, drift = 0.1, time = 0, End_Time = 1)
#'
#' @export

Bisection_method <- function(option_value, a0, b0, FUN, ..., epsilon = 1e-7){
  f_a <- FUN(L = a0, ...) - option_value
  f_b <- FUN(L = b0, ...) - option_value

  if(f_a*f_b > 0){
    stop( paste("Too large option_value or wrong initial values. Select other parameters.", "a0:", a0, "b0:", b0) )
  }
  n = 1
  a <- a0
  b <- b0
  c <- (a + b)/2
  f_c <- FUN(L = c, ...) - option_value

  n <- 1
  while( abs(f_c) > epsilon ){
    if(f_c*f_a < 0){
      b <- c
      f_b <- f_c
    }

    else{
      a <- c
      f_a <- f_c
    }
    n <- n + 1
    c <- (a + b)/2
    f_c <- FUN(L = c, ...) - option_value

  }

  return( c )
}
