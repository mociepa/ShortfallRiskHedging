#' @title Bisection method for Monte Carlo simulations
#'
#' @description
#' The Bisection_method_MC function is used to find const L in modified european option.
#'
#' @usage Bisection_method_MC(option_value, a0, b0, rate, X, FUN, ..., epsilon = 1e-3, time = 0, End_Time = 1)
#'
#' @param option_value numeric value, price of the modified option.
#' @param a0 numeric value, lower boundary of the range.
#' @param b0 numeric value, upper boundary of the range.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @param X numeric matrix, price of the asset from different trajectories.
#' @param FUN the function to be applied, explicit formula for the option price.
#' @param ... arguments to FUN.
#' @param time numeric value, actual time.
#' @param End_Time numeric value, end time of the option, time < End_Time.
#' @param epsilon calculation error.
#' @return A numeric value, a constant for which the price of the modified option is equal to option_value.
#'
#' @details This function estimated a constant for which price of the modified option is equal to option_value. If warning occurs means that
#' epsilon parameter is too small to find constant for which calculation error of option price is smaller then epsilon.
#' The reason for this is simulations. When the number of trajectories is increased, the epsilon parameter can be decreased.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Bisection_method}
#'
#' @examples
#' x <- generate_scenarios(100, 0, 0.3)
#' Bisection_method_MC(5, 1, 1000, 0, x, option_modificate_payoff, drift = 0.05, vol = 0.3, p = 1, call_payoff, strike = 100)
#'
#' @export

Bisection_method_MC <- function(option_value, a0, b0, rate, X, FUN, ..., epsilon = 1e-3, time = 0, End_Time = 1){
  payoff_a <- calculate_payoffs(paths = X, FUN, const = a0, rate = rate, ...)
  payoff_b <- calculate_payoffs(paths = X, FUN, const = b0, rate = rate, ...)
  f_a <- calculate_option_value(payoff_a, rate, time, End_Time) - option_value
  f_b <- calculate_option_value(payoff_b, rate, time, End_Time) - option_value

  if(f_a*f_b > 0){
    stop( paste("Too large option_value or wrong initial values. Select other parameters.", "a0:", a0, "b0:", b0) )
  }

  n = 1
  a <- a0
  b <- b0
  c <- (a + b)/2
  payoff_c <- calculate_payoffs(paths = X, FUN, const = c, rate = rate, ...)
  f_c <- calculate_option_value(payoff_c, rate, time, End_Time) - option_value
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
    payoff_c <- calculate_payoffs(paths = X, FUN, const = c, rate = rate, ...)
    f_c_previous <- f_c
    f_c <- calculate_option_value(payoff_c, rate, time, End_Time) - option_value
    if(f_c == f_c_previous & n > 100){
      warning(paste("Too small epsilon or too big b0 parameter, return constant with calculation error equal:", abs(f_c)))
      break
    }
    print(c(n, c, f_c))
  }

  return( c )
}
