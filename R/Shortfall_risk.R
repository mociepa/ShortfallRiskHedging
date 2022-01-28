#' @title Calculate Shortfall risk
#'
#' @description
#' The Shortfall risk function takes payoff from option, and their modife version and returns a mean of shortfall weighted by x^p function.
#' @usage Shortfall_risk(payoff, modificate_payoff, p, rate, End_Time = 1, is_discounted = FALSE)
#'
#' @param payoff a numeric vector of option payoff from trajectory simulations.
#' @param modificate_payoff a numeric vector of modife option payoff from trajectory simulations.
#' @param p numeric value, power of the loss function, p > 0.
#' @param rate numeric value, risk free rate in the model, rate >= 0.
#' @param End_Time numeric value, end time of the option.
#' @param is_discounted logical value, if TRUE, payoff and modificate_payoff will be discounted.
#'
#' @seealso
#'
#' @examples
#' Shortfall_risk(5, 4, 1, 0.05, 1)
#' Shortfall_risk(c(100, 120), c(100, 107.5), 3, 0.05, TRUE)
#'
#'
#'
#' @export

Shortfall_risk <- function(payoff, modificate_payoff, p, rate, End_Time = 1, is_discounted = FALSE){
  if( is_discounted ){
    payoff <- exp(-rate*End_Time)*payoff
    modificate_payoff <- exp(-rate*End_Time)*modificate_payoff
  }

  if (p == 1){
    result <- mean(payoff - modificate_payoff)
  }

  else if (p < 1){
    result <- mean( (payoff - modificate_payoff)^p )
  }

  else{
    result <- mean( (payoff - modificate_payoff)^p ) / p
  }
  return(result)
}
