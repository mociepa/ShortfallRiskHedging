#' @title Calculate modified european call option
#'
#' @description
#' The call_price_explicit function takes parameters from Black-Scholes model and returns a price of modified european call option.
#'
#' @usage call_price_explicit(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
#'
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param p numeric value, power of the loss function, p > 1.
#' @param End_Time end time of the option, End_time >= time.
#' @param L numeric value, determines option payoff, L > 0.
#' @param L2 numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.
#' @return A numeric vector, price of the modification of european call option using x^p loss function.
#'
#'
#' @examples
#' call_price_explicit(100, 100, 0, 0.5, 0.05, 2,  0, 1, 105)
#' call_price_explicit(c(100, 120), 100, 0, 0.3, 0.05, 2, 0, 1, 105)
#' call_price_explicit(c(100, 120), 100, 0, 0.3, 0.05, 2, c(0, 0.5), 1, 105)
#'
#'
#'
#' @export

call_price_explicit <- function(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA){
  if (p <= 0){
    stop("Wrong p argument. p > 0")
  }

  else if (p == 1){
    result <- call_price_linear(asset, strike, rate, vol, drift, time, End_Time, L)
  }

  else if (p < 1){
    result <- call_price_concave(asset, strike, rate, vol, drift, p, time, End_Time, L, L2)
  }

  else{
    result <- call_price_convex(asset, strike, rate, vol, drift, p, time, End_Time, L, L2)
  }

  return(result)
}
