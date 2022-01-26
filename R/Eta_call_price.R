#' @title Delta hedging for european call option
#' 
#' @description 
#' The Eta_call_price function takes parameters from Black-Scholes model and returns a number of stock needed to fully hedge european call option.
#' 
#' @usage Eta_call_price(asset, strike, rate, vol, time, End_Time)
#' 
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param time a numeric vector of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @return A numeric vector, amount of money needed to hedge an european call option.
#' 
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#' 
#' @examples 
#' Eta_call_price(100, 100, 0, 0.5, 0, 1)
#' Eta_call_price(c(100, 120), 100, 0, 0.3, 0, 1)
#' Eta_call_price(c(100, 120), 100, 0, 0.3, c(0, 0.5), 1)
#' 
#' 
#' @export

Eta_call_price <- function(asset, strike, rate, vol, time, End_Time){
  return( -strike*exp(-rate*End_Time) * pnorm( d2(asset, strike, rate, vol, time, End_Time)))
}