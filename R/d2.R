#' @title d2
#' 
#' @description 
#' The d2 function is used in an analytical approach to calculate standard call and put options and their modifications.
#' 
#' @usage d2(asset, strike, rate, vol, time, End_Time)
#' 
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param time a numeric vector of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @return A numeric vector, which help calculate option value.
#' 
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#' 
#' @examples 
#' d2(100, 100, 0, 0.5, 0, 1)
#' d2(c(100, 120), 100, 0, 0.3, 0, 1)
#' d2(c(100, 120), 100, 0, 0.3, c(0, 0.5), 1)
#' 
#' 
#' 
#' @export

d2 <- function(asset, strike, rate, vol, time, End_Time){
  tau <- End_Time - time
  return( d1(asset, strike, rate, vol, time, End_Time) - vol*sqrt(tau) )
}