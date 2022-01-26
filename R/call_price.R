#' @title Calculate european call option
#' 
#' @description 
#' The call_price function takes parameters from Black-Scholes model and returns a price of european call option.
#' 
#' @usage call_price(asset, strike, rate, vol, time, End_Time)
#' 
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param time a numeric vector of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @return A numeric vector, price of the european call option.
#' 
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#' 
#' @examples 
#' call_price(100, 100, 0, 0.5, 0, 1)
#' call_price(c(100, 120), 100, 0, 0.3, 0, 1)
#' call_price(c(100, 120), 100, 0, 0.3, c(0, 0.5), 1)
#' 
#' 
#' 
#' @export


call_price <- function(asset, strike, rate, vol, time, End_Time){
  tau <- End_Time - time
  return( asset*pnorm( d1(asset, strike, rate, vol, time, End_Time) ) - 
            strike*exp(-rate*tau)*pnorm( d2(asset, strike, rate, vol, time, End_Time) ) )
}
