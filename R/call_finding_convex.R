#' @title Finding constant in modified option.
#' 
#' @description 
#' The call_finding_convex function is used to find constant L in modified call option.
#' 
#' @usage call_finding_convex(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...)
#' 
#' @param asset a numeric value of asset prices.
#' @param strike numeric value, strike price for call option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param p numeric value, power of the loss function, p > 1.
#' @param time a numeric value of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @param option_value numeric value, price of the modified option.
#' @param FUN the function to be applied, numeric method to find constant.
#' @param ... arguments to FUN.
#' @return A numeric vector, a constants for which the price of the modified option is equal to option_value.
#' 
#' @details If an error occurs: "Too large option_value or wrong initial values. Select other parameters." occur, try to use the function
#'  Bisection_method function with different a0 and b0 parameter.
#' 
#' @examples 
#' call_finding_convex(100, 100, 0, 0.3, 0.1, 2, 0, 1, 7, Bisection_method)
#' 
#' @export

call_finding_convex <- function(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...){
  if (p <= 1){
    stop("Wrong p argument. p > 1")
  }
  if( option_value > call_price(asset, strike, rate, vol, time, End_Time) ){
    stop("Wrong initial value of the option price")
  }
  
  else{
    m <- drift - rate
    k <- m / ( (p-1)*vol^2 )
    
    if (-k > 1 & m != 0){
      x <- (strike*k) / (k + 1)
      result1 <-  FUN(option_value = option_value, a0 = strike+1e-6, b0 = x, call_price_convex, asset, strike, rate, vol, drift, p, time, End_Time, ...)
      result2 <- call_Newton_convex(result1, strike, drift, rate, vol, p)[1]
      result <- c( min(result1, result2), max(result1, result2) )
    }
    else{
      result <-  FUN(option_value = option_value, a0 = strike, b0 = 1000*asset, call_price_convex, asset, strike, rate, vol, drift, p, time, End_Time, ...)
    }
  }
  return(result)
}