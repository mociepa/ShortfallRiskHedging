#' @title Finding constant in modified option.
#' 
#' @description 
#' The put_finding_concave function is used to find constant L in modified put option.
#' 
#' @usage put_finding_concave(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...)
#' 
#' @param asset a numeric value of asset prices.
#' @param strike numeric value, strike price for put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param p numeric value, power of the loss function, p < 1.
#' @param time a numeric value of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @param option_value numeric value, price of the modified option.
#' @param FUN the function to be applied, numeric method to find constant.
#' @param ... arguments to FUN.
#' @return A numeric vector, a constants for which the price of the modified option is equal to option_value.
#' 
#' @details If an error occurs: "Too large option_value or wrong initial values. Select other parameters." occur, try to use the function
#'  Bisection_method function with different b0 parameter.
#' 
#' @examples 
#' put_finding_concave(100, 100, 0, 0.3, 0.1, 0.5, 0, 1, 2, Bisection_method)
#' 
#' @export

put_finding_concave <- function(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...){
  if (p <= 0 | p >= 1){
    stop("Wrong p argument. p is in the range (0, 1)")
  }
  
  if( option_value > put_price(asset, strike, rate, vol, time, End_Time) ){
    stop("Wrong initial value of the option price")
  }
  
  else{
    m <- drift - rate
    
    if(m < 0){
      x <- -m/vol^2*strike / (1 - p - m/vol^2)
      result1 <-  FUN(option_value = option_value, a0 = x, b0 = strike, put_price_concave, asset, strike, rate, vol, drift, p, time, End_Time, ...)
      result2 <- put_Newton_concave(result1, strike, drift, rate, vol, p)[1]
      result <- c( min(result1, result2), max(result1, result2))
    }
    
    else{
      result <-  FUN(option_value = option_value, a0 = 0, b0 = strike, put_price_concave, asset, strike, rate, vol, drift, p, time, End_Time, ...)
    }
  }
  return(result)
}