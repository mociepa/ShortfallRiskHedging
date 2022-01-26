#' @title Finding constant in modified option.
#' 
#' @description 
#' The put_finding_modified function is used to find constant L in modified put option.
#' 
#' @usage put_finding_modified(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...)
#' 
#' @param asset a numeric value of asset prices.
#' @param strike numeric value, strike price for put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param p numeric value, power of the loss function, p > 0.
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
#' put_finding_modified(100, 100, 0, 0.3, 0.1, 2, 0, 1, 2, Bisection_method)
#' put_finding_modified(100, 100, 0, 0.3, 0.1, 1, 0, 1, 2.5, Bisection_method)
#' 
#' @export

put_finding_modified <- function(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...){
  if (p <= 0){
    stop("Wrong p argument. p > 0")
  }
  
  else if (p == 1){
    result <- put_finding_linear(asset, strike, rate, vol, drift, time, End_Time, option_value, FUN, ...)
  }
  
  else if (p < 1){
    result <- put_finding_concave(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...)
  }
  
  else{
    result <- put_finding_convex(asset, strike, rate, vol, drift, p, time, End_Time, option_value, FUN, ...)
  }
  return(result)
}