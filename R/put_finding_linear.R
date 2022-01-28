#' @title Finding constant in modified option.
#'
#' @description
#' The put_finding_linear function is used to find constant L in modified put option.
#'
#' @usage put_finding_linear(asset, strike, rate, vol, drift, time, End_Time, option_value, FUN, ...)
#'
#' @param asset a numeric value of asset prices.
#' @param strike numeric value, strike price for put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric value of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @param option_value numeric value, price of the modified option.
#' @param FUN the function to be applied, numeric method to find constant.
#' @param ... arguments to FUN.
#' @return A numeric value, a constant for which the price of the modified option is equal to option_value.
#'
#'
#' @examples
#' put_finding_linear(100, 100, 0, 0.3, 0.1, 0, 1, 2, Bisection_method)
#'
#' @export

put_finding_linear <- function(asset, strike, rate, vol, drift, time, End_Time, option_value, FUN, ...){
  m <- drift - rate

  if( option_value > put_price(asset, strike, rate, vol, time, End_Time) ){
    result <- stop("Wrong initial value of the option price")
  }

  else{
    if(m == 0){
      result <-  c(0, option_value/put_price(asset, strike, rate, vol, time, End_Time))
    }
    else{
      result <-  FUN(option_value = option_value, a0 = 0, b0 = strike, put_price_linear, asset, strike, rate, vol, drift, time, End_Time, ...)
    }
  }
  return(result)
}
