#' @title Finding const value
#'
#' @description
#' The call_const function computes a constant from the overall model.
#'
#' @usage call_const(L, strike, drift, rate, vol, p)
#'
#' @param L numeric value, const value from the formulas for the modified call options.
#' @param strike numeric value, strike of the call option.
#' @param drift numeric value, drift of the asset in the model.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param p, numeric value, power of the loss function.
#' @return A numeric value, const from the overall model.
#'
#' @examples
#' call_const(110, 100, 0.1, 0, 0.3, 1)
#' call_const(110, 100, 0.1, 0, 0.3, 2)
#'
#' @export

call_const <- function(L, strike, drift, rate, vol, p){
  if (L < strike){
    warning("L argument should be bigger then strike price.")
  }
  if (p <= 0){
    stop("Wrong p argument, p > 0.")
  }
  m <- drift - rate
  if (p == 1){
    result <- L
  }
  else if(p > 1){
    k <- m / ( vol^2*(p - 1) )

    result <- (L - strike)*L^k
  }

  else if(p < 1){
   result <- (L - strike)^(1 - p) * L^(-m/vol^2)
  }
  return(result)
}
