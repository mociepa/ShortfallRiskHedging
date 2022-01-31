#' @title Delta hedging for modified european call option
#'
#' @description
#' The Xi_call_price_concave function takes parameters from Black-Scholes model and returns a number of stock needed to fully hedge modified european call option.
#'
#' @usage Xi_call_price_concave(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
#'
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param p numeric positive value, power of the loss function, p < 1.
#' @param End_Time end time of the option, End_time >= time.
#' @param L numeric value, determines option payoff, L > 0.
#' @param L2 numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.
#' @return A numeric vector, number of asset to hedge modification of european call option using concave loss function.
#'
#'
#' @examples
#' Xi_call_price_concave(100, 100, 0, 0.5, 0.05, 0.5,  0, 1, 105)
#' Xi_call_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, 0, 1, 105)
#' Xi_call_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, c(0, 0.5), 1, 105)
#'
#'
#'
#' @export

Xi_call_price_concave <- function(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA){
  m = drift - rate
  tau = End_Time - time
  if (length(tau) == 1){
    tau <- rep(tau, length(asset))
  }

  if ( m/vol^2 <= 1 - p ){
    result1 <- Xi_call_price(asset, strike, rate, vol, time, End_Time) - Xi_call_price(asset, L, rate, vol, time, End_Time)
    result2 <- Xi_call_price(asset, strike, rate, vol, time, End_Time) - Xi_call_price(asset, L, rate, vol, time, End_Time) +
      (strike - L) / (L*vol*sqrt(tau)) * dnorm( d1(asset, L, rate, vol, time, End_Time) )

    result <- ifelse(tau == 0, result1, result2)
  }

  else{
    if ( is.na(L2) == TRUE ){
      warning("In this case L2 is needed, while parameter L2 = NA. Calculated L2 argument by the Newton method (call_Newton_concave function).")
      L_bis <- call_Newton_concave(L, strike, drift, rate, vol, p)[1]
      L1 <- min(L, L_bis)
      L2 <- max(L, L_bis)
    }
    else{
      L1 <- min(L, L2)
      L2 <- max(L, L2)
    }
    result1 <- Xi_call_price(asset, strike, rate, vol, time, End_Time) - Xi_call_price(asset, L1, rate, vol, time, End_Time) + Xi_call_price(asset, L2, rate, vol, time, End_Time)
    result2 <- Xi_call_price(asset, strike, rate, vol, time, End_Time) - Xi_call_price(asset, L1, rate, vol, time, End_Time) +
      (strike - L1)/(L1*vol*sqrt(tau))* dnorm( d1(asset, L1, rate, vol, time, End_Time) ) +
      Xi_call_price(asset, L2, rate, vol, time, End_Time) + (L2 - strike)/(L2*vol*sqrt(tau))* dnorm( d1(asset, L2, rate, vol, time, End_Time) )

    result <- ifelse(tau == 0, result1, result2)
  }

  return(result)
}
