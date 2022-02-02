#' @title Delta hedging for modified european put option
#'
#' @description
#' The Xi_put_price_linear function takes parameters from Black-Scholes model and returns a number of stock needed to fully hedge modified european put option.
#'
#' @usage Xi_put_price_linear(asset, strike, rate, vol, drift, time, End_Time, L)
#'
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @param L a numeric value, determines where option payoff is zero, see details, L > 0.
#' @return A numeric vector, number of asset to hedge modification of european put option using linear loss function.
#'
#' @details Payoff of this modified call option is:
#' ## \eqn{ 1(asset > L)(asset - strike)^+ }, when \eqn{ drift > rate }.
#' ## \eqn{ 1(asset < L)(asset - strike)^+ }, when \eqn{ drift < rate }.
#' ## \eqn{ L(asset - strike)^+ }, when \eqn{ drift == rate }, of course in this case L <= 1.
#'
#'
#' @examples
#' Xi_put_price_linear(100, 100, 0, 0.5, 0.05, 0, 1, 70)
#' Xi_put_price_linear(c(100, 120), 100, 0, 0.3, 0.05, 0, 1, 40)
#' Xi_put_price_linear(c(100, 120), 100, 0, 0.3, 0.05, c(0, 0.5), 1, 20)
#'
#'
#'
#' @export

Xi_put_price_linear <- function(asset, strike, rate, vol, drift, time, End_Time, L){
  m = drift - rate
  tau = End_Time - time

  if (length(tau) == 1){
    tau <- rep(tau, length(asset))
  }

  if (m > 0){
    result1 <- Xi_put_price(asset, strike, rate, vol, time, End_Time) - Xi_put_price(asset, L, rate, vol, time, End_Time)
    result2 <- Xi_put_price(asset, strike, rate, vol, time, End_Time) - Xi_put_price(asset, L, rate, vol, time, End_Time) - (L - strike)/(L*vol*sqrt(tau))*dnorm( d1(asset, L, rate, vol, time, End_Time))

    result <- ifelse(tau == 0, result1, result2)
  }

  else if (m < 0){
    result <- ifelse(tau == 0, Xi_put_price(asset, L, rate, vol, time, End_Time), Xi_put_price(asset, L, rate, vol, time, End_Time) - (strike - L)/(L*vol*sqrt(tau))*dnorm( d1(asset, L, rate, vol, time, End_Time) ))
  }

  else{
    if(L > 1){
      warning( "The drift is equal to the risk free rate. In this case L parameter should be in range (0, 1)." )
    }
    result <- L*Xi_put_price(asset, strike, r, vol, time, End_Time)
  }

  return(result)
}
