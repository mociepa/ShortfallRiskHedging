#' @title Delta hedging for modified european put option
#'
#' @description
#' The Xi_put_price_convex function takes parameters from Black-Scholes model and returns a number of stock needed to fully hedge modified european put option.
#'
#' @usage Xi_put_price_convex(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
#'
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param p numeric value, power of the loss function, p > 1.
#' @param End_Time end time of the option, End_time >= time.
#' @param L numeric value, determines option payoff, see details, L > 0.
#' @param L2 numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.
#' @return A numeric vector, number of asset to hedge modification of european put option using convex loss function.
#'
#' @details Payoff of this modified call option is:
#' ## \eqn{ 1(asset > L)(asset - strike)^+ }, when \eqn{ drift > rate }.
#' ## \eqn{ 1(asset < L)(asset - strike)^+ }, when \eqn{ drift < rate }.
#' ## \eqn{ L(asset - strike)^+ }, when \eqn{ drift == rate }, of course in this case L <= 1.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#'
#' @examples
#' put_price_convex(100, 100, 0, 0.5, 0.05, 2, 0, 1, 105)
#' put_price_convex(c(100, 120), 100, 0, 0.3, 0.05, 2, 0, 1, 105)
#' put_price_convex(c(100, 120), 100, 0, 0.3, 0.05, 2, c(0, 0.5), 1, 105)
#'
#'
#'
#' @export

Xi_put_price_convex <- function(asset, strike, rate, vol, drift, p, time, End_Time, L1, L2 = NA) {
  m = drift - rate
  k = m/(vol^2*(p-1))
  l = 0.5*vol^2 - rate + 0.5*m/(p-1)
  tau = End_Time - time

  if (length(tau) == 1){
    tau <- rep(tau, length(asset))
  }

  if (-k > 0){
    L <- L1

    result1 <- - pnorm( -d1(asset, L, rate, vol, time, End_Time) ) -
      k*(L - strike)*L^(k)*asset^(-k - 1)*exp(-rate*tau)*exp(tau*k*l)*pnorm( -d2(asset, L, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) )

    result2 <- 1/(vol*sqrt(tau))*dnorm( d1(asset, L, rate, vol, time, End_Time) ) - pnorm( -d1(asset, L, rate, vol, time, End_Time) ) -
      strike/(L*vol*sqrt(tau))*dnorm( d1(asset, L, rate, vol, time, End_Time) ) -
      k*(L - strike)*L^(k)*asset^(-k - 1)*exp(-rate*tau)*exp(tau*k*l)*pnorm( -d2(asset, L, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) -
      (L - strike)*(L/asset)^(k)*exp(-rate*tau)*exp(tau*k*l)/(asset*vol*sqrt(tau))*dnorm( -d2(asset, L, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) )

    result <- ifelse(tau == 0, result1, result2)
  }

  else if (-k < 0){

    result1 <- - ( pnorm( -d1(asset, L2, rate, vol, time, End_Time) ) - pnorm( -d1(asset, L1, rate, vol, time, End_Time) ) ) -
      k*(L1 - strike)*L1^(k)*asset^(-k - 1)*exp(-rate*tau)*exp(tau*k*l)*( pnorm( -d2(asset, L2, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) -
                                                                pnorm( -d2(asset, L1, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ))

    result2 <- 1/(vol*sqrt(tau))*(dnorm( d1(asset, L2, rate, vol, time, End_Time) ) - dnorm( d1(asset, L1, rate, vol, time, End_Time) )) -
      ( pnorm( -d1(asset, L2, rate, vol, time, End_Time) ) - pnorm( -d1(asset, L1, rate, vol, time, End_Time) ) ) +
      strike/(vol*sqrt(tau))*( dnorm( d1(asset, L2, rate, vol, t, End_Time) )/L2 - dnorm( d1(asset, L1, rate, vol, t, End_Time) )/L1 ) -
      k*(L1 - strike)*L1^(k)*asset^(-k - 1)*exp(-rate*tau)*exp(tau*k*l)*( pnorm( -d2(asset, L2, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) -
                                                                pnorm( -d2(asset, L1, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) )) -
      (L1 - strike)*(L1/asset)^(k)*exp(-rate*tau)*exp(tau*k*l)/(asset*vol*sqrt(tau))*( dnorm( -d2(asset, L2, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) -
                                                                         dnorm( -d2(asset, L1, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ))

    result <- ifelse(tau == 0, result1, result2)
  }

  else{
    L <- L1
    result <- Xi_put_price(asset, L, rate, vol, time, End_Time)
  }

  return(result)
}
