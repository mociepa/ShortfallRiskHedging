#' @title Calculate modified european put option
#'
#' @description
#' The put_price_convex function takes parameters from Black-Scholes model and returns a price of modified european call option.
#'
#' @usage put_price_convex(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
#'
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param p numeric value, power of the loss function, p > 1.
#' @param End_Time end time of the option, End_time >= time.
#' @param L numeric value, determines option payoff, L > 0.
#' @param L2 numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.
#' @return A numeric vector, price of the modification of european put option using convex loss function.
#'
#'
#' @examples
#' put_price_convex(100, 100, 0, 0.5, 0.05, 2,  0, 1, 70)
#' put_price_convex(c(100, 120), 100, 0, 0.3, 0.05, 2, 0, 1, 80)
#' put_price_convex(c(100, 120), 100, 0, 0.3, 0.05, 2, c(0, 0.5), 1, 55)
#'
#'
#'
#' @export

put_price_convex <- function(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA){
  if (p <= 1){
    stop("Wrong p argument. p > 1")
  }
  m <- drift - rate
  k <- m / ( (p-1)*vol^2 )
  tau <- End_Time - time

  if(L > strike){
    result <- put_price(asset, strike, rate, vol, time, End_Time)
  }

  else{
    if(-k > 0){
      result1 <- strike*exp(-rate*tau)*pnorm( -d2(asset, L, rate, vol, time, End_Time) ) - asset*pnorm( -d1(asset, L, rate, vol, time, End_Time) ) +
        (L - strike)*(L/asset)^(k)*exp(-rate*tau)*exp(k*tau*( 0.5*vol^2 - rate + 0.5*m/(p-1) ))*pnorm( -d2(asset, L, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) )
      result <- ifelse(asset == 0, 0, result1)
    }

    else if(-k < 0){

      if (is.na(L2)){
        warning("In this case L2 is needed, while parameter L2 = NA. Calculated L2 argument by the Newton method (put_Newton_convex function).")
        L_bis <- put_Newton_convex(L, strike, drift, rate, vol, p)[1]
        L1 <- min(L, L_bis)
        L2 <- max(L, L_bis)
      }
      else{
        L1 <- min(L, L2)
        L2 <- max(L, L2)
      }
      result1 <- strike*exp(-rate*tau)*( pnorm( -d2(asset, L2, rate, vol, time, End_Time) ) - pnorm( -d2(asset, L1, rate, vol, time, End_Time) ) ) -
        asset*( pnorm( -d1(asset, L2, rate, vol, time, End_Time) ) - pnorm( -d1(asset, L1, rate, vol, time, End_Time) ) ) +
        (L1 - strike)*(L1/asset)^(k)*exp(-rate*tau)*exp(k*tau*( 0.5*vol^2 - rate + 0.5*m/(p-1) ))*( pnorm( -d2(asset, L2, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) - pnorm( -d2(asset, L1, rate, vol, time, End_Time) + m*sqrt(tau)/(vol*(p-1)) ) )
      result <- ifelse(asset == 0, 0, result1)
    }

    else{
      result <- put_price(asset, L, rate, vol, time, End_Time)
    }
  }
  return(result)
}
