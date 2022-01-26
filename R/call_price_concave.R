#' @title Calculate modified european call option
#' 
#' @description 
#' The call_price_concave function takes parameters from Black-Scholes model and returns a price of modified european call option.
#' 
#' @usage call_price_concave(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
#' 
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param p numeric positive value, power of the loss function, p < 1.
#' @param End_Time end time of the option, End_time >= time.
#' @param L numeric value, determines option payoff, see details, L > 0.
#' @param L2 numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.
#' @return A numeric vector, price of the modification of european call option using concave loss function.
#' 
#' @details Payoff of this modified call option is: 
#' ## \eqn{ 1(asset > L)(asset - strike)^+ }, when \eqn{ drift > rate }.
#' ## \eqn{ 1(asset < L)(asset - strike)^+ }, when \eqn{ drift < rate }.
#' ## \eqn{ L(asset - strike)^+ }, when \eqn{ drift == rate }, of course in this case L <= 1.
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#' 
#' @examples 
#' call_price_concave(100, 100, 0, 0.5, 0.05, 0.5,  0, 1, 105)
#' call_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, 0, 1, 105)
#' call_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, c(0, 0.5), 1, 105)
#' 
#' 
#' 
#' @export

call_price_concave <- function(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA){
  m = drift - rate
  tau = End_Time - time
  
  if (L < strike){
    result <- 0
  }
  
  else{
    if ( m/vol^2 <= 1 - p){
      result <- call_price(asset, strike, rate, vol, time, End_Time) - call_price(asset, L, rate, vol, time, End_Time) + (strike - L)*exp(-rate*tau)*pnorm( d2(asset, L, rate, vol, time, End_Time) )
    }
    
    else{
      L1 <- L
      if ( is.na(L2) == TRUE ){
        L_bis <- call_Newton_concave(L, strike, drift, rate, vol, p)[1]
        L1 <- min(L, L_bis)
        L2 <- max(L, L_bis)
      }
      else{
        L1 <- min(L, L2)
        L2 <- max(L, L2)
      }
      result <- call_price(asset, strike, rate, vol, time, End_Time) - call_price(asset, L1, rate, vol, time, End_Time) + (strike - L1)*exp(-rate*tau)*pnorm( d2(asset, L1, rate, vol, time, End_Time) ) +
        call_price(asset, L2, rate, vol, time, End_Time) + (L2 - strike)*exp(-rate*tau)*pnorm( d2(asset, L2, rate, vol, time, End_Time) )
    }
    
  }
  
  return(result)
}