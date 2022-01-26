#' @title Calculate modified european put option
#' 
#' @description 
#' The put_price_linear function takes parameters from Black-Scholes model and returns a price of modified europeanput option.
#' 
#' @usage put_price_linear(asset, strike, rate, vol, drift, time, End_Time, L)
#' 
#' @param asset a numeric vector of asset prices.
#' @param strike numeric value, strike price for call or put option.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param drift numeric value, drift of the model.
#' @param time a numeric vector of actual time, time > 0.
#' @param End_Time end time of the option, End_time >= time.
#' @param L a numeric value, determines where option payoff is zero, see details, L > 0.
#' @return A numeric vector, price of the modification of european put option using linear loss function.
#' 
#' @details Payoff of this modified call option is: 
#' ## \eqn{ 1(asset > L)(asset - strike)^+ }, when \eqn{ drift > rate }.
#' ## \eqn{ 1(asset < L)(asset - strike)^+ }, when \eqn{ drift < rate }.
#' ## \eqn{ L(asset - strike)^+ }, when \eqn{ drift == rate }, of course in this case L <= 1.
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Black–Scholes_model}.
#' 
#' @examples 
#' put_price_linear(100, 100, 0, 0.5, 0.05, 0, 1, 120)
#' put_price_linear(c(100, 120), 100, 0, 0.3, 0.05, 0, 1, 120)
#' put_price_linear(c(100, 120), 100, 0, 0.3, 0.05, c(0, 0.5), 1, 120)
#' 
#' 
#' 
#' @export

put_price_linear <- function(asset, strike, rate, vol, drift, time, End_Time, L){
  m = drift - rate
  tau = End_Time - time
  
  if(m > 0){
    if(L >= strike){
      result <- 0
    }
    else{
      result <- put_price( asset, strike, rate, vol, time, End_Time ) - put_price( asset, L, rate, vol, time, End_Time ) + 
        (L - strike)*pnorm( -d2(asset, L, rate, vol, time, End_Time) )*exp(-rate*tau)
    }
  }
  
  else if(m < 0){
    if(L >= strike){
      result <- put_price( asset, strike, rate, vol, time, End_Time )
    }
    else{
      result <- put_price( asset, L, rate, vol, time, End_Time ) + (strike - L)*pnorm( -d2(asset, L, rate, vol, time, End_Time) )*exp(-rate*tau)
    }
  }
  
  else{
    result <- L*put_price( asset, strike, rate, vol, time, End_Time )
  }
  
  return(result)
}
