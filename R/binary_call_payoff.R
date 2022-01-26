#' @title Calculate european binary call option payoff
#' 
#' @description 
#' The binary_call_payoff function calculate payoffs from trajectory.
#' 
#' @usage binary_call_payoff(asset_price, strike)
#' 
#' @param asset_price numeric vector, price of the asset over time.
#' @param strike numeric value, strike price of the option.
#' @return A numeric value, european binary call option payoff.
#' 
#' @examples 
#' x <- generate_scenarios(100, 0, 0.3)
#' binary_call_payoff(x[, 1], strike = 100)
#' binary_call_payoff(c(100, 120), strike = 107)
#' @export

binary_call_payoff <- function(asset_price, strike){
  if(tail(asset_price, 1) > strike){
    result <- 1
  }
  else{
    result <- 0
  }
  
  return(result)
}