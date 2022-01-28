#' @title Calculate european binary put option payoff
#'
#' @description
#' The binary_put_payoff function calculate payoffs from trajectory.
#'
#' @usage binary_put_payoff(asset_price, strike)
#'
#' @param asset_price numeric vector, price of the asset over time.
#' @param strike numeric value, strike price of the option.
#' @return A numeric value, european binary put option payoff.
#'
#' @examples
#' x <- generate_scenarios(100, 0, 0.3)
#' binary_put_payoff(x[, 1], strike = 100)
#' binary_put_payoff(c(100, 120), strike = 107)
#' binary_put_payoff(c(100, 120, 107), strike = 107)
#' @export

binary_put_payoff <- function(asset_price, strike){
  if(tail(asset_price, 1) < strike){
    result <- 1
  }
  else{
    result <- 0
  }

  return(result)
}
