#' @title Calculate european put option payoff
#'
#' @description
#' The put_payoff function calculate payoffs from trajectory.
#'
#' @usage put_payoff(asset_price, strike)
#'
#' @param asset_price numeric vector, price of the asset over time.
#' @param strike numeric value, strike price of the option.
#' @return A numeric value, european put option payoff.
#'
#' @details In the case when strike = 0, option is called european asset or nothing put option.
#'
#' @examples
#' x <- generate_scenarios(100, 0, 0.3)
#' put_payoff(x[, 1], strike = 100)
#' put_payoff(c(100, 120), strike = 107)
#' put_payoff(c(100, 120, 107), strike = 107)
#' @export

put_payoff <- function(asset_price, strike){
  result <- max(strike - tail(asset_price, 1), 0)

  return(result)
}
