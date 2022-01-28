#' @title Calculate european call option payoff
#'
#' @description
#' The call_payoff function calculate payoffs from trajectory.
#'
#' @usage call_payoff(asset_price, strike)
#'
#' @param asset_price numeric vector, price of the asset over time.
#' @param strike numeric value, strike price of the option.
#' @return A numeric value, european call option payoff.
#'
#' @details In the case when strike = 0, option is called european asset or nothing call option.
#'
#' @examples
#' x <- generate_scenarios(100, 0, 0.3)
#' call_payoff(x[, 1], strike = 100)
#' call_payoff(c(100, 120), strike = 107)
#' call_payoff(c(100, 120, 107), strike = 107)
#' @export

call_payoff <- function(asset_price, strike){
  result <- max(tail(asset_price, 1) - strike, 0)

  return(result)
}
