#' @title Delta hedging for european option
#'
#' @description
#' The Eta function takes parameters from Black-Scholes model and returns amount of money needed to hedge an european option.
#' @usage Eta(asset, Xi, rate, option_price, time)
#'
#' @param asset a numeric vector of asset prices.
#' @param Xi a numeric vector, number of asset needed to hedge an option
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param option_price a numeric vector of option prices
#' @param time a numeric vector of actual time, time >= 0.
#' @return A numeric vector, amount of money needed to hedge an option.
#'
#' @examples
#' Eta(100, 0.5596177, 0, 11.92354, 0)
#' Eta(c(100, 120), c(0.5596177, 0.7756962), 0, c(11.92354, 25.44056), 0)
#' Eta(c(100, 120), c(0.5596177, 0.8328623), 0, c(11.92354, 22.50378), c(0, 0.5))
#'
#'
#'
#' @export

Eta <- function(asset, Xi, rate, option_price, time){

  return( (option_price - Xi*asset) / exp(rate*time) )
}
