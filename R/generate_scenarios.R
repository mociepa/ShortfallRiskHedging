#' @title Generate scenarios from Black-Scholes model
#'
#' @description
#' The generate_scenarios function is used to generate trajectories of stock price from Geometric Brownian Motion.
#'
#' @usage generate_scenarios(initial_price, rate, vol, dt = 1/250, initial_time = 0, End_Time = 1, n = 10^5, seed = 219)
#'
#' @param initial_price numeric value, initial price of the asset.
#' @param rate numeric value, risk free rate in the model.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param dt numeric value, represents the number of simulated stock prices in one trajectory.
#' @param initial_time numeric value, start time of the simulations.
#' @param End_Time numeric value, end time of the simulations.
#' @param n numeric value, numbers of trajectory simulations, n is natural number.
#' @param seed numeric value, denotes the seed from which the simulations are drawn, seed is natural number.
#' @return A numeric matrix, where in columns are different trajectories of asset prices.
#'
#' @details generate_scenarios() function generate asset price assumed that stock price are from Black-Scholes model. Next asset price is
#' calculate by iteration: \cr
#' \eqn{asset_{i} = asset_{i-1}(1 + rate*dt + vol*Z)}, where Z is draw from normal distribution with variance dt. \cr \cr
#' When in parameter rate we take risk free rate of the market, we generate scenarios using equivalent martingale measure.\cr
#' If in parameter rate we take drift of the model, function generate scenarios in a real measure. \cr \cr
#' Usually initial_time = 0, if that's not the case we assumed that we are in the course of a lifetime of options, and previous stock price is important (in the case of path-dependent options)
#' initial_time should me equal one of the values in vector \eqn{c (0, seq(dt, End_Time, dt) )}. \cr \cr
#' \eqn{1/dt + 1} represent number of changes in stock price in a year. In the default case we assumed dt = 1/250, which represent the average number of trading days.
#'
#' @examples
#' generate_scenarios(100, 0.05, 0.3)
#' generate_scenarios(100, 0.05, 0.3, n = 10^4)
#' generate_scenarios(100, 0.05, 0.3, dt = 1/500, initial_time = 0, End_Time = 1, n = 10^5, seed = 219)
#'
#'
#'
#' @export

generate_scenarios <- function(initial_price, rate, vol, dt = 1/250, initial_time = 0, End_Time = 1, n = 10^5, seed = 219){
  time <- seq(dt + initial_time, End_Time, by = dt)
  set.seed(seed)
  inc <- matrix( rnorm( n*(length(time)), 0, sqrt(dt)), length(time) )
  x <- apply(inc, 2, cumsum)
  x <- (rate - 0.5*vol^2) * time + vol*x
  x <- exp(x)
  return(rbind( rep(initial_price, n), initial_price*x ))
}
