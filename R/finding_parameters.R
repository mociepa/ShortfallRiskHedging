#' @title Finite difference parameters
#' 
#' @description 
#' The finding_parameters function calculate calculate time and price step sizes.
#' 
#' @usage finding_parameters(initial_price, vol, I, multiplier_price, dt_simulate = 1/250)
#' 
#' @param initial_price numeric value, initial price of the asset.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param I numeric value, number of steps in the stock price.
#' @param multiplier_price numeric value, a number multiplied by the initial_price to get an approximation to infinity.
#' @param dt_simulate numeric value, time step size using in simulations. 
#' @return A numeric vector, respectively price step size and time step size.
#' 
#' @details Explicit finite difference algorithm is not stable, it means we have an limitation on size of asset step size (which is not problematic)
#' and on size of time step size (which is problematic). This function calculate good parameters to the asset and step size for stability of explicit
#' finite difference algorithm. \cr \cr
#' dt_simulate argument is needed for a sake of interpolation. We want existing a natural k which meets:
#' \eqn{ dt = dt_simulate/k }, where dt is step size from finite difference algorithm. If dt is very small is no needed to have this property.
#' 
#' @examples 
#' finding_parameters(100, 0.3, 600, 5)
#' finding_parameters(100, 0.5, 1000, 6, 1/500)
#' 
#' @export

finding_parameters <- function(initial_price, vol, I, multiplier_price, dt_simulate = 1/250){
  ds <- multiplier_price*initial_price / I   
  dt <- 1/(vol^2*I^2)
  vector <- dt_simulate/(1:10000)
  dt <- ifelse( is.na(which(vector <= dt)[1]), dt,  vector[which(vector <= dt)[1]] )
  
  return( c(ds, dt) )
}