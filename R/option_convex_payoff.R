#' @title Calculate payoff from modificate european option
#' 
#' @description 
#' The option_convex_payoff function is used to calculate payoff of modified european option.
#' 
#' @usage option_convex_payoff(const, asset_price, drift, rate, vol, p, FUN, ...)
#' 
#' @param const numeric value, parameter of the modified payoff.
#' @param asset_price numeric vector, the price of the asset over the life of the option.
#' @param drift numeric value, drift of the model.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param p numeric value, power of the loss function, p > 1.
#' @param FUN the function to be applied, payoff option: see 'Details' and 'Examples'.
#' @param ... arguments to FUN.
#' @return A numeric value, payoff of the modified european option using convex loss function.
#' 
#' @details FUN have to had parameter asset_price which represent price oft the asset over the life of the option, see call_payoff \cr \cr
#' Arguments in ... cannot have the same name as any of the other arguments, and care may be needed to avoid errors.
#' @examples 
#' option_convex_payoff(5.110, c(121.1, 120, 125.4), 0.1, 0.05, 0.3, 3, call_payoff, strike = 105)
#' option_convex_payoff(1.410, c(141.1, 150, 135.4), 0.1, 0.05, 0.3, 3, put_payoff, strike = 150)
#' ## Example with asian call payoff
#' asian_call <- function(asset_price, strike){
#'   return(mean(asset_price) - min( mean(asset_price), strike ))
#'   }
#' option_convex_payoff(1.1, c(121.1, 120, 125.4), 0.1, 0.05, 0.3, 3, asian_call, strike = 105)
#'  
#' @export

option_convex_payoff <- function(const, asset_price, drift, rate, vol, p, FUN, ...){
  m <- drift - rate
  k <- m / ( vol^2*(p - 1) )
  return( FUN(asset_price, ...) - min( const*tail(asset_price, 1)^(-k), FUN(asset_price, ...) ) )
}