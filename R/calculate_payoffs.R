#' @title Calculate payoffs from trajectories
#' 
#' @description 
#' The calculate_payoffs function calculate payoffs from trajectories
#' 
#' @usage calculate_payoffs(paths, FUN, ...)
#' 
#' @param paths numeric matrix, price of the asset from different trajectories.
#' @param FUN the function to be applied, payoff option: see 'Details' and 'Examples'.
#' @param ... arguments to FUN.
#' @return A numeric vector, option payoffs from different trajectories
#' 
#' @details FUN have to had parameter asset_price which represent price of the asset over the life of the option, see call_payoff \cr \cr
#' Arguments in ... cannot have the same name as any of the other arguments, and care may be needed to avoid errors.
#' @examples 
#' x <- generate_scenarios(100, 0, 0.3)
#' calculate_payoffs(X, call_payoff, strike = 100)
#' calculate_payoffs(X, put_payoff, strike = 100)
#' ## Example with asian call payoff
#' asian_call <- function(asset_price, strike){
#'   return(mean(asset_price) - min( mean(asset_price), strike ))
#'   }
#' calculate_payoffs(X, option_modificate_payoff, const = 110, drift = 0.05, rate = 0, vol = 0.3, p = 1, asian_call, strike = 105)
#' @export

calculate_payoffs <- function(paths, FUN, ...){
  result <- rep(0, times = ncol(paths))
  for (i in (1:length(result) ) ) {
    result[i] <- FUN(asset_price = paths[, i], ...)
  }
  return(result)
}