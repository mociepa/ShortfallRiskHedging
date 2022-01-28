#' @title Calculate payoff from modificate european option
#'
#' @description
#' The option_linear_payoff function is used to calculate payoff of modified european option.
#'
#' @usage option_linear_payoff(const, asset_price, drift, rate, FUN, ...)
#'
#' @param const numeric value, parameter of the modified payoff.
#' @param asset_price numeric vector, the price of the asset over the life of the option.
#' @param drift numeric value, drift of the model.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @param FUN the function to be applied, payoff option: see 'Details' and 'Examples'.
#' @param ... arguments to FUN
#' @return A numeric value, payoff of the modified european option using linear loss function.
#'
#' @details FUN have to had parameter asset_price which represent price oft the asset over the life of the option, see call_payoff \cr \cr
#' Arguments in ... cannot have the same name as any of the other arguments, and care may be needed to avoid errors.
#' @examples
#' option_linear_payoff(110, c(121.1, 120, 125.4), 0.1, 0.05, call_payoff, strike = 105)
#' option_linear_payoff(110, c(141.1, 150, 135.4), 0.1, 0.05, put_payoff, strike = 150)
#' ## Example with asian call payoff
#' asian_call <- function(asset_price, strike){
#'   return(mean(asset_price) - min( mean(asset_price), strike ))
#'   }
#' option_linear_payoff(110, c(121.1, 120, 125.4), 0.1, 0.05, asian_call, strike = 105)
#'
#' @export

option_linear_payoff <- function(const, asset_price, drift, rate, FUN, ...){
  m <- drift - rate
  if (m > 0){
    result <- 0
    if (tail(asset_price, 1) > const){
      result <- FUN(asset_price, ...)
    }
  }

  else if (m < 0){
    result <- 0
    if (tail(asset_price, 1) < const){
      result <- FUN(asset_price, ...)
    }
  }

  else{
    result <- L*FUN(asset_price, ...)
  }
  return(result)
}
