#' @title Hedging of the option
#' 
#' @description 
#' The matrix_eta function calculate amount of money needed to hedge an option claim.
#' 
#' @usage matrix_eta(option_matrix, Xi_matrix_matrix, ds, dt, I, rate)
#' 
#' @param option_matrix numeric matrix, value of the option.
#' @param Xi_matrix numeric matrix, number of asset needed to hedge an option.
#' @param ds numeric value, step size of the asset price.
#' @param dt numeric value, step size of the time.
#' @param I numeric value, number of steps in the stock price.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @return A numeric matrix, amount of money needed to hedge an option claim using finite difference algorithms.
#'
#' @examples 
#' ds <- finding_parameters(100, 0.3, 600, 5)[1]
#' dt <- finding_parameters(100, 0.3, 600, 5)[2]
#' option <- finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, call_payoff, 100)
#' Xi <- delta_finite_difference(option, ds)
#' matrix_eta(option, Xi, ds, dt, 600, 0)
#' 
#' @export

matrix_eta <- function(option_matrix, Xi_matrix_matrix, ds, dt, I, rate){
  S <- (0:I)*ds
  t <- (0:( ncol(option_matrix) - 1 ) )*dt
  S_matrix <- matrix( rep(S, ncol(option_matrix)), nrow(option_matrix), ncol(option_matrix) )
  t_matrix <- matrix( rep(t, nrow(option_matrix)), nrow(option_matrix), ncol(option_matrix), byrow = TRUE )
  
  return(Eta(S_matrix, Xi_matrix, rate, option_matrix, t_matrix))
}