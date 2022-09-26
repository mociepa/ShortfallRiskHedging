#' @title Explicit finite difference
#'
#' @description
#' The finite_difference_explicit function calculate option price using finite difference.
#'
#' @usage finite_difference_explicit(ds, dt, I, rate, volatility, End_Time, FUN, ..., is_modificate = FALSE)
#'
#' @param ds numeric value, step size of the asset price.
#' @param dt numeric value, step size of the time.
#' @param I numeric value, number of steps in the stock price.
#' @param rate numeric value, risk free rate in the model rate >= 0.
#' @param volatility numeric value, volatility of the model, vol > 0.
#' @param End_Time numeric value, end time of the option.
#' @param FUN the function to be applied, payoff option.
#' @param ... arguments to FUN.
#' @param is_modificate logical value, determines whether FUN payoff is from modified payoff or not, see 'Details'.
#' @return A numeric matrix, calculated price of the option using finite difference method, see 'Details'.
#'
#' @details When is_modificate is TRUE (so when as FUN is used option_linear_payoff, option_convex_payoff, option_concave_payoff or option_modificate_payoff),
#' there no need to write an risk free rate argument in ..., this argument is taken from rate argument.\cr \cr
#' Return of this option is a matrix. Rows determines changes of the option prices over time, when we price of the asset is not changing.
#' The price of the asset increases as the rows increase. Time to exercise of the option increases as the columns increase. For example first column
#' shows payoff price.
#' @examples
#' ds <- finding_parameters(100, 0.3, 600, 5)[1]
#' dt <- finding_parameters(100, 0.3, 600, 5)[2]
#' finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, call_payoff, 100)
#' finite_difference_explicit(ds, dt, 600, 0, volatility = 0.3, 1, FUN = option_modificate_payoff, const = 120, drift = 0.1, vol = 0.3, p = 1, call_payoff, 100, is_modificate = TRUE)
#'
#' @export

finite_difference_explicit <- function(ds, dt, I, rate, volatility, End_Time, FUN, ..., is_modificate = FALSE){
  J <- length(seq(End_Time, 0, -dt)) - 1
  dt <- End_Time/J
  V <- matrix(0, I + 1, J + 1)
  Si <- ds*(0:I)
  ai <- 0.5*volatility^2*Si^2
  bi <- rate*Si
  ci <- -rate

  for (i in 1:(I + 1) ) {
    if(is_modificate){
      V[i, 1] <- FUN(asset_price = (i - 1)*ds, rate = rate, ...)
    }

    else{
      V[i, 1] <- FUN(asset_price = (i - 1)*ds, ...)
    }
  }

  for (k in 2:(J + 1) ){
    A <- dt/ds^2*ai - 0.5*dt/ds*bi
    B <- -2*dt/ds^2*ai + dt*ci
    C <- dt/ds^2*ai + 0.5*dt/ds*bi
    V[1, k] <- (1 - rate*dt)*V[1, k - 1]
    V[2:I, k] <- A[2:I]*V[1:(I-1), k - 1] + (1 + B[2:I])*V[2:I, k - 1] + C[2:I]*V[3:(I + 1), k - 1]
    if ( V[I + 1, 1] == 0){
      V[I + 1, k] <- A[I + 1]*V[I, k - 1]
    }
    else{
      V[I + 1, k] <- 2*V[I, k] - V[I - 1, k]
      diff <- V[I, k] - V[I, k - 1]
      V[I+1, k] <- V[I + 1, k - 1] + diff
    }

  }

  return(V)
}
