#' @title Finding constant in modified option.
#'
#' @description
#' The call_Newton_concave function is used to find second constant L in modified call option.
#'
#' @usage call_Newton_concave(L1, strike, drift, rate, vol, p, epsilon = 1e-10)
#'
#' @param L1 numeric value, first constant L1 > K.
#' @param strike numeric value, strike price for call option.
#' @param drift numeric value, drift of the model.
#' @param rate numeric value, risk free rate in the model, r >= 0.
#' @param vol numeric value, volatility of the model, vol > 0.
#' @param p numeric value, power of the loss function, p > 1.
#' @param epsilon numeric value, acceptable calculation error
#' @return A numeric value, a second constant for modified call option by concave function.
#'
#' @details There is a need to be careful when there is a warning message.
#' It means that a numerical error occurred during the algorithm and the algorithm was terminated without a while loop
#'
#' @examples
#' call_Newton_concave(140, 100, 0.1, 0, 0.2, 0.5)
#'
#' @export

call_Newton_concave <- function(L1, strike, drift, rate, vol, p, epsilon = 1e-10){
  if (p >= 1 | p <= 0){
    stop("Wrong p argument. p > 1")
  }
  m = drift - rate
  k = m / ( vol^2*(1 - p) )
  if(m / vol^2 <= 1 - p){
    stop(paste("There is no need for a constant other than", L1))
  }

  c <- ( L1 - strike )^(1 - p) * L1^(-m / vol^2)
  c1 <- c^( 1 / (1 - p) )
  x_pocz = ( 1 / (k*c1) )^( 1 / (k-1) )

  if(L1 <= x_pocz){
    x = x_pocz + (x_pocz - L1)/2
  }

  else{
    x =  strike
  }
  n <- 0
  if( is.nan(c1*x^(k) - x + strike) ){
    warning(paste("NaNs produced. Newton's algorithm was not fully working. Check if const are the same.", "For L1:",
                  call_const(L1, strike, drift, rate, vol, p), "For L2:",  call_const(x, strike, drift, rate, vol, p) ))
    return(x)
  }
  while ( abs(c1*x^(k) - x + strike) > epsilon ) {
    x <- x - (c1 * x^(k) - x + strike) / (k * c1 * x^(k-1) - 1)
    n <- n + 1
  }

  return(x)
}
