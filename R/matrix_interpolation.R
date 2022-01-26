#' @title Linear interpolation
#' 
#' @description 
#' The matrix_interpolation function interpolate values for vector.
#' 
#' @usage matrix_interpolation(Value_matrix, ds, dt, I, asset_price, time, End_Time)
#' 
#' @param Value_matrix numeric matrix, returns from finite different algorithm or delta_finite_difference function.
#' @param ds numeric value, step size of the asset price.
#' @param dt numeric value, step size of the time.
#' @param I numeric value, number of steps in the stock price.
#' @param asset_price numeric vector, the price of the asset over the life of the option.
#' @param time numeric vector, actual time.
#' @param End_Time numeric value, end time of the option.
#' 
#' @return A numeric vector, interpolated value of option price or numbers of asset when asset_price is an argument.
#' 
#' @details If asset price is bigger then I*ds, we take value from this element of the matrix where asset_price = I*ds.
#' @examples 
#' ds <- finding_parameters(100, 0.3, 600, 5)[1]
#' dt <- finding_parameters(100, 0.3, 600, 5)[2]
#' option <- finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, call_payoff, 100)
#' matrix_interpolation(option, ds, dt, 600, c(259, 150), c(0, 0.5), 1)
#' 
#' @export

matrix_interpolation <- function(Value_matrix, dt, ds, I, asset_price, time, End_Time){
  S <- rev(asset_price)
  time_to_maturity <- End_Time - rev(time)
  S_vector <- ds*(0:I)
  time_vector <- End_Time - dt*( (ncol(Value_matrix)-1):0 )
  
  index_S <- rep(0, length(S))
  for (i in 1:length(S)) {
    index_S[i] <- which(S[i] < S_vector)[1]#g?rny index
  }
  
  index_t <- rep(0, length(time_to_maturity))
  for (i in 1:length(time_to_maturity) ) {
    index_t[i] <- which.min(abs(time_vector - time_to_maturity[i]))
  }
  
  result <- ifelse(is.na(index_S), Value_matrix[ length(S_vector), index_t[which( is.na(index_S) )] ], 
                   linear_interpolation(S, S_vector[index_S-1], S_vector[index_S], Value_matrix[cbind(index_S-1, index_t)], Value_matrix[cbind(index_S, index_t)]))
  return(rev(result))
}