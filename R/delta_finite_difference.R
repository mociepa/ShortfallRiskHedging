#' @title Hedging of the option
#'
#' @description
#' The delta_finite_difference function calculate number of asset needed to hedge an option using finite difference.
#'
#' @usage delta_finite_difference(option_value, ds, is_continuous = TRUE, discontinuity_points = NA)
#'
#' @param option_value numeric matrix, option prices, return from finite_difference_explicit function.
#' @param ds numeric value, asset step size.
#' @param is_continuous logical value, TRUE means that the payoff function is continuous due to the asset price.
#' @param discontinuity_points numeric vector, denoting at which points of asset price the payoff function is discontinuous.
#' Elements of this vector have to be positive.
#' @return A numeric matrix, estimated number of asset needed to hedge an option.
#'
#' @examples
#' ds <- finding_parameters(100, 0.3, 600, 5)[1]
#' dt <- finding_parameters(100, 0.3, 600, 5)[2]
#' option <- finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, call_payoff, 100)
#' delta_finite_difference(option, ds)
#' option <- finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, FUN = option_modificate_payoff, const = 120, drift = 0.1, vol = 0.3, p = 1, call_payoff, 100, is_modificate = TRUE)
#' delta_finite_difference(option, ds)
#' @export

delta_finite_difference <- function(option_value, ds, is_continuous = TRUE, discontinuity_points = NA){
  if(is_continuous == FALSE & is.na(discontinuity_points)) {
    stop( "When payoff option is discontinuous, you have to give when discontinuity occur in discontinuity_points argument" )
  }
  asset_price_step <- 0:(nrow(option_value) - 1) * ds

  deltaV <- matrix(0, nrow = nrow(option_value), ncol = ncol(option_value))
  for (i in 2:(nrow(option_value) - 1) ) {
    deltaV[i, ] <- ( option_value[i+1, ] - option_value[i-1, ] )/(2*ds)
  }
  deltaV[1, ] <- ( option_value[2, ] - option_value[1, ]) / ds
  deltaV[nrow(option_value), ] <- (option_value[nrow(option_value), ] -  option_value[nrow(option_value) - 1, ]) / ds

  if(is_continuous == FALSE){
    index_vector <- rep(0, length(discontinuity_points))
    for (i in 1:length(index_vector)) {
      index_vector[i] <- which(asset_price_step >= discontinuity_points)[1]
      deltaV[index_vector[i], 1] <- (option_value[index_vector[i] + 1, 1] -  option_value[index_vector[i], 1]) / ds
      deltaV[index_vector[i] - 1, 1] <- (option_value[index_vector[i] - 1, 1] -  option_value[index_vector[i] - 2, 1]) / ds
    }

  }

  return(deltaV)
}
