#' @title Calculate payoffs from trajectories
#'
#' @description
#' The portfolio_valuation function determines the value of the portfolio while option hedging.
#'
#' @usage portfolio_valuation(option_value, rate, Xi, asset_price, End_Time = 1, initial_capital = 0)
#'
#' @param option_value numeric value, option price at the beginning of the simulations.
#' @param rate numeric value, risk free rate in the model, rate >= 0.
#' @param Xi numeric vector, number of asset needed to hedge an option over time.
#' @param asset_price numeric vector, price oft the asset over time.
#' @param End_Time numeric value, end time of the option.
#' @param initial_capital numeric value, the amount of money we want to add to the portfolio at time 0.
#' @return A numeric vector, value of the hedging portfolio over time.
#'
#' @details In theory hedging give us full protection against the option. Due to discretization of the Black-Scholes model in simulations we can
#' have some losses or gains during time. portfolio_valuation function calculate this gains/losses over time. \cr \cr
#' initial_capital is the initial amount, if we were to use the incomplete initial price of the option to insure against it. For example:
#' Price of the european call option is 5, to hedge this option we will hedge some modification of this option with the
#' inital price 4. Now we can set initial_capital = 1, which we add at the beginning of the hedging, and grow according with the risk free rate.
#' @examples
#' x <- generate_scenarios(100, 0.05, 0.3) #real measure
#' time_period <- seq(0, 1, 1/250)
#' option <- call_price(x[, 1], 100, 0, 0.3, time_period, 1)
#' Xi <- Xi_call_price(x[, 1], 100, 0, 0.3, time_period, 1)
#' portfolio_valuation(option[1], 0, Xi, x[, 1])
#' @export

portfolio_valuation <- function(option_value, rate, Xi, asset_price, End_Time = 1, initial_capital = 0){
  time <- End_Time / ( length(asset_price) - 1 )
  cash <- rep( 0, times = length(asset_price) )
  cash[1] <- Eta(asset_price[1], xi[1], rate, option_value, 0) + initial_capital
  for (i in 2:length(asset_price)) {
    cash[i] <- cash[i-1]*exp(rate*time) - (xi[i] - xi[i-1])*asset_price[i]
  }

  portfolio <- cash + xi*asset_price
  return(portfolio)
}
