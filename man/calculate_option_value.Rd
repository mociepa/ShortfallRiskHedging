% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_option_value.R
\name{calculate_option_value}
\alias{calculate_option_value}
\title{Calculate option values}
\usage{
calculate_option_value(payoffs, rate, time = 0, End_Time = 1)
}
\arguments{
\item{payoffs}{numeric vector, payoffs of the option from simulations.}

\item{rate}{numeric value, risk free rate in the model rate >= 0.}

\item{time}{numeric value, actual time.}

\item{End_Time}{numeric value, end time of the option, time < End_Time.}
}
\value{
A numeric value, estimated value of the option.
}
\description{
The calculate_option_value function calculate option value using Monte Carlo simulations.
}
\examples{
X <- generate_scenarios(100, 0, 0.3)
payoff <- calculate_payoffs(X, call_payoff, strike = 100)
calculate_option_value(payoff, 0)

payoff <- calculate_payoffs(X, put_payoff, strike = 100)
calculate_option_value(payoff, 0)

## Example with asian call payoff
asian_call <- function(asset_price, strike){
  return(mean(asset_price) - min( mean(asset_price), strike ))
  }
payoff <- calculate_payoffs(paths = X, option_modificate_payoff, const = 110, drift = 0.05, rate = 0, vol = 0.3, p = 1, asian_call, strike = 105)
calculate_option_value(payoff, 0)
}
