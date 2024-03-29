% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Xi_call_price.R
\name{Xi_call_price}
\alias{Xi_call_price}
\title{Delta hedging for european call option}
\usage{
Xi_call_price(asset, strike, rate, vol, time, End_Time)
}
\arguments{
\item{asset}{a numeric vector of asset prices.}

\item{strike}{numeric value, strike price for call or put option.}

\item{rate}{numeric value, risk free rate in the model, r >= 0.}

\item{vol}{numeric value, volatility of the model, vol > 0.}

\item{time}{a numeric vector of actual time, time > 0.}

\item{End_Time}{end time of the option, End_time >= time.}
}
\value{
A numeric vector, price of the european call option.
}
\description{
The Xi_call_price function takes parameters from Black-Scholes model and returns a number of stock needed to fully hedge european call option.
}
\examples{
Xi_call_price(100, 100, 0, 0.5, 0, 1)
Xi_call_price(c(100, 120), 100, 0, 0.3, 0, 1)
Xi_call_price(c(100, 120), 100, 0, 0.3, c(0, 0.5), 1)



}
\seealso{
\url{https://en.wikipedia.org/wiki/Black–Scholes_model}.

}
