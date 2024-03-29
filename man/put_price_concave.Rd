% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/put_price_concave.R
\name{put_price_concave}
\alias{put_price_concave}
\title{Calculate modified european put option}
\usage{
put_price_concave(asset, strike, rate, vol, drift, p, time, End_Time, L, L2 = NA)
}
\arguments{
\item{asset}{a numeric vector of asset prices.}

\item{strike}{numeric value, strike price for call or put option.}

\item{rate}{numeric value, risk free rate in the model, r >= 0.}

\item{vol}{numeric value, volatility of the model, vol > 0.}

\item{drift}{numeric value, drift of the model.}

\item{p}{numeric positive value, power of the loss function, p < 1.}

\item{time}{a numeric vector of actual time, time > 0.}

\item{End_Time}{end time of the option, End_time >= time.}

\item{L}{numeric value, determines option payoff, L > 0.}

\item{L2}{numeric value, determines option payoff, if L2 = NA, but is needed, function finds it with Newton's algorithm.}
}
\value{
A numeric vector, price of the modification of european put option using concave loss function.
}
\description{
The put_price_concave function takes parameters from Black-Scholes model and returns a price of modified european put option.
}
\examples{
put_price_concave(100, 100, 0, 0.5, 0.05, 0.5,  0, 1, 80)
put_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, 0, 1, 80)
put_price_concave(c(100, 120), 100, 0, 0.3, 0.05, 0.5, c(0, 0.5), 1, 60)



}
