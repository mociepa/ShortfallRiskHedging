% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_const.R
\name{call_const}
\alias{call_const}
\title{Finding const value}
\usage{
call_const(L, strike, drift, rate, vol, p)
}
\arguments{
\item{L}{numeric value, const value from the formulas for the modified call options.}

\item{strike}{numeric value, strike of the call option.}

\item{drift}{numeric value, drift of the asset in the model.}

\item{rate}{numeric value, risk free rate in the model rate >= 0.}

\item{vol}{numeric value, volatility of the model, vol > 0.}

\item{p, }{numeric value, power of the loss function.}
}
\value{
A numeric value, const from the overall model.
}
\description{
The call_const function computes a constant from the overall model.
}
\examples{
call_const(110, 100, 0.1, 0, 0.3, 1)
call_const(110, 100, 0.1, 0, 0.3, 2)

}
