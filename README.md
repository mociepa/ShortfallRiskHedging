# ShortfallRiskHedging
ShortfallRiskHedging \
To install packages use the following formula in R: devtools::install_github('mociepa/ShortfallRiskHedging') \
\
The Black-Scholes model is complete, it means that every european contingent claim is attainable. So every european option has one "fair" price, 
any other price for the option will lead to arbitrage. For this fair price, we can create a self-financing strategy that will secure a given contingent claim. \
Let's assume that the investor does not want to use the initial capital that will fully hedge the option, but wants to use some part of it, and is ready to take some risk. What in that case is the optimal hedging strategy? 

In a thesis we will focus to minimize Shorfall risk i.e. expected value of the shortfall weighted by the x^p loss function, where p > 0. 


This R package was created for the needs of my master's thesis, it allows to evaluate and hedge modificated call and put option (using theoretical formulas), also allows to evaluate and hedge any modified option that is not path dependent. 



The english version of the thesis has been added. Also there was a change in a finite difference explicit example. Previously:
finite_difference_explicit(ds, dt, 600, 0, 0.3, 1, FUN = option_modificate_payoff, const = 120, drift = 0.1, vol = 0.3, p = 1, call_payoff, 100, is_modificate = TRUE)
there was wrong dimension of the matrix

