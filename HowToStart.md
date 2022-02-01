This document help you get started with the package, and shows the main features of the package.

### First we give parameters of the model:

```
library(ShortfallRiskHedging)
S0 <- 100 #asset price at 0
K <- 100 #strike price
r <- 0.05 #risk free rate
mu <- 0.1 #drift
vol <- 0.3 #volatility
t <- 0 #start time
End_Time <- 1 #end time
```
### Theoretical evaluation:
#### Adding parameters which we want to use in an optimal hedging:
```
V0 <- 0.75*call_price(S0, K, r, vol, t, End_Time) #initial capital
p <- 2 #power of the loss function
L1 <- call_finding_modified(S0, K, r, vol, mu, p, t, End_Time, V0, Bisection_method)[1]
L2 <- call_finding_modified(S0, K, r, vol, mu, p, t, End_Time, V0, Bisection_method)[2]
call_price_explicit(S0, K, r, vol, mu, p, t, End_Time, L1, L2)
```

#### Simulate the data:

```
X <- generate_scenarios(S0, mu, vol, seed = 1341, n = 10^4) #trajectories of the asset price 
time <- (0:250)*(1/250)

option_prices <- call_price_explicit(X[, 1], K, r, vol, mu, p, time, End_Time, L1, L2) #option price change over time
Xi <- Xi_call_price_explicit(X[, 1], K, r, vol, mu, p, time, End_Time, L1, L2) #number of asset needed to hedge over time
portfolio_valuation(option_prices[1], r, Xi, X[, 1]) #portfolio value over time
```

#### Simple histogram of losses:

```
vector_of_losses <- rep(0, 10000)
for (i in 1:10000) {
  option_prices <- call_price_explicit(X[, i], K, r, vol, mu, p, time, End_Time, L1, L2) #option price change over time
  Xi <- Xi_call_price_explicit(X[, i], K, r, vol, mu, p, time, End_Time, L1, L2) #number of asset needed to hedge over time
  portfolio <- portfolio_valuation(option_prices[1], r, Xi, X[, i])
  vector_of_losses[i] <- losses(call_payoff(X[, i], K), portfolio[251])
}
hist(vector_of_losses)
```

### Numerical approach:
#### Adding parameters which we want to use in an optimal hedging:
```
V0 <- 0.75*call_price(S0, K, r, vol, t, End_Time) #initial capital
p <- 2 #power of the loss function
sim <- generate_scenarios(S0, r, vol) #martingale stock price trajectories
call_const(L1, K, mu, r, vol, p)
const <- Bisection_method_MC(V0, 0, 10^5, r, sim, option_modificate_payoff, drift = mu, vol = vol, p, call_payoff, strike = K) #find const
```

#### Using the finite difference algorithm:
```
I <- 500
ds <- finding_parameters(S0, vol, I, multiplier_price = 5)[1]
dt_f <- finding_parameters(S0, vol, I, multiplier_price = 5)[2]
option_matrix <-  finite_difference_explicit(ds, dt_f, I, r, vol, End_Time, option_modificate_payoff, const, mu, vol, p, call_payoff, K, is_modificate = TRUE)
delta_matrix <- delta_finite_difference(option_matrix, ds)

V <- matrix_interpolation(option_matrix, ds, dt_f, I, X[, 1], time, End_Time) #option price change over time
Xi <- matrix_interpolation(delta_matrix, ds, dt_f, I, X[, 1], time, End_Time) #number of asset needed to hedge over time
```

#### Simple histogram of losses:
Warning: the code takes a few minutes
```
loss <- rep(0, ncol(X))
portfolio <- rep(0, ncol(X))
for (i in 1:length(loss)) {
  V <- matrix_interpolation(option_matrix, ds, dt_f, I, X[, i], time, End_Time)
  Xi <- matrix_interpolation(delta_matrix, ds, dt_f, I, X[, i], time, End_Time)
  portfolio[i] <- portfolio_valuation(V[1], r, Xi, X[, i], initial_capital = 0)[nrow(X)]
  loss[i] <- losses(call_payoff(X[, i], K), portfolio[i])
}
hist(loss)
```
