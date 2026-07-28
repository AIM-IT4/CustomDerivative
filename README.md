# CustomDerivative

`CustomDerivative` is an R package for transparent derivative pricing and risk
analytics. It combines analytical Black-Scholes pricing with extensible Monte
Carlo engines for terminal and path-dependent payoffs.

## Capabilities

- Black-Scholes-Merton pricing for European calls and puts
- continuous dividend yields
- user-defined vectorized terminal payoff functions
- antithetic variates and control variates
- Monte Carlo standard errors and confidence intervals
- geometric Brownian motion path simulation
- Asian and discretely monitored barrier payoff helpers
- generic finite-difference delta, gamma, vega, rho, and theta
- input validation and reproducible simulation
- cross-platform `R CMD check` through GitHub Actions

## Installation

```r
install.packages("pak")
pak::pak("AIM-IT4/CustomDerivative")
```

For the development branch:

```r
pak::pak("AIM-IT4/CustomDerivative@agent/advanced-derivatives-engine")
```

## Analytical European option

```r
library(CustomDerivative)

black_scholes_price(
  spot = 100,
  strike = 100,
  maturity = 1,
  rate = 0.05,
  volatility = 0.20,
  type = "call"
)
```

## Custom European payoff with Monte Carlo

The package prices a payoff \(g(S_T)\) as

\[
V_0 = e^{-rT}\mathbb{E}^{\mathbb{Q}}[g(S_T)],
\]

under risk-neutral geometric Brownian motion.

```r
result <- price_european_mc(
  payoff = call_payoff(100),
  spot = 100,
  maturity = 1,
  rate = 0.05,
  volatility = 0.20,
  n_simulations = 100000,
  seed = 42
)

result
result$diagnostics$variance_reduction_ratio
```

A custom digital payoff can be supplied directly:

```r
digital <- function(terminal_price) {
  100 * as.numeric(terminal_price > 110)
}

price_european_mc(
  payoff = digital,
  spot = 100,
  maturity = 1,
  rate = 0.05,
  volatility = 0.20,
  seed = 42
)
```

## Path-dependent derivative

```r
asian <- price_path_dependent_mc(
  payoff = asian_call_payoff(strike = 100),
  spot = 100,
  maturity = 1,
  rate = 0.05,
  volatility = 0.20,
  n_steps = 252,
  n_simulations = 20000,
  seed = 42
)

asian
```

## Greeks

```r
call_pricer <- function(spot, maturity, rate, volatility) {
  black_scholes_price(
    spot = spot,
    strike = 100,
    maturity = maturity,
    rate = rate,
    volatility = volatility,
    type = "call"
  )
}

finite_difference_greeks(
  pricer = call_pricer,
  spot = 100,
  maturity = 1,
  rate = 0.05,
  volatility = 0.20
)
```

## Model scope

The current simulation model assumes a single tradable underlying following
risk-neutral geometric Brownian motion with constant volatility, interest rate,
and dividend yield. Path-dependent claims are monitored on a discrete grid.
The package does not yet implement early exercise, stochastic volatility,
jump diffusion, or multi-asset correlation models.

## Development

```r
install.packages(c("devtools", "testthat"))
devtools::document()
devtools::test()
devtools::check()
```

## License

MIT. Copyright Amit Kumar Jha.
