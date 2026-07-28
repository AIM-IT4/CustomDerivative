# CustomDerivative 0.2.0

## Major redesign

- Replaced the original R6 prototype with an idiomatic functional pricing API.
- Added analytical Black-Scholes-Merton prices for European calls and puts.
- Added European Monte Carlo pricing with antithetic and control variates.
- Added Monte Carlo standard errors, confidence intervals, and diagnostics.
- Added risk-neutral geometric Brownian motion path simulation.
- Added path-dependent pricing with Asian and down-and-out call payoff helpers.
- Added generic finite-difference delta, gamma, vega, rho, and theta.
- Added strict input and payoff validation.
- Added deterministic seeded simulations that preserve caller RNG state.
- Added numerical benchmark, parity, reproducibility, and exotic-option tests.
- Added cross-platform GitHub Actions checks for R release and R devel.
- Corrected package installation and usage documentation.
