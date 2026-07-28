## Test environments

- GitHub Actions: Ubuntu, R release
- GitHub Actions: Ubuntu, R devel
- GitHub Actions: Windows, R release
- GitHub Actions: macOS, R release

## R CMD check results

The CRAN preparation workflow builds the source package and runs
`R CMD check --as-cran` against the resulting tarball.

## Downstream dependencies

There are currently no known downstream CRAN dependencies.

## Release summary

This update expands the package from the original R6 Monte Carlo prototype into
an extensible pricing and risk-analytics toolkit. It adds analytical
Black-Scholes-Merton prices, variance-reduced Monte Carlo pricing, uncertainty
estimates, finite-difference Greeks, path simulation, and path-dependent payoff
support.

The original `CustomDerivative$new(...)` interface remains available as a
backward-compatible wrapper for users of versions 0.1.x.
