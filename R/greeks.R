#' Finite-difference Greeks for a pricing function
#'
#' The pricing function must accept named arguments `spot`, `maturity`, `rate`,
#' and `volatility`, and return either a numeric price or a `cd_pricing_result`.
#'
#' @param pricer Pricing function.
#' @param spot Current underlying price.
#' @param maturity Time to maturity in years.
#' @param rate Risk-free rate.
#' @param volatility Annualized volatility.
#' @param ... Additional arguments passed to `pricer`.
#' @param spot_bump Relative spot bump.
#' @param volatility_bump Absolute volatility bump.
#' @param rate_bump Absolute rate bump.
#' @param time_bump Time bump in years.
#'
#' @return Named numeric vector containing delta, gamma, vega, rho, and theta.
#' @export
finite_difference_greeks <- function(pricer, spot, maturity, rate, volatility, ...,
                                     spot_bump = 1e-4,
                                     volatility_bump = 1e-4,
                                     rate_bump = 1e-4,
                                     time_bump = 1 / 365) {
  if (!is.function(pricer)) stop("`pricer` must be a function.", call. = FALSE)
  .validate_market(spot, maturity, rate, volatility, 0)

  value <- function(s = spot, t = maturity, r = rate, v = volatility) {
    out <- pricer(spot = s, maturity = t, rate = r, volatility = v, ...)
    if (inherits(out, "cd_pricing_result")) out <- out$price
    .assert_scalar_numeric(out, "pricer output")
    out
  }

  h_s <- max(abs(spot) * spot_bump, .Machine$double.eps^(1 / 3))
  base <- value()
  up_s <- value(s = spot + h_s)
  down_s <- value(s = max(spot - h_s, .Machine$double.eps))

  delta <- (up_s - down_s) / (2 * h_s)
  gamma <- (up_s - 2 * base + down_s) / h_s^2
  vega <- (value(v = volatility + volatility_bump) -
           value(v = max(volatility - volatility_bump, 0))) /
    (volatility_bump + min(volatility_bump, volatility))
  rho <- (value(r = rate + rate_bump) - value(r = rate - rate_bump)) /
    (2 * rate_bump)

  if (maturity > time_bump) {
    theta <- (value(t = maturity - time_bump) - value(t = maturity + time_bump)) /
      (2 * time_bump)
  } else {
    theta <- (value(t = 0) - base) / max(maturity, time_bump)
  }

  c(delta = delta, gamma = gamma, vega = vega, rho = rho, theta = theta)
}
