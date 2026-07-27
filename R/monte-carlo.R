#' Monte Carlo price for a European custom payoff
#'
#' Prices a terminal-value payoff under risk-neutral geometric Brownian motion.
#' The estimator can use antithetic variates and a discounted terminal-underlying
#' control variate whose expectation is known analytically.
#'
#' @param payoff Vectorized function of terminal prices.
#' @param spot Current underlying price.
#' @param maturity Time to maturity in years.
#' @param rate Continuously compounded risk-free rate.
#' @param volatility Annualized volatility.
#' @param dividend_yield Continuous dividend yield.
#' @param n_simulations Number of Monte Carlo scenarios.
#' @param seed Optional integer random seed. The caller's RNG state is restored.
#' @param antithetic Whether to use antithetic normal variates.
#' @param control_variate Whether to use the discounted terminal underlying as a
#'   control variate.
#' @param confidence_level Confidence level for the normal-approximation interval.
#'
#' @return An object of class `cd_pricing_result`.
#' @export
price_european_mc <- function(payoff, spot, maturity, rate, volatility,
                              dividend_yield = 0,
                              n_simulations = 100000L,
                              seed = NULL,
                              antithetic = TRUE,
                              control_variate = TRUE,
                              confidence_level = 0.95) {
  if (!is.function(payoff)) stop("`payoff` must be a function.", call. = FALSE)
  .validate_market(spot, maturity, rate, volatility, dividend_yield)
  .assert_count(n_simulations, "n_simulations", 2L)
  .assert_scalar_numeric(confidence_level, "confidence_level", 0, strict = TRUE)
  if (confidence_level >= 1) {
    stop("`confidence_level` must be strictly less than 1.", call. = FALSE)
  }

  if (maturity == 0) {
    value <- .validate_payoffs(payoff(spot), 1L)
    return(.new_pricing_result(value, method = "maturity payoff"))
  }

  n <- as.integer(n_simulations)

  estimator <- .with_seed(seed, {
    if (isTRUE(antithetic)) {
      half_n <- ceiling(n / 2)
      normals <- stats::rnorm(half_n)
      normals <- c(normals, -normals)[seq_len(n)]
    } else {
      normals <- stats::rnorm(n)
    }

    terminal_prices <- spot * exp(
      (rate - dividend_yield - 0.5 * volatility^2) * maturity +
        volatility * sqrt(maturity) * normals
    )
    raw_payoffs <- .validate_payoffs(payoff(terminal_prices), n)
    discount_factor <- exp(-rate * maturity)
    discounted_payoffs <- discount_factor * raw_payoffs

    beta <- 0
    variance_reduction <- 1
    adjusted_payoffs <- discounted_payoffs

    if (isTRUE(control_variate) && volatility > 0) {
      control <- discount_factor * terminal_prices
      expected_control <- spot * exp(-dividend_yield * maturity)
      control_variance <- stats::var(control)
      if (is.finite(control_variance) && control_variance > 0) {
        beta <- stats::cov(discounted_payoffs, control) / control_variance
        adjusted_payoffs <- discounted_payoffs - beta * (control - expected_control)
        raw_variance <- stats::var(discounted_payoffs)
        adjusted_variance <- stats::var(adjusted_payoffs)
        if (is.finite(raw_variance) && raw_variance > 0 &&
            is.finite(adjusted_variance)) {
          variance_reduction <- raw_variance / adjusted_variance
        }
      }
    }

    estimate <- mean(adjusted_payoffs)
    standard_error <- stats::sd(adjusted_payoffs) / sqrt(n)
    critical <- stats::qnorm(0.5 + confidence_level / 2)

    .new_pricing_result(
      price = estimate,
      standard_error = standard_error,
      confidence_interval = estimate + c(-1, 1) * critical * standard_error,
      n_simulations = n,
      method = "risk-neutral GBM Monte Carlo",
      diagnostics = list(
        antithetic = isTRUE(antithetic),
        control_variate = isTRUE(control_variate),
        control_beta = beta,
        variance_reduction_ratio = variance_reduction,
        confidence_level = confidence_level
      )
    )
  })

  estimator
}
