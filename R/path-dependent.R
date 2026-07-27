#' Simulate geometric Brownian motion paths
#'
#' @param spot Current underlying price.
#' @param maturity Time horizon in years.
#' @param rate Continuously compounded risk-free rate.
#' @param volatility Annualized volatility.
#' @param dividend_yield Continuous dividend yield.
#' @param n_steps Number of monitoring intervals.
#' @param n_simulations Number of paths.
#' @param seed Optional integer random seed.
#' @param antithetic Whether to use antithetic normal innovations.
#'
#' @return A matrix with one path per row, including the initial price in column 1.
#' @export
simulate_gbm_paths <- function(spot, maturity, rate, volatility,
                               dividend_yield = 0,
                               n_steps = 252L,
                               n_simulations = 10000L,
                               seed = NULL,
                               antithetic = TRUE) {
  .validate_market(spot, maturity, rate, volatility, dividend_yield)
  .assert_count(n_steps, "n_steps", 1L)
  .assert_count(n_simulations, "n_simulations", 1L)

  n_steps <- as.integer(n_steps)
  n_simulations <- as.integer(n_simulations)

  if (maturity == 0) {
    return(matrix(spot, nrow = n_simulations, ncol = n_steps + 1L))
  }

  .with_seed(seed, {
    if (isTRUE(antithetic)) {
      half_n <- ceiling(n_simulations / 2)
      z_half <- matrix(stats::rnorm(half_n * n_steps),
                       nrow = half_n, ncol = n_steps)
      innovations <- rbind(z_half, -z_half)[seq_len(n_simulations), , drop = FALSE]
    } else {
      innovations <- matrix(stats::rnorm(n_simulations * n_steps),
                            nrow = n_simulations, ncol = n_steps)
    }

    dt <- maturity / n_steps
    log_increments <-
      (rate - dividend_yield - 0.5 * volatility^2) * dt +
      volatility * sqrt(dt) * innovations
    log_paths <- t(apply(log_increments, 1L, cumsum))
    cbind(spot, spot * exp(log_paths))
  })
}

#' Monte Carlo price for a path-dependent custom payoff
#'
#' @param payoff Function accepting a path matrix and returning one payoff per row.
#' @inheritParams simulate_gbm_paths
#' @param confidence_level Confidence level for the normal-approximation interval.
#'
#' @return An object of class `cd_pricing_result`.
#' @export
price_path_dependent_mc <- function(payoff, spot, maturity, rate, volatility,
                                    dividend_yield = 0,
                                    n_steps = 252L,
                                    n_simulations = 10000L,
                                    seed = NULL,
                                    antithetic = TRUE,
                                    confidence_level = 0.95) {
  if (!is.function(payoff)) stop("`payoff` must be a function.", call. = FALSE)
  .assert_scalar_numeric(confidence_level, "confidence_level", 0, strict = TRUE)
  if (confidence_level >= 1) {
    stop("`confidence_level` must be strictly less than 1.", call. = FALSE)
  }

  paths <- simulate_gbm_paths(
    spot = spot,
    maturity = maturity,
    rate = rate,
    volatility = volatility,
    dividend_yield = dividend_yield,
    n_steps = n_steps,
    n_simulations = n_simulations,
    seed = seed,
    antithetic = antithetic
  )

  n <- nrow(paths)
  raw_payoffs <- .validate_payoffs(payoff(paths), n)
  discounted <- exp(-rate * maturity) * raw_payoffs
  estimate <- mean(discounted)
  standard_error <- if (n > 1L) stats::sd(discounted) / sqrt(n) else 0
  critical <- stats::qnorm(0.5 + confidence_level / 2)

  .new_pricing_result(
    price = estimate,
    standard_error = standard_error,
    confidence_interval = estimate + c(-1, 1) * critical * standard_error,
    n_simulations = n,
    method = "path-dependent risk-neutral GBM Monte Carlo",
    diagnostics = list(
      n_steps = as.integer(n_steps),
      antithetic = isTRUE(antithetic),
      confidence_level = confidence_level
    )
  )
}

#' Arithmetic-average Asian call payoff
#'
#' @param strike Strike price.
#' @param include_spot Whether the initial spot column participates in the average.
#' @return A payoff function accepting a path matrix.
#' @export
asian_call_payoff <- function(strike, include_spot = FALSE) {
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)
  force(strike)
  force(include_spot)
  function(paths) {
    monitored <- if (isTRUE(include_spot)) paths else paths[, -1L, drop = FALSE]
    pmax(rowMeans(monitored) - strike, 0)
  }
}

#' Down-and-out European call payoff
#'
#' @param strike Strike price.
#' @param barrier Lower barrier level.
#' @return A payoff function accepting a path matrix.
#' @export
down_and_out_call_payoff <- function(strike, barrier) {
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)
  .assert_scalar_numeric(barrier, "barrier", 0, strict = TRUE)
  force(strike)
  force(barrier)
  function(paths) {
    alive <- apply(paths, 1L, min) > barrier
    alive * pmax(paths[, ncol(paths)] - strike, 0)
  }
}
