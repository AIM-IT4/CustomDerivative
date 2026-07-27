.assert_scalar_numeric <- function(x, name, lower = -Inf, strict = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be one finite numeric value.", name), call. = FALSE)
  }
  invalid <- if (strict) x <= lower else x < lower
  if (invalid) {
    operator <- if (strict) ">" else ">="
    stop(sprintf("`%s` must be %s %s.", name, operator, lower), call. = FALSE)
  }
  invisible(x)
}

.assert_count <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < minimum) {
    stop(sprintf("`%s` must be an integer greater than or equal to %d.",
                 name, minimum), call. = FALSE)
  }
  invisible(as.integer(x))
}

.validate_market <- function(spot, maturity, rate, volatility, dividend_yield) {
  .assert_scalar_numeric(spot, "spot", 0, strict = TRUE)
  .assert_scalar_numeric(maturity, "maturity", 0)
  .assert_scalar_numeric(rate, "rate")
  .assert_scalar_numeric(volatility, "volatility", 0)
  .assert_scalar_numeric(dividend_yield, "dividend_yield")
  invisible(TRUE)
}

.validate_payoffs <- function(payoffs, expected_length) {
  if (!is.numeric(payoffs) || length(payoffs) != expected_length) {
    stop("The payoff function must return one numeric value per simulated scenario.",
         call. = FALSE)
  }
  if (any(!is.finite(payoffs))) {
    stop("The payoff function returned non-finite values.", call. = FALSE)
  }
  payoffs
}

.with_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  .assert_count(seed, "seed", 0L)

  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  on.exit({
    if (existed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))
  force(code)
}

.new_pricing_result <- function(price, standard_error = 0, confidence_interval = NULL,
                                n_simulations = 0L, method, diagnostics = list()) {
  if (is.null(confidence_interval)) confidence_interval <- c(price, price)
  structure(
    list(
      price = as.numeric(price),
      standard_error = as.numeric(standard_error),
      confidence_interval = as.numeric(confidence_interval),
      n_simulations = as.integer(n_simulations),
      method = method,
      diagnostics = diagnostics
    ),
    class = "cd_pricing_result"
  )
}

#' @export
print.cd_pricing_result <- function(x, ...) {
  cat(sprintf("Derivative price: %.8f\n", x$price))
  cat(sprintf("Method: %s\n", x$method))
  if (x$n_simulations > 0L) {
    cat(sprintf("Monte Carlo standard error: %.8f\n", x$standard_error))
    cat(sprintf("Confidence interval: [%.8f, %.8f]\n",
                x$confidence_interval[1L], x$confidence_interval[2L]))
    cat(sprintf("Simulations: %d\n", x$n_simulations))
  }
  invisible(x)
}
