test_that("Black-Scholes reproduces the standard benchmark", {
  expect_equal(
    black_scholes_price(100, 100, 1, 0.05, 0.20, type = "call"),
    10.45058357,
    tolerance = 1e-7
  )
  expect_equal(
    black_scholes_price(100, 100, 1, 0.05, 0.20, type = "put"),
    5.57352602,
    tolerance = 1e-7
  )
})

test_that("call-put parity holds", {
  call <- black_scholes_price(100, 105, 1.5, 0.04, 0.25,
                              dividend_yield = 0.01, type = "call")
  put <- black_scholes_price(100, 105, 1.5, 0.04, 0.25,
                             dividend_yield = 0.01, type = "put")
  parity <- 100 * exp(-0.01 * 1.5) - 105 * exp(-0.04 * 1.5)
  expect_equal(call - put, parity, tolerance = 1e-10)
})

test_that("Monte Carlo estimate is statistically consistent with analytical value", {
  result <- price_european_mc(
    payoff = call_payoff(100),
    spot = 100,
    maturity = 1,
    rate = 0.05,
    volatility = 0.20,
    n_simulations = 50000,
    seed = 42
  )
  benchmark <- black_scholes_price(100, 100, 1, 0.05, 0.20)
  standardized_error <- abs(result$price - benchmark) / result$standard_error

  expect_s3_class(result, "cd_pricing_result")
  expect_true(is.finite(standardized_error))
  expect_lt(standardized_error, 4)
  expect_gt(result$standard_error, 0)
  expect_gt(result$diagnostics$variance_reduction_ratio, 1)
})

test_that("seeded simulation is reproducible without changing caller RNG", {
  set.seed(999)
  before <- .Random.seed
  first <- price_european_mc(call_payoff(100), 100, 1, 0.05, 0.2,
                             n_simulations = 1000, seed = 123)
  expect_identical(.Random.seed, before)
  second <- price_european_mc(call_payoff(100), 100, 1, 0.05, 0.2,
                              n_simulations = 1000, seed = 123)
  expect_equal(first$price, second$price)
})

test_that("path-dependent Asian option is priced", {
  result <- price_path_dependent_mc(
    payoff = asian_call_payoff(100),
    spot = 100,
    maturity = 1,
    rate = 0.05,
    volatility = 0.20,
    n_steps = 12,
    n_simulations = 5000,
    seed = 7
  )
  expect_s3_class(result, "cd_pricing_result")
  expect_gt(result$price, 0)
  expect_equal(result$diagnostics$n_steps, 12L)
})

test_that("invalid market inputs fail clearly", {
  expect_error(black_scholes_price(-1, 100, 1, 0.05, 0.2), "spot")
  expect_error(black_scholes_price(100, 100, -1, 0.05, 0.2), "maturity")
  expect_error(black_scholes_price(100, 100, 1, 0.05, -0.2), "volatility")
})
