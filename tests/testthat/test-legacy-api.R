test_that("legacy R6 interface remains available", {
  derivative <- CustomDerivative$new(
    underlying_price = 100,
    strike_price = 100,
    time_to_maturity = 1,
    volatility = 0.2,
    risk_free_rate = 0.05,
    payoff_function = call_payoff(100)
  )

  price <- derivative$price(n_simulations = 20000, seed = 42)

  expect_type(price, "double")
  expect_gt(price, 0)
  expect_equal(price, black_scholes_price(100, 100, 1, 0.05, 0.2), tolerance = 0.2)
})
