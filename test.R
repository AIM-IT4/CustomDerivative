context("Testing CustomDerivative class")

test_that("CustomDerivative can be initialized and priced", {
  derivative <- CustomDerivative$new(100, 100, 1, 0.2, 0.05, function(x) max(x - 100, 0))
  price <- derivative$price()
  expect_true(is.numeric(price))
  expect_true(price > 0)
})
