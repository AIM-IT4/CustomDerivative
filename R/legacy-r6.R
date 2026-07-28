#' Legacy CustomDerivative R6 interface
#'
#' Backward-compatible wrapper for users of versions 0.1.x. New code should
#' generally use [price_european_mc()] and the payoff helper functions directly.
#'
#' @importFrom R6 R6Class
#' @export
CustomDerivative <- R6Class(
  "CustomDerivative",
  public = list(
    underlying_price = NULL,
    strike_price = NULL,
    time_to_maturity = NULL,
    volatility = NULL,
    risk_free_rate = NULL,
    payoff_function = NULL,

    #' @description Create a legacy custom derivative object.
    #' @param underlying_price Initial underlying price.
    #' @param strike_price Strike price retained for compatibility.
    #' @param time_to_maturity Time to maturity in years.
    #' @param volatility Annualized volatility.
    #' @param risk_free_rate Continuously compounded risk-free rate.
    #' @param payoff_function Vectorized terminal payoff function.
    initialize = function(underlying_price, strike_price, time_to_maturity,
                          volatility, risk_free_rate, payoff_function) {
      .validate_market(underlying_price, time_to_maturity, risk_free_rate,
                       volatility, 0)
      .assert_scalar_numeric(strike_price, "strike_price", 0, strict = TRUE)
      if (!is.function(payoff_function)) {
        stop("`payoff_function` must be a function.", call. = FALSE)
      }

      self$underlying_price <- underlying_price
      self$strike_price <- strike_price
      self$time_to_maturity <- time_to_maturity
      self$volatility <- volatility
      self$risk_free_rate <- risk_free_rate
      self$payoff_function <- payoff_function
    },

    #' @description Price the derivative using risk-neutral Monte Carlo.
    #' @param n_simulations Number of simulations.
    #' @param seed Optional random seed.
    #' @return Numeric derivative price.
    price = function(n_simulations = 10000L, seed = NULL) {
      result <- price_european_mc(
        payoff = self$payoff_function,
        spot = self$underlying_price,
        maturity = self$time_to_maturity,
        rate = self$risk_free_rate,
        volatility = self$volatility,
        n_simulations = n_simulations,
        seed = seed
      )
      result$price
    }
  )
)
