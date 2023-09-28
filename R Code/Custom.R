#' Custom Derivative R6 Class


#' An R6 class to create and price custom derivatives.
#'
#' @field underlying_price The underlying asset price.
#' @field strike_price The strike price of the option.
#' @field time_to_maturity Time to maturity of the option.
#' @field volatility The volatility of the underlying asset.
#' @field risk_free_rate The risk-free rate.
#' @field payoff_function The function that determines the payoff of the option.
#'
#' @description
#' This class provides methods to create and price custom derivatives.
#'
#' @details
#' The class uses the Monte Carlo method for pricing. The price method simulates
#' the underlying asset price paths and applies the payoff function to determine
#' the option price.
#'
#' @importFrom R6 R6Class

CustomDerivative <- R6::R6Class(
  "CustomDerivative",
  public = list(
    underlying_price = NULL,
    strike_price = NULL,
    time_to_maturity = NULL,
    volatility = NULL,
    risk_free_rate = NULL,
    payoff_function = NULL,
    #' @param underlying_price Initial price of the underlying asset.
    #' @param strike_price Strike price of the option.
    #' @param time_to_maturity Time to maturity in years.
    #' @param volatility Volatility of the underlying asset.
    #' @param risk_free_rate Risk-free rate (annual).
    #' @param payoff_function A function that calculates the option payoff.
    initialize = function(underlying_price, strike_price, time_to_maturity, volatility, risk_free_rate, payoff_function) {
      self$underlying_price <- underlying_price
      self$strike_price <- strike_price
      self$time_to_maturity <- time_to_maturity
      self$volatility <- volatility
      self$risk_free_rate <- risk_free_rate
      self$payoff_function <- payoff_function
    },
    #' @return Numeric value representing the option price.
    price = function() {
      num_simulations <- 10000
      simulated_prices <- numeric(num_simulations)

      for (i in 1:num_simulations) {
        simulated_prices[i] <- self$underlying_price * exp((self$risk_free_rate - 0.5 * self$volatility^2) * self$time_to_maturity +
                                                      self$volatility * sqrt(self$time_to_maturity) * rnorm(1))
      }

      payoffs <- sapply(simulated_prices, self$payoff_function)

      option_price <- mean(payoffs) * exp(-self$risk_free_rate * self$time_to_maturity)
      return(option_price)
    }
  )
)


