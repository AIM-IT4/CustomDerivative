#' Black-Scholes price for a European option
#'
#' Computes the analytical Black-Scholes-Merton price of a European call or put
#' with a continuous dividend yield.
#'
#' @param spot Current underlying price.
#' @param strike Strike price.
#' @param maturity Time to maturity in years.
#' @param rate Continuously compounded risk-free rate.
#' @param volatility Annualized volatility.
#' @param dividend_yield Continuous dividend yield.
#' @param type Either `"call"` or `"put"`.
#'
#' @return A numeric option price.
#' @export
black_scholes_price <- function(spot, strike, maturity, rate, volatility,
                                dividend_yield = 0,
                                type = c("call", "put")) {
  type <- match.arg(type)
  .validate_market(spot, maturity, rate, volatility, dividend_yield)
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)

  if (maturity == 0) {
    return(if (type == "call") max(spot - strike, 0) else max(strike - spot, 0))
  }

  if (volatility == 0) {
    forward_terminal <- spot * exp((rate - dividend_yield) * maturity)
    payoff <- if (type == "call") {
      max(forward_terminal - strike, 0)
    } else {
      max(strike - forward_terminal, 0)
    }
    return(exp(-rate * maturity) * payoff)
  }

  root_t <- sqrt(maturity)
  d1 <- (log(spot / strike) +
         (rate - dividend_yield + 0.5 * volatility^2) * maturity) /
    (volatility * root_t)
  d2 <- d1 - volatility * root_t

  if (type == "call") {
    spot * exp(-dividend_yield * maturity) * stats::pnorm(d1) -
      strike * exp(-rate * maturity) * stats::pnorm(d2)
  } else {
    strike * exp(-rate * maturity) * stats::pnorm(-d2) -
      spot * exp(-dividend_yield * maturity) * stats::pnorm(-d1)
  }
}

#' Standard terminal payoff functions
#'
#' @param strike Strike price.
#' @param cash Cash amount paid by the digital call when in the money.
#' @return A vectorized payoff function accepting terminal prices.
#' @name payoff_helpers
NULL

#' @rdname payoff_helpers
#' @export
call_payoff <- function(strike) {
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)
  force(strike)
  function(terminal_price) pmax(terminal_price - strike, 0)
}

#' @rdname payoff_helpers
#' @export
put_payoff <- function(strike) {
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)
  force(strike)
  function(terminal_price) pmax(strike - terminal_price, 0)
}

#' @rdname payoff_helpers
#' @export
digital_call_payoff <- function(strike, cash = 1) {
  .assert_scalar_numeric(strike, "strike", 0, strict = TRUE)
  .assert_scalar_numeric(cash, "cash", 0)
  force(strike)
  force(cash)
  function(terminal_price) cash * as.numeric(terminal_price > strike)
}
