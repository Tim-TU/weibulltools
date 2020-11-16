#' Estimation of Quantiles for Parametric Lifetime Distributions
#'
#' This function estimates the quantiles for a given set of estimated
#' location-scale (and threshold) parameters and specified failure probabilities.
#'
#' @param p A numeric vector which consists of failure probabilities
#'   regarding the lifetime data.
#' @param loc_sc_params A (named) numeric vector of estimated location
#'   and scale parameters for a specified distribution. The order of
#'   elements is important. First entry needs to be the location
#'   parameter \eqn{\mu} and the second element needs to be the scale
#'   parameter \eqn{\sigma}. If a three-parametric model is used the third element
#'   is the threshold parameter \eqn{\gamma}.
#' @param distribution Supposed distribution of the random variable.
#'
#' @return A vector containing the estimated quantiles for a given set of
#'   failure probabilities and estimated parameters.
#'
#' @export
#'
#' @examples
#' # Example 1: Predicted quantiles for two-parameter Weibull:
#' quants <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   loc_sc_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2: Predicted quantiles for three-parameter Weibull:
#' quants_weib3 <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   loc_sc_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )

predict_quantile <- function(p, loc_sc_params,
                             distribution = c("weibull", "lognormal", "loglogistic",
                                              "normal", "logistic", "sev", "weibull3",
                                              "lognormal3", "loglogistic3")) {

  distribution <- match.arg(distribution)

  # Log-Location-Scale Distributions:
  if (distribution == "weibull") {
    quantiles <- exp(SPREDA::qsev(p) * loc_sc_params[[2]] + loc_sc_params[[1]])
  }
  if (distribution == "lognormal") {
    quantiles <- exp(stats::qnorm(p) * loc_sc_params[[2]] + loc_sc_params[[1]])
  }
  if (distribution == "loglogistic") {
    quantiles <- exp(stats::qlogis(p) * loc_sc_params[[2]] + loc_sc_params[[1]])
  }
  # Log-Location-Scale Distributions with Threshold:
  if (distribution == "weibull3") {
    quantiles <- exp(SPREDA::qsev(p) * loc_sc_params[[2]] + loc_sc_params[[1]]) +
      loc_sc_params[[3]]
  }
  if (distribution == "lognormal3") {
    quantiles <- exp(stats::qnorm(p) * loc_sc_params[[2]] + loc_sc_params[[1]]) +
      loc_sc_params[[3]]
  }
  if (distribution == "loglogistic3") {
    quantiles <- exp(stats::qlogis(p) * loc_sc_params[[2]] + loc_sc_params[[1]]) +
      loc_sc_params[[3]]
  }
  # Location-Scale Distributions:
  if (distribution == "sev") {
    quantiles <- SPREDA::qsev(p) * loc_sc_params[[2]] + loc_sc_params[[1]]
  }
  if (distribution == "normal") {
    quantiles <- stats::qnorm(p) * loc_sc_params[[2]] + loc_sc_params[[1]]
  }
  if (distribution == "logistic") {
    quantiles <- stats::qlogis(p) * loc_sc_params[[2]] + loc_sc_params[[1]]
  }

  x_pred <- quantiles
  return(x_pred)
}


#' Estimation of Failure Probabilities for Parametric Lifetime Distributions
#'
#' This function estimates the failure probabilities for a given set of estimated
#' location-scale (and threshold) parameters and specified quantiles.
#'
#' @param q A numeric vector which consists of lifetime data.
#' @inheritParams predict_quantile
#'
#' @return A vector containing the estimated failure probabilities for a given
#'   set of quantiles and estimated parameters.
#' @export
#'
#' @examples
#' # Example 1: Predicted probabilities for two-parameter Weibull:
#' probs <- predict_prob(
#'   q = c(15, 48, 124),
#'   loc_sc_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2: Predicted probabilities for three-parameter Weibull:
#' probs_weib3 <- predict_prob(
#'   q = c(25, 58, 134),
#'   loc_sc_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )

predict_prob <- function(q, loc_sc_params,
                         distribution = c("weibull", "lognormal", "loglogistic",
                                          "normal", "logistic", "sev", "weibull3",
                                          "lognormal3", "loglogistic3")) {

  distribution <- match.arg(distribution)

  # Log-Location-Scale Distributions:
  if (distribution == "weibull") {
    # Standardize:
    z <- (log(q) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- SPREDA::psev(z)
  }
  if (distribution == "lognormal") {
    # Standardize:
    z <- (log(q) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::pnorm(z)
  }
  if (distribution == "loglogistic") {
    # Standardize:
    z <- (log(q) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::plogis(z)
  }
  # Log-Location-Scale Distributions with Threshold:
  if (distribution == "weibull3") {
    # Standardize:
    z <- (log(q - loc_sc_params[[3]]) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- SPREDA::psev(z)
  }
  if (distribution == "lognormal3") {
    # Standardize:
    z <- (log(q - loc_sc_params[[3]]) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::pnorm(z)
  }
  if (distribution == "loglogistic3") {
    # Standardize:
    z <- (log(q - loc_sc_params[[3]]) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::plogis(z)
  }
  # Location-Scale Distributions:
  if (distribution == "sev") {
    # Standardize:
    z <- (q - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- SPREDA::psev(z)
  }
  if (distribution == "normal") {
    # Standardize:
    z <- (q - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::pnorm(z)
  }
  if (distribution == "logistic") {
    # Standardize:
    z <- (q - loc_sc_params[[1]]) / loc_sc_params[[2]]
    cdf <- stats::plogis(z)
  }

  y_pred <- cdf
  return(y_pred)
}
