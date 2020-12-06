#' Prediction of Quantiles for Parametric Lifetime Distributions
#'
#' @description
#' This function predicts the quantiles of two- or three-parametric lifetime
#' distributions that belong to the (log-)location-scale family.
#'
#' @details
#' For a given set of parameters and specified probabilities the quantiles
#' of the chosen model are determined.
#'
#' @param p A numeric vector of probabilities.
#' @param loc_sc_params A (named) numeric vector of (log-)location-scale parameters
#'   in the order of location (\eqn{\mu}) and scale (\eqn{\sigma}). If a
#'   three-parametric model is selected, the threshold parameter (\eqn{\gamma})
#'   has to be the third element.
#' @param distribution Supposed distribution of the random variable.
#'
#' @return A vector with predicted quantiles.
#'
#' @examples
#' # Example 1 - Predicted quantiles for a two-parameter weibull distribution:
#' quants_weib2 <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   loc_sc_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Predicted quantiles for a three-parameter weibull distribution:
#' quants_weib3 <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   loc_sc_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )
#'
#' @export
predict_quantile <- function(p,
                             loc_sc_params,
                             distribution = c(
                               "weibull", "lognormal", "loglogistic",
                               "normal", "logistic", "sev",
                               "weibull3", "lognormal3", "loglogistic3"
                             )
) {

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

  return(quantiles)
}



#' Prediction of Failure Probabilities for Parametric Lifetime Distributions
#'
#' @description
#' This function predicts the (failure) probabilities of two- or three-parametric
#' lifetime distributions that belong to the (log-)location-scale family.
#'
#' @details
#' For a given set of parameters and specified quantiles the (failure) probabilities
#' of the chosen model are determined.
#'
#' @inheritParams predict_quantile
#'
#' @param q A numeric vector of quantiles.
#'
#' @return A vector with predicted (failure) probabilities.
#'
#' @examples
#' # Example 1 - Predicted probabilities for a two-parameter weibull distribution:
#' probs_weib2 <- predict_prob(
#'   q = c(15, 48, 124),
#'   loc_sc_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Predicted quantiles for a three-parameter weibull distribution:
#' probs_weib3 <- predict_prob(
#'   q = c(25, 58, 134),
#'   loc_sc_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )
#'
#' @export
predict_prob <- function(q,
                         loc_sc_params,
                         distribution = c(
                           "weibull", "lognormal", "loglogistic",
                           "normal", "logistic", "sev",
                           "weibull3","lognormal3", "loglogistic3"
                         )
) {

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

  return(cdf)
}
