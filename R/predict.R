#' Prediction of Quantiles for Parametric Lifetime Distributions
#'
#' @description
#' This function predicts the quantiles of a two- or three-parameter lifetime
#' distribution using the (log-)location-scale parameterization.
#'
#' @details
#' For a given set of parameters and specified probabilities the quantiles
#' of the chosen model are determined.
#'
#' @param p A numeric vector of probabilities.
#' @param dist_params A vector of parameters. An overview of the
#' distribution-specific parameters can be found in section 'Distributions'.
#' @param distribution Supposed distribution of the random variable.
#'
#' @return A vector with predicted quantiles.
#'
#' @template dist-params
#'
#' @examples
#' # Example 1 - Predicted quantiles for a two-parameter weibull distribution:
#' quants_weib2 <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   dist_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Predicted quantiles for a three-parameter weibull distribution:
#' quants_weib3 <- predict_quantile(
#'   p = c(0.01, 0.1, 0.5),
#'   dist_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )
#'
#' @md
#'
#' @export
predict_quantile <- function(p,
                             dist_params,
                             distribution = c(
                               "weibull", "lognormal", "loglogistic",
                               "sev", "normal", "logistic",
                               "weibull3", "lognormal3", "loglogistic3"
                             )
) {

  distribution <- match.arg(distribution)

  check_dist_params(dist_params, distribution)

  distribution <- two_parametric(distribution)

    # Determine q_p by switching between distributions:
    q_p <- switch(
      distribution,
      "weibull" = ,
      "sev" = qsev(p) * dist_params[[2]] + dist_params[[1]],
      "lognormal" = ,
      "normal" = stats::qnorm(p) * dist_params[[2]] + dist_params[[1]],
      "loglogistic" = ,
      "logistic" = stats::qlogis(p) * dist_params[[2]] + dist_params[[1]]
    )

    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
      q_p <- exp(q_p)
    }

    # Three-parametric models:
    if (length(dist_params) == 3L) {
      q_p <- q_p + dist_params[[3]]
    }

    q_p
}



#' Prediction of Failure Probabilities for Parametric Lifetime Distributions
#'
#' @description
#' This function predicts the (failure) probabilities of a two- or three-parameter
#' lifetime distribution using the (log-)location-scale parameterization.
#'
#' @details
#' For a given set of parameters and specified quantiles the probabilities
#' of the chosen model are determined.
#'
#' @inheritParams predict_quantile
#' @param q A numeric vector of quantiles.
#'
#' @return A vector with predicted (failure) probabilities.
#'
#' @template dist-params
#'
#' @examples
#' # Example 1 - Predicted probabilities for a two-parameter weibull distribution:
#' probs_weib2 <- predict_prob(
#'   q = c(15, 48, 124),
#'   dist_params = c(5, 0.5),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Predicted quantiles for a three-parameter weibull distribution:
#' probs_weib3 <- predict_prob(
#'   q = c(25, 58, 134),
#'   dist_params = c(5, 0.5, 10),
#'   distribution = "weibull3"
#' )
#'
#' @md
#'
#' @export
predict_prob <- function(q,
                         dist_params,
                         distribution = c(
                           "weibull", "lognormal", "loglogistic",
                           "sev", "normal", "logistic",
                           "weibull3","lognormal3", "loglogistic3"
                         )
) {

  distribution <- match.arg(distribution)

  check_dist_params(dist_params, distribution)

  # Standardize:
  z <- standardize(
    x = q, dist_params = dist_params, distribution = distribution
  )

  distribution <- two_parametric(distribution)

  # Determine p_q by switching between distributions:
  p_q <- switch(
    distribution,
    "sev" = ,
    "weibull" = psev(z),
    "normal" = ,
    "lognormal" = stats::pnorm(z),
    "logistic" = ,
    "loglogistic" = stats::plogis(z)
  )

  p_q
}
