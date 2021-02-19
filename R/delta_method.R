#' Delta Method for Parametric Lifetime Distributions
#'
#' @description
#' This function applies the delta method to a two- or three-parameter lifetime
#' distribution that belong to the (log-)location-scale family.
#'
#' @details
#' The delta method estimates the standard errors for quantities that can be
#' written as non-linear functions of ML estimators. Hence, (log-)location-scale
#' parameters as well as the variance-covariance matrix of these have to be estimated
#' with [maximum likelihood][ml_estimation].
#'
#' The estimated standard errors are needed to calculate Fisher's (normal
#' approximation) confidence intervals. For confidence bounds on the probability,
#' standard errors of the standardized quantiles (`direction = "y"`)
#' have to be computed (*z-procedure*) and for bounds on quantiles, standard errors
#' of quantiles (`direction = "x"`) are required. For more information see
#' [confint_fisher].
#'
#' @param p A numeric vector of probabilities or quantiles. If the standard errors
#' of quantiles should be determined the corresponding probabilities have to be
#' specified, and if the standard errors of standardized quantiles (z-values)
#' should be computed corresponding quantiles are required.
#' @param dist_params The parameters (`coefficients`) returned by [ml_estimation].
#' @param dist_varcov The variance-covariance-matrix (`varcov`) returned by
#' [ml_estimation].
#' @param distribution Supposed distribution of the random variable. Has to be in
#' line with the specification made in [ml_estimation].
#' @param direction A character string specifying for which quantity the standard
#' errors are calculated. One of `"y"` (if `p` are quantiles) or `"x"` (if `p`
#' are probabilities).
#'
#' @return A numeric vector of estimated standard errors for quantiles or
#'   standardized quantiles (*z-values*).
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @examples
#' # Reliability data preparation:
#' data <- reliability_data(
#'   shock,
#'   x = distance,
#'   status = status
#' )
#'
#' # Parameter estimation using maximum likelihood:
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' # Example 1 - Standard errors of standardized quantiles:
#' delta_y <- delta_method(
#'   p = shock$distance,
#'   dist_params = mle$coefficients,
#'   dist_varcov = mle$varcov,
#'   distribution = "weibull",
#'   direction = "y"
#' )
#'
#' # Example 2 - Standard errors of quantiles:
#' delta_x <- delta_method(
#'   p = seq(0.01, 0.99, 0.01),
#'   dist_params = mle$coefficients,
#'   dist_varcov = mle$varcov,
#'   distribution = "weibull",
#'   direction = "x"
#' )
#'
#' @md
#'
#' @export
delta_method <- function(p,
                         dist_params,
                         dist_varcov,
                         distribution = c(
                           "weibull", "lognormal", "loglogistic",
                           "normal", "logistic", "sev",
                           "weibull3", "lognormal3", "loglogistic3"
                         ),
                         direction = c("y", "x")
) {

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  dm_vectorized <- Vectorize(delta_method_, "p")

  dm_vectorized(
    p = p,
    dist_params = dist_params,
    dist_varcov = dist_varcov,
    distribution = distribution,
    direction = direction
  )
}



delta_method_ <- function(p,
                          dist_params,
                          dist_varcov,
                          distribution = c(
                            "weibull", "lognormal", "loglogistic",
                            "sev", "normal", "logistic",
                            "weibull3", "lognormal3", "loglogistic3"
                          ),
                          direction = c("y", "x")
) {

  # Standard Errors for quantiles:
  if (direction == "x") {

    # z-quantile, i.e. standardized random variable:
    if (distribution %in% c("weibull", "weibull3", "sev")) {
      z <- SPREDA::qsev(p)
    }
    if (distribution %in% c("lognormal", "lognormal3", "normal")) {
      z <- stats::qnorm(p)
    }
    if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
      z <- stats::qlogis(p)
    }

    # First derivatives of quantile functions regarding parameters:
    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
      ## Quantile for specified distribution:
      q <- predict_quantile(
        p = p,
        dist_params = dist_params,
        distribution = distribution
      )

      dq_dmu <- q
      dq_dsc <- z * q
      dq_dpar <- c(dq_dmu, dq_dsc)
    }
    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      # Quantile for specified two-parameter distribution (gamma is irrelevant; see derivatives):
      q <- predict_quantile(
        p = p,
        dist_params = dist_params[-3],
        distribution = two_parametric(distribution)
      )

      dq_dmu <- q
      dq_dsc <- z * q
      dq_dgam <- 1
      dq_dpar <- c(dq_dmu, dq_dsc, dq_dgam)
    }
    if (distribution %in% c("sev", "normal", "logistic")) {
      ## Quantile for specified distribution:
      dq_dmu <- 1
      dq_dsc <- z
      dq_dpar <- c(dq_dmu, dq_dsc)
    }

    # Variance and standard error of quantiles:
    var_q <- t(dq_dpar) %*% dist_varcov %*% dq_dpar
    std_err <- sqrt(var_q)

    # Standard Errors for z: The "z-Procedure":
  } else {
    # Standardized Random Variable:
    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
      z <- (log(p) - dist_params[[1]]) / dist_params[[2]]
    }
    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      z <- (log(p - dist_params[[3]]) - dist_params[[1]]) / dist_params[[2]]
    }
    if (distribution %in% c("sev", "normal", "logistic")) {
      z <- (p - dist_params[[1]]) / dist_params[[2]]
    }

    # First derivatives of z regarding parameters:
    if (distribution %in% c("weibull", "lognormal", "loglogistic", "sev",
                            "normal", "logistic")) {
      dz_dmu <- (-1 / dist_params[[2]])
      dz_dsc <- (-1 / dist_params[[2]]) * z
      dz_dpar <- c(dz_dmu, dz_dsc)
    }

    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      dz_dmu <- (-1 / dist_params[[2]])
      dz_dsc <- (-1 / dist_params[[2]]) * z
      dz_dgam <- (1 / dist_params[[2]]) * (1 / (dist_params[[3]] - p))
      dz_dpar <- c(dz_dmu, dz_dsc, dz_dgam)
    }

    var_z <- t(dz_dpar) %*% dist_varcov %*% dz_dpar
    std_err <- sqrt(var_z)
  }

  return(std_err)
}
