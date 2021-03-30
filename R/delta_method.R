#' Delta Method for Parametric Lifetime Distributions
#'
#' @description
#' This function applies the *delta method* to a parametric lifetime distribution.
#'
#' @details
#' The delta method estimates the standard errors for quantities that can be
#' written as non-linear functions of ML estimators. Hence, the parameters as
#' well as the variance-covariance matrix of these quantities have to be estimated
#' with [maximum likelihood][ml_estimation].
#'
#' The estimated standard errors are used to calculate Fisher's (normal
#' approximation) confidence intervals. For confidence bounds on the probability,
#' standard errors of the standardized quantiles (`direction = "y"`) have to be
#' computed (*z-procedure*) and for bounds on quantiles, standard errors of
#' quantiles (`direction = "x"`) are required. For more information see
#' [confint_fisher].
#'
#' @param x A numeric vector of probabilities or quantiles. If the standard errors
#' of quantiles should be determined the corresponding probabilities have to be
#' specified, and if the standard errors of standardized quantiles (z-values)
#' should be computed corresponding quantiles are required.
#' @param dist_params The parameters (`coefficients`) returned by [ml_estimation].
#' @param dist_varcov The variance-covariance-matrix (`varcov`) returned by
#' [ml_estimation].
#' @param distribution Supposed distribution of the random variable. Has to be in
#' line with the specification made in [ml_estimation].
#' @param direction A character string specifying for which quantity the standard
#' errors are calculated. `"y"` if `x` are quantiles or `"x"` if `x` are probabilities.
#' @param p `r lifecycle::badge("soft-deprecated")`: Use `x` instead.
#'
#' @return A numeric vector of estimated standard errors for quantiles or
#' standardized quantiles (*z-values*).
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#' reliability data, New York: Wiley series in probability and statistics, 1998
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
#'   x = shock$distance,
#'   dist_params = mle$coefficients,
#'   dist_varcov = mle$varcov,
#'   distribution = "weibull",
#'   direction = "y"
#' )
#'
#' # Example 2 - Standard errors of quantiles:
#' delta_x <- delta_method(
#'   x = seq(0.01, 0.99, 0.01),
#'   dist_params = mle$coefficients,
#'   dist_varcov = mle$varcov,
#'   distribution = "weibull",
#'   direction = "x"
#' )
#'
#' @md
#'
#' @export
delta_method <- function(x,
                         dist_params,
                         dist_varcov,
                         distribution = c(
                           "weibull", "lognormal", "loglogistic",
                           "sev", "normal", "logistic",
                           "weibull3", "lognormal3", "loglogistic3",
                           "exponential", "exponential2"
                         ),
                         direction = c("y", "x"),
                         p = deprecated()
) {

  if (lifecycle::is_present(p)) {
    deprecate_warn("2.1.0", "delta_method(p)", "delta_method(x)")
    x <- p
  }

  # Checks:
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)
  check_dist_params(dist_params, distribution)

  dm_vectorized <- Vectorize(delta_method_, "x")

  dm_vectorized(
    x = x,
    dist_params = dist_params,
    dist_varcov = dist_varcov,
    distribution = distribution,
    direction = direction
  )
}



delta_method_ <- function(x,
                          dist_params,
                          dist_varcov,
                          distribution = c(
                            "weibull", "lognormal", "loglogistic",
                            "sev", "normal", "logistic",
                            "weibull3", "lognormal3", "loglogistic3",
                            "exponential", "exponential2"
                          ),
                          direction = c("y", "x")
) {

  n_par <- length(dist_params)

  # Only consider parameters of standard distribution:
  if (has_thres(distribution)) {
    n_par <- n_par - 1L
  }

  # Standard Errors for quantiles:
  if (direction == "x") {

    ## Step 1: Determine first derivatives of the quantile function t_p:
    ### Outer derivative is often the quantile function:
    q <- predict_quantile(
      p = x,
      dist_params = dist_params[1:n_par],
      distribution = std_parametric(distribution)
    )

    ### Inner derivative is often the standardized quantile function:
    z <- standardize(
      x = q,
      dist_params = dist_params[1:n_par],
      distribution = std_parametric(distribution)
    )

    ### Derivatives of scale or location-scale distributions:
    dt_dmu <- if (std_parametric(distribution) != "exponential") 1 else NULL
    dt_dsc <- z
    dpar <- c(dt_dmu, dt_dsc)

    ### Derivatives of (log-)location-scale distributions:
    if (std_parametric(distribution) %in% c("weibull", "lognormal", "loglogistic")) {
      dpar <- dpar * q
    }

    ### Derivative w.r.t threshold:
    if (has_thres(distribution)) {
      dpar <- c(dpar, 1)
    }

    # Standard Errors for z using the "z-Procedure":
  } else {
    # Standardized Random Variable:
    z <- standardize(x, dist_params = dist_params, distribution = distribution)

    ## Step 1: Determine first derivatives of the standardized quantile z:
    ### Derivative w.r.t scale or location-scale:
    dz_dmu <- if (distribution != "exponential") (-1 / dist_params[n_par]) else NULL
    dz_dsc <- (-1 / dist_params[n_par]) * z
    dpar <- c(dz_dmu, dz_dsc)

    ### Derivative w.r.t threshold:
    if (has_thres(distribution)) {
      if (distribution == "exponential2") {
        dpar <- rev(dpar)
      } else {
        dz_dgam <- (1 / dist_params[n_par]) * (1 / (dist_params[n_par + 1] - x))
        dpar <- c(dpar, dz_dgam)
      }
    }
  }

  ## Step 2: Determine variance and standard errors:
  dvar <- t(dpar) %*% dist_varcov %*% dpar
  std_err <- sqrt(dvar)

  std_err
}
