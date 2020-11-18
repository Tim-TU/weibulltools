#' Beta Binomial Confidence Bounds for Quantiles and/or Probabilities
#'
#' This non-parametric approach calculates confidence bounds for quantiles and/or
#' failure probabilities using a procedure that is similar to that used in
#' calculating median ranks. The location-scale (and threshold) parameters estimated
#' by rank regression are needed.
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=confint_betabinom.model_estimation]{model_estimation}}}{
#'     Preferred. Provide the output of \code{\link{rank_regression}}.
#'    }
#'   \item{\code{\link[=confint_betabinom.default]{default}}}{
#'     Provide \code{x}, \code{event}, \code{loc_sc_params} and \code{distribution}
#'     manually.
#'   }
#' }
#'
#' @return A tibble containing the lifetime characteristic, interpolated
#' ranks as a function of probabilities, the probabilities which are used to
#' compute the ranks and computed values for the specified confidence bound(s).
#'
#'
confint_betabinom <- function(x, ...) {
  UseMethod("confint_betabinom")
}

#' Beta Binomial Confidence Bounds for Quantiles and/or Probabilities
#'
#' @inherit confint_betabinom description return
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x Object of class \code{model_estimation} returned from
#'   \code{\link{rank_regression}}.
#' @param bounds A character string specifying the interval(s) which has/have to
#'   be computed. Must be one of "two_sided" (default), "lower" or "upper".
#' @param conf_level Confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param direction A character string specifying the direction of the computed
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#'
#' @export
#'
#' @examples
#' # Example 1: Beta-Binomial Confidence Bounds for two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' tbl <- reliability_data(x = obs, status = status)
#'
#' tbl_john <- estimate_cdf(tbl, methods = "johnson")
#'
#' mrr <- rank_regression(
#'   tbl_john,
#'   distribution = "weibull",
#'   conf_level = .95
#' )
#'
#' conf_betabin <- confint_betabinom(
#'   mrr,
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2: Beta-Binomial Confidence Bounds for three-parameter Weibull:
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#'
#' tbl_2 <- reliability_data(x = cycles, status = status)
#'
#' tbl_john_2 <- estimate_cdf(tbl_2, methods = "johnson")
#'
#' mrr_weib3 <- rank_regression(
#'   tbl_john_2,
#'   distribution = "weibull3",
#'   conf_level = .95
#' )
#'
#' conf_betabin_weib3 <- confint_betabinom(
#'   mrr_weib3,
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' @export
confint_betabinom.model_estimation <- function(
                                      x,
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = 0.95,
                                      direction = c("y", "x")
) {
  data <- x$data
  distribution <- x$distribution

  confint_betabinom.default(
    x = data$x,
    event = data$event,
    loc_sc_params = x$loc_sc_params,
    distribution = distribution,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}

#' Beta Binomial Confidence Bounds for Quantiles and/or Probabilities
#'
#' @inherit confint_betabinom description return
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x A numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the range of confidence region(s).
#' @param event A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @inheritParams predict_quantile
#' @inheritParams confint_betabinom.model_estimation
#'
#' @examples
#' # Example 1: Beta-Binomial Confidence Bounds for two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' tbl <- reliability_data(x = obs, status = status)
#'
#' tbl_john <- estimate_cdf(tbl, methods = "johnson")
#'
#' mrr <- rank_regression(
#'   x = tbl_john$characteristic,
#'   y = tbl_john$prob,
#'   event = tbl_john$status,
#'   distribution = "weibull",
#'   conf_level = .95
#' )
#'
#' conf_betabin <- confint_betabinom(
#'   x = tbl_john$characteristic,
#'   event = tbl_john$status,
#'   loc_sc_params = mrr$loc_sc_params,
#'   distribution = "weibull"
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2: Beta-Binomial Confidence Bounds for three-parameter Weibull:
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' tbl_2 <- reliability_data(shock, x = cycles, status = status)
#'
#' tbl_john_2 <- estimate_cdf(tbl_2, methods = "johnson")
#'
#' mrr_weib3 <- rank_regression(
#'   x = tbl_john_2$characteristic,
#'   y = tbl_john_2$prob,
#'   event = tbl_john_2$status,
#'   distribution = "weibull3",
#'   conf_level = .95
#' )
#'
#' conf_betabin_weib3 <- confint_betabinom(
#'   x = tbl_john_2$characteristic,
#'   event = tbl_john_2$status,
#'   loc_sc_params = mrr_weib3$loc_sc_params,
#'   distribution = "weibull3",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' @export
confint_betabinom.default <- function(x,
                                      event,
                                      loc_sc_params,
                                      distribution = c(
                                        "weibull", "lognormal", "loglogistic",
                                        "normal", "logistic", "sev", "weibull3",
                                        "lognormal3", "loglogistic3"
                                      ),
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = .95,
                                      direction = c("y", "x")
) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  n <- length(x)
  x_ob <- x[event == 1]

  x_y_b_lives <- add_b_lives(x_ob, loc_sc_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  # Caluclating virtual ranks, i.e. interpolating between realisations using
  # Benard's approximation:
  virt_rank <- y_seq * (n + 0.4) + 0.3


  # Two-sided or one-sided bounds:
  if (bounds == "two_sided") {
    conf_up <- stats::qbeta((1 + conf_level) / 2, virt_rank, n - virt_rank + 1)
    conf_low <- stats::qbeta((1 - conf_level) / 2, virt_rank, n - virt_rank + 1)
    list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)
  } else if (bounds == "lower") {
    conf_low <- stats::qbeta(1 - conf_level, virt_rank, n - virt_rank + 1)
    list_confint <- list(lower_bound = conf_low)
  } else {
    conf_up <- stats::qbeta(conf_level, virt_rank, n - virt_rank + 1)
    list_confint <- list(upper_bound = conf_up)
  }

  # Bounds for probability (y) or quantiles (x):
  if (direction == "y") {
    list_output <- c(
      list(characteristic = x_seq, rank = virt_rank, prob = y_seq),
      list_confint
    )

    tbl_out <- tibble::as_tibble(list_output)
  } else {
    x_confint <- purrr::map(
      list_confint,
      predict_quantile,
      loc_sc_params = loc_sc_params,
      distribution = distribution
    )

    list_output <- c(
      list(characteristic = x_seq, rank = virt_rank, prob = y_seq),
      x_confint
    )

    tbl_out <- tibble::as_tibble(list_output)
  }

  class(tbl_out) <- c("confint", class(tbl_out))

  attr(tbl_out, "distribution") <- distribution
  attr(tbl_out, "bounds") <- bounds
  attr(tbl_out, "direction") <- direction

  return(tbl_out)
}

#' Delta Method for Parametric Lifetime Distributions
#'
#' The Delta Method estimates the standard error for quantities that can be
#' written as non-linear functions of ML estimators like quantiles. I.e. the
#' (log-)location-scale (and threshold) parameters and variance-covariance matrix
#' of these need to be estimated by Maximum Likelihood.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param p a numeric value of a probability or a quantile. If the standard error
#'   of quantile is of interest a specific probability needs to be supplied and if
#'   the standard error of a standardized quantile (z-value) should be calculated
#'   a specific quantile should be provided.
#' @param loc_sc_params a (named) numeric vector of estimated
#'   (by Maximum Likelihood) location and scale parameters for a specified
#'   distribution. The order of elements is important. First entry needs to be
#'   the location parameter \eqn{\mu} and the second element needs to be the
#'   scale parameter \eqn{\sigma}. If a three-parametric model is used the third element
#'   is the threshold parameter \eqn{\gamma}.
#' @param loc_sc_varcov a (named) numeric matrix of estimated
#'   (by Maximum Likelihood) location and scale variances and covariances for a
#'   specified distribution. The order of elements is important. First entry
#'   of the diagonal needs to be the variance of the location parameter
#'   Var(\eqn{\mu}) and the second element of the diagonal needs to be the
#'   variance of the scale parameter Var(\eqn{\sigma}). If a three-parametric model
#'   is used the third element of the diagonal needs to be the variance of the
#'   threshold parameter Var(\eqn{\gamma}).
#' @inheritParams predict_quantile
#' @param direction a character string specifying the direction of the computed
#'   standard errors. Must be either "y" (used for confidence intervals of failure
#'   probabilities in \code{\link{confint_fisher}}) or "x" (used for confidence
#'   intervals of quantiles in \code{\link{confint_fisher}}). If \code{p} is a
#'   quantile then \emph{direction} needs to be "y" and vice versa.
#'
#' @return A numeric value with estimated standard errors of quantiles or standardized
#'   z values. Both are required for the computation of normal approximation
#'   confidence intervals. If standard errors of standardized z values are compueted
#'   one can calculate confidence intervals for distribution probabilities (z-procedure,
#'   which is used inside \code{\link{confint_fisher}}).
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' data <- reliability_data(x = obs, status = status)
#'
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' delta_prob <- sapply(obs, delta_method,
#'   loc_sc_params = mle$loc_sc_params,
#'   loc_sc_varcov = mle$loc_sc_varcov,
#'   distribution = "weibull",
#'   direction = "y"
#' )
delta_method <- function(
  p,
  loc_sc_params,
  loc_sc_varcov,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"
  ),
  direction = c("y", "x")
) {

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  dm_vectorized <- Vectorize(delta_method_, "p")

  dm_vectorized(
    p = p,
    loc_sc_params = loc_sc_params,
    loc_sc_varcov = loc_sc_varcov,
    distribution = distribution,
    direction = direction
  )
}

delta_method_ <- function(p, loc_sc_params, loc_sc_varcov,
                         distribution = c("weibull", "lognormal", "loglogistic",
                                          "normal", "logistic", "sev", "weibull3",
                                          "lognormal3", "loglogistic3"),
                         direction = c("y", "x")) {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }

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
      q <- predict_quantile(p = p, loc_sc_params = loc_sc_params,
        distribution = distribution)
      dq_dmu <- q
      dq_dsc <- z * q
      dq_dpar <- c(dq_dmu, dq_dsc)
    }
    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      # Quantile for specified two-parameter distribution (gamma is irrelevant; see derivatives):
      q <- predict_quantile(p = p, loc_sc_params = loc_sc_params[-3],
        distribution = substr(distribution, start = 1, stop = nchar(distribution) - 1))
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
    var_q <- t(dq_dpar) %*% loc_sc_varcov %*% dq_dpar
    std_err <- sqrt(var_q)

    # Standard Errors for z: The "z-Procedure":
  } else {
    # Standardized Random Variable:
    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
      z <- (log(p) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }
    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      z <- (log(p - loc_sc_params[[3]]) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }
    if (distribution %in% c("sev", "normal", "logistic")) {
      z <- (p - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }

    # First derivatives of z regarding parameters:
    if (distribution %in% c("weibull", "lognormal", "loglogistic", "sev",
                            "normal", "logistic")) {
      dz_dmu <- (-1 / loc_sc_params[[2]])
      dz_dsc <- (-1 / loc_sc_params[[2]]) * z
      dz_dpar <- c(dz_dmu, dz_dsc)
    }

    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      dz_dmu <- (-1 / loc_sc_params[[2]])
      dz_dsc <- (-1 / loc_sc_params[[2]]) * z
      dz_dgam <- (1 / loc_sc_params[[2]]) * (1 / (loc_sc_params[[3]] - p))
      dz_dpar <- c(dz_dmu, dz_dsc, dz_dgam)
    }

    var_z <- t(dz_dpar) %*% loc_sc_varcov %*% dz_dpar
    std_err <- sqrt(var_z)
  }

  return(std_err)
}


#' Fisher Confidence Bounds for Quantiles and/or Probabilities
#'
#' This method computes normal-approximation confidence intervals for quantiles
#' and/or failure probabilities using the \code{\link{delta_method}}. The
#' required (log-)location-scale (and threshold) parameters and variance-covariance matrix
#' of these need to be estimated by Maximum Likelihood.
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=confint_fisher.model_estimation]{model_estimation}}}{
#'     Preferred. Provide the output of \code{\link{ml_estimation}}.
#'   }
#'   \item{\code{\link[=confint_fisher.default]{default}}}{
#'     Provide \code{x}, \code{event}, \code{loc_sc_params}, \code{loc_sc_varcov}
#'     and \code{distribution} manually.
#'   }
#' }
#'
#' @return A tibble containing the lifetime characteristic, the
#'   probabilities, estimated standard errors by the delta method and computed
#'   values for the specified confidence bound(s).
#'
#' @export
confint_fisher <- function(x, ...) {
  UseMethod("confint_fisher")
}

#' Fisher Confidence Bounds for Quantiles and/or Probabilities
#'
#' @inherit confint_fisher description return
#'
#' @param x Object of class \code{model_estimation} returned from
#'   \code{\link{ml_estimation}}.
#' @inheritParams confint_betabinom.model_estimation
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' data <- reliability_data(x = obs, status = status)
#'
#' tbl_john <- estimate_cdf(data, "johnson")
#'
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' conf_fish <- confint_fisher(
#'   mle,
#'   distribution = "weibull",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#' @export
confint_fisher.model_estimation <- function(
                                      x,
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = 0.95,
                                      direction = c("y", "x")
) {
  data <- x$data
  distribution <- x$distribution

  confint_fisher.default(
    x = data$x,
    event = data$status,
    loc_sc_params = data$loc_sc_params,
    loc_sc_varcov = data$loc_sc_varcov,
    distribution = distribution,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}

#' Fisher Confidence Bounds for Quantiles and/or Probabilities
#'
#' @inherit confint_fisher description return
#'
#' @param x a numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the range of confidence region(s).
#' @inheritParams delta_method
#' @inheritParams confint_betabinom.default
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' data <- reliability_data(x = obs, status = status)
#'
#' tbl_john <- estimate_cdf(data, "johnson")
#'
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' conf_fish <- confint_fisher(
#'   x = tbl_john$characteristic,
#'   event = tbl_john$status,
#'   loc_sc_params = mle$loc_sc_params,
#'   loc_sc_varcov = mle$loc_sc_varcov,
#'   distribution = "weibull",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#' @export
confint_fisher.default <- function(
  x,
  event,
  loc_sc_params,
  loc_sc_varcov,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"
  ),
  b_lives = c(0.01, 0.1, 0.50),
  bounds = c("two_sided", "lower", "upper"),
  conf_level = .95,
  direction = c("y", "x")
) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  n <- length(x)
  x_ob <- x[event == 1]

  x_y_b_lives <- add_b_lives(x_ob, loc_sc_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  if (direction == "x") {
    se_delta <- sapply(y_seq, delta_method, loc_sc_params = loc_sc_params,
                       loc_sc_varcov = loc_sc_varcov,
                       distribution = distribution, direction = direction)

    if (bounds == "two_sided") {
      w <- exp((stats::qnorm((1 + conf_level) / 2) * se_delta) / x_seq)
      conf_up <- x_seq * w
      conf_low <- x_seq / w
      list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)
    } else if (bounds == "lower") {
      w <- exp((stats::qnorm(conf_level) * se_delta) / x_seq)
      conf_low <- x_seq / w
      list_confint <- list(lower_bound = conf_low)
    } else {
      w <- exp((stats::qnorm(conf_level) * se_delta) / x_seq)
      conf_up <- x_seq * w
      list_confint <- list(upper_bound = conf_up)
    }

    list_output <- c(list(characteristic = x_seq, prob = y_seq, std_err = se_delta),
                     list_confint)

    tbl_out <- tibble::as_tibble(list_output)
  } else {
    # Standard errors for z:
    se_delta <- sapply(x_seq, delta_method, loc_sc_params = loc_sc_params,
      loc_sc_varcov = loc_sc_varcov,
      distribution = distribution, direction = direction)

    # Standardized Random Variable:
    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
      z <- (log(x_seq) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }
    if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
      z <- (log(x_seq - loc_sc_params[[3]]) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }
    if (distribution %in% c("sev", "normal", "logistic")) {
      z <- (x_seq - loc_sc_params[[1]]) / loc_sc_params[[2]]
    }

    # Calculating confidence intervals:
    if (bounds == "two_sided") {
      # Confidence Interval for z:
      w <- stats::qnorm((1 + conf_level) / 2) * se_delta
      if (distribution %in% c("weibull", "weibull3", "sev")) {
        conf_up <- SPREDA::psev(z + w)
        conf_low <- SPREDA::psev(z - w)
      }
      if (distribution %in% c("lognormal", "lognormal3", "normal")) {
        conf_up <- stats::pnorm(z + w)
        conf_low <- stats::pnorm(z - w)
      }
      if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
        conf_up <- stats::plogis(z + w)
        conf_low <- stats::plogis(z - w)
      }
      list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)

    } else if (bounds == "lower") {
      w <- stats::qnorm(conf_level) * se_delta
      if (distribution %in% c("weibull", "weibull3", "sev")) {
        conf_low <- SPREDA::psev(z - w)
      }
      if (distribution %in% c("lognormal", "lognormal3", "normal")) {
        conf_low <- stats::pnorm(z - w)
      }
      if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
        conf_low <- stats::plogis(z - w)
      }
      list_confint <- list(lower_bound = conf_low)

    } else {
      w <- stats::qnorm(conf_level) * se_delta
      if (distribution %in% c("weibull", "weibull3", "sev")) {
        conf_up <- SPREDA::psev(z + w)
      }
      if (distribution %in% c("lognormal", "lognormal3", "normal")) {
        conf_up <- stats::pnorm(z + w)
      }
      if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
        conf_up <- stats::plogis(z + w)
      }
      list_confint <- list(upper_bound = conf_up)

    }

    list_output <- c(list(characteristic = x_seq, prob = y_seq, std_err = se_delta),
      list_confint)

    tbl_out <- tibble::as_tibble(list_output)
  }

  # Make output usable for generics
  class(tbl_out) <- c("confint", class(tbl_out))

  attr(tbl_out, "distribution") <- distribution
  attr(tbl_out, "bounds") <- bounds
  attr(tbl_out, "direction") <- direction

  return(tbl_out)
}



add_b_lives <- function(x, loc_sc_params, distribution, b_lives) {
  # Range of failed items:
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_seq <- seq(x_min, x_max, length.out = 100)

  # Range of probabilities calculated with estimated regression line:
  y_seq <- predict_prob(q = x_seq, loc_sc_params = loc_sc_params,
                        distribution = distribution)

  # Looking for B lives in range of estimated ones:
  b_lives_present <- b_lives[b_lives >= y_seq[1] & b_lives <= y_seq[length(y_seq)]]

  # Add them:
  y_seq <- sort(unique(c(y_seq, b_lives_present)))
  x_seq <- predict_quantile(
    p = y_seq,
    loc_sc_params = loc_sc_params,
    distribution = distribution
  )

  list(
    x_seq = x_seq,
    y_seq = y_seq
  )
}
