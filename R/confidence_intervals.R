#' Beta Binomial Confidence Bounds for Quantiles and Probabilities
#'
#' @description
#' This non-parametric approach computes the beta binomial bounds (BB) for
#' quantiles and failure probabilities using a procedure similar to the calculation
#' of probabilities in terms of (\emph{Median Ranks}).
#'
#' @details The difference to \emph{Median Ranks}, i.e. finding the probability of
#' rank \emph{j} at a 50\% level, is to determine the probability of rank \emph{j}
#' on another level, the specified confidence level.
#'
#' @param x Object of class \code{model_estimation} (or \code{model_estimation_list})
#'   returned from \code{\link{rank_regression}}.
#' @param b_lives A numeric vector indicating the probabilities \emph{p} of the
#'   \emph{B_p} lives (quantiles) to be considered.
#' @param bounds A character string specifying of which bounds have to be computed.
#'   One of \code{"two_sided"}, \code{"lower"} or \code{"upper"}.
#' @param conf_level Confidence level of the interval.
#' @param direction A character string specifying the direction of the
#'   confidence interval. One of \code{"y"} (failure probabilities) or \code{"x"}
#'   (quantiles).
#'
#' @return A tibble with class \code{"confint"} containing the following columns:
#'   \itemize{
#'     \item \code{x} : An ordered sequence of the lifetime characteristic regarding
#'       the failed units, starting at \code{min(x)} and ending up at \code{max(x)}.
#'       With \code{b_lives = c(0.01, 0.1, 0.5)} the 1\%, 10\% and 50\% quantiles
#'       are additionally included in \code{x}, but only if the specified
#'       probabilities are in the range of the estimated probabilities.
#'     \item \code{rank} : Interpolated ranks as a function of probabilities,
#'       computed with the converted approximation formula of Benard.
#'     \item \code{prob} : An ordered sequence of probabilities with specified
#'       \code{b_lives} included.
#'     \item \code{lower_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"lower"}. Lower limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{upper_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"upper"}. Upper limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{distribution} : Specified distribution (determined when calling
#'       \code{\link{rank_regression}}).
#'     \item \code{bounds} : Specified bound(s).
#'     \item \code{direction} : Specified direction.
#'     \item \code{method} : Specified method for the estimation of failure
#'       probabilities (determined when calling \code{\link{estimate_cdf}}).
#'   }
#'
#' @examples
#' # Reliability data preparation:
#' ## Data for two-parametric model:
#' data_2p <- reliability_data(
#'   data = shock,
#'   x = distance,
#'   status = status
#' )
#'
#' ## Data for three-parametric model:
#' data_3p <- reliability_data(
#'   data = alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl_2p <- estimate_cdf(
#'   x = data_2p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_3p <- estimate_cdf(
#'   x = data_3p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_mult <- estimate_cdf(
#'   x = data_3p,
#'   methods = c("johnson", "mr")
#' )
#'
#' # Model estimation with rank_regression():
#' rr_2p <- rank_regression(
#'   x = prob_tbl_2p,
#'   distribution = "weibull"
#' )
#'
#' rr_3p <- rank_regression(
#'   x = prob_tbl_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.90
#' )
#'
#' rr_lists <- rank_regression(
#'   x = prob_tbl_mult,
#'   distribution = "loglogistic3",
#'   conf_level = 0.90
#' )
#'
#' # Example 1 - Two-sided 95% confidence interval for probabilities ('y'):
#' conf_betabin_1 <- confint_betabinom(
#'   x = rr_2p,
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2 - One-sided lower/upper 90% confidence interval for quantiles ('x'):
#' conf_betabin_2_1 <- confint_betabinom(
#'   x = rr_2p,
#'   bounds = "lower",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' conf_betabin_2_2 <- confint_betabinom(
#'   x = rr_2p,
#'   bounds = "upper",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' # Example 3 - Two-sided 90% confidence intervals for both directions using
#' # a three-parametric model:
#'
#' conf_betabin_3_1 <- confint_betabinom(
#'   x = rr_3p,
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "y"
#' )
#'
#' conf_betabin_3_2 <- confint_betabinom(
#'   x = rr_3p,
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' # Example 4 - Confidence intervals if multiple methods in estimate_cdf, i.e.
#' # "johnson" and "mr", were specified:
#'
#' conf_betabin_4 <- confint_betabinom(
#'   x = rr_lists,
#'   bounds = "two_sided",
#'   conf_level = 0.99,
#'   direction = "y"
#' )
#'
#' @export
confint_betabinom <- function(x, ...) {
  UseMethod("confint_betabinom")
}



#' @rdname confint_betabinom
#'
#' @export
confint_betabinom.model_estimation <- function(
                                 x,
                                 b_lives = c(0.01, 0.1, 0.50),
                                 bounds = c("two_sided", "lower", "upper"),
                                 conf_level = 0.95,
                                 direction = c("y", "x"),
                                 ...
) {
  confint_betabinom_(
    model_estimation = x,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



#' @rdname confint_betabinom
#'
#' @export
confint_betabinom.model_estimation_list <- function(
                                      x,
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = 0.95,
                                      direction = c("y", "x"),
                                      ...
) {
  bounds <- match.arg(bounds)
  direction <- match.arg(direction)

  purrr::map_dfr(x, function(model_estimation) {
    confint <- confint_betabinom_(
      model_estimation = model_estimation,
      b_lives = b_lives,
      bounds = bounds,
      conf_level = conf_level,
      direction = direction
    )
  })
}



#' Beta Binomial Confidence Bounds for Quantiles and Probabilities
#'
#' @inherit confint_betabinom description details
#'
#' @inheritParams ml_estimation.default
#' @inheritParams confint_betabinom.model_estimation
#'
#' @param loc_sc_params A (named) numeric vector of (log-)location-scale parameters
#'   returned from \code{\link{rank_regression}}.
#' @param distribution Supposed distribution of the random variable. Has to be in
#'   line with the specification made in \code{\link{rank_regression}}.
#'
#' @return A tibble with class \code{"confint"} containing the following columns:
#'   \itemize{
#'     \item \code{x} : An ordered sequence of the lifetime characteristic regarding
#'       the failed units, starting at \code{min(x)} and ending up at \code{max(x)}.
#'       With \code{b_lives = c(0.01, 0.1, 0.5)} the 1\%, 10\% and 50\% quantiles
#'       are additionally included in \code{x}, but only if the specified
#'       probabilities are in the range of the estimated probabilities.
#'     \item \code{rank} : Interpolated ranks as a function of probabilities,
#'       computed with the converted approximation formula of Benard.
#'     \item \code{prob} : An ordered sequence of probabilities with specified
#'       \code{b_lives} included.
#'     \item \code{lower_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"lower"}. Lower limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{upper_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"upper"}. Upper limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{distribution} : Specified distribution.
#'     \item \code{bounds} : Specified bound(s).
#'     \item \code{direction} : Specified direction.
#'     \item \code{method} : A character that is always \code{"conf_null"}. Due to
#'       generic visualization functions column \code{method} has to be provided.
#'   }
#'
#' @seealso \code{\link{confint_betabinom}}
#'
#' @examples
#' # Vectors:
#' obs <- seq(10000, 100000, 10000)
#' status_1 <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' cycles <- alloy$cycles
#' status_2 <- alloy$status
#'
#' # Probability estimation:
#' tbl_john <- estimate_cdf(
#'   x = obs,
#'   status = status_1,
#'   methods = "johnson"
#' )
#'
#' tbl_john_2 <- estimate_cdf(
#'   x = cycles,
#'   status = status_2,
#'   methods = "johnson"
#' )
#'
#' # Model estimation with rank_regression():
#' rr <- rank_regression(
#'   x = tbl_john$x,
#'   y = tbl_john$prob,
#'   status = tbl_john$status,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' rr_2 <- rank_regression(
#'   x = tbl_john_2$x,
#'   y = tbl_john_2$prob,
#'   status = tbl_john_2$status,
#'   distribution = "lognormal3"
#' )
#'
#' # Example 1 - Two-sided 95% confidence interval for probabilities ('y'):
#' conf_betabin_1 <- confint_betabinom(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = rr$loc_sc_params,
#'   distribution = "weibull",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2 - One-sided lower/upper 90% confidence interval for quantiles ('x'):
#' conf_betabin_2_1 <- confint_betabinom(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = rr$loc_sc_params,
#'   distribution = "weibull",
#'   bounds = "lower",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' conf_betabin_2_2 <- confint_betabinom(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = rr$loc_sc_params,
#'   distribution = "weibull",
#'   bounds = "upper",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' # Example 3 - Two-sided 90% confidence intervals for both directions using
#' # a three-parametric model:
#'
#' conf_betabin_3_1 <- confint_betabinom(
#'   x = cycles,
#'   status = status_2,
#'   loc_sc_params = rr_2$loc_sc_params,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "y"
#' )
#'
#' conf_betabin_3_2 <- confint_betabinom(
#'   x = cycles,
#'   status = status_2,
#'   loc_sc_params = rr_2$loc_sc_params,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' @export
confint_betabinom.default <- function(x,
                                      status,
                                      loc_sc_params,
                                      distribution = c(
                                        "weibull", "lognormal", "loglogistic",
                                        "normal", "logistic", "sev",
                                        "weibull3", "lognormal3", "loglogistic3"
                                      ),
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = 0.95,
                                      direction = c("y", "x"),
                                      ...
) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  # Fake model_estimation
  model_estimation <- list(
    data = tibble::tibble(x = x, status = status, method = "conf_null"),
    loc_sc_params = loc_sc_params,
    distribution = distribution
  )

  confint_betabinom_(
    model_estimation = model_estimation,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



confint_betabinom_ <- function(model_estimation,
                               b_lives,
                               bounds,
                               conf_level,
                               direction
) {

  method <- model_estimation$data$method[1]

  if (method %in% c("kaplan", "nelson")) {
    stop(
      "The beta binomial confidence intervals cannot be calculated for method '",
      method, "'. Use method 'mr' or 'johnson'."
    )
  }

  x <- model_estimation$data$x
  status <- model_estimation$data$status

  distribution <- model_estimation$distribution
  loc_sc_params <- model_estimation$loc_sc_params

  n <- length(x)
  x_ob <- x[status == 1]

  x_y_b_lives <- add_b_lives(x_ob, loc_sc_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  # Calculating virtual ranks, i.e. interpolating between realizations using
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
      list(x = x_seq, rank = virt_rank, prob = y_seq),
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
      list(x = x_seq, rank = virt_rank, prob = y_seq),
      x_confint
    )

    tbl_out <- tibble::as_tibble(list_output)
  }

  tbl_out <- tbl_out %>%
    dplyr::mutate(
      distribution = distribution,
      bounds = bounds,
      direction = direction,
      method = method
    )

  class(tbl_out) <- c("confint", class(tbl_out))

  return(tbl_out)
}



#' Delta Method for Parametric Lifetime Distributions
#'
#' @description
#' This function applies the delta method for two- or three-parametric lifetime
#' distributions that belong to the (log-)location-scale family.
#'
#' @details
#' The delta method estimates the standard errors for quantities that can be
#' written as non-linear functions of ML estimators. Hence, (log-)location-scale
#' parameters as well as the variance-covariance matrix of these have to be estimated
#' with \link[=ml_estimation]{maximum likelihood}.
#'
#' The estimated standard errors are needed to calculate Fisher's (normal
#' approximation) confidence intervals. For confidence bounds on the probability,
#' standard errors of the standardized quantiles (\code{direction = "y"})
#' have to be computed (\emph{z-procedure}) and for bounds on quantiles, standard errors
#' of quantiles (\code{direction = "x"}) are required. For more information see
#' \code{\link{confint_fisher}}.
#'
#' @param p A numeric vector of probabilities or quantiles. If the standard errors
#'   of quantiles should be determined the corresponding probabilities have to be
#'   specified, and if the standard errors of standardized quantiles (z-values)
#'   should be computed corresponding quantiles are required.
#' @param loc_sc_params A (named) numeric vector of (log-)location-scale parameters
#'   returned from \code{\link{ml_estimation}}.
#' @param distribution Supposed distribution of the random variable. Has to be in
#'   line with the specification made in \code{\link{ml_estimation}}.
#' @param loc_sc_varcov A (named) numeric matrix of estimated variances and
#'   covariances returned from \code{\link{ml_estimation}}.
#' @param direction A character string specifying for which quantity the standard
#'   errors are calculated. One of \code{"y"} (if \code{p} are quantiles) or
#'   \code{"x"} (if \code{p} are probabilities).
#'
#' @return A numeric vector of estimated standard errors for quantiles or
#'   standardized quantiles (\emph{z-values}).
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @examples
#' # Reliability data preparation:
#' data <- reliability_data(
#'   data = shock,
#'   x = distance,
#'   status = status
#' )
#'
#' # Parameter estimation using maximum likelihood:
#' mle <- ml_estimation(
#'   x = data,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' # Example 1 - Standard errors of standardized quantiles:
#' delta_y <- delta_method(
#'   p = shock$distance,
#'   loc_sc_params = mle$loc_sc_params,
#'   loc_sc_varcov = mle$loc_sc_varcov,
#'   distribution = "weibull",
#'   direction = "y"
#' )
#'
#' # Example 2 - Standard errors of quantiles:
#' delta_x <- delta_method(
#'   p = seq(0.01, 0.99, 0.01),
#'   loc_sc_params = mle$loc_sc_params,
#'   loc_sc_varcov = mle$loc_sc_varcov,
#'   distribution = "weibull",
#'   direction = "x"
#' )
#'
#' @export
delta_method <- function(p,
                         loc_sc_params,
                         loc_sc_varcov,
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
    loc_sc_params = loc_sc_params,
    loc_sc_varcov = loc_sc_varcov,
    distribution = distribution,
    direction = direction
  )
}



delta_method_ <- function(p,
                          loc_sc_params,
                          loc_sc_varcov,
                          distribution = c(
                            "weibull", "lognormal", "loglogistic",
                            "normal", "logistic", "sev",
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
        loc_sc_params = loc_sc_params,
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
        loc_sc_params = loc_sc_params[-3],
        distribution = substr(
          distribution,
          start = 1,
          stop = nchar(distribution) - 1
        )
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



#' Fisher's Confidence Bounds for Quantiles and Probabilities
#'
#' @description
#' This function computes normal-approximation confidence intervals for quantiles
#' and failure probabilities.
#'
#' @details
#' The basis for the calculation of these confidence bounds are the standard errors
#' determined by the \link[=delta_method]{delta method} and hence the required
#' (log-)location-scale parameters as well as the variance-covariance matrix of
#' these have to be estimated with \link[=ml_estimation]{maximum likelihood}.
#'
#' The bounds on the probability are determined by the \emph{z-procedure}. See
#' 'References' for more information on this approach.
#'
#' @inheritParams confint_betabinom
#'
#' @param x Object of class \code{model_estimation} returned from
#'   \code{\link{ml_estimation}}.
#'
#' @return A tibble with class \code{"confint"} containing the following columns:
#'   \itemize{
#'     \item \code{x} : An ordered sequence of the lifetime characteristic regarding
#'       the failed units, starting at \code{min(x)} and ending up at \code{max(x)}.
#'       With \code{b_lives = c(0.01, 0.1, 0.5)} the 1\%, 10\% and 50\% quantiles
#'       are additionally included in \code{x}, but only if the specified
#'       probabilities are in the range of the estimated probabilities.
#'     \item \code{prob} : An ordered sequence of probabilities with specified
#'       \code{b_lives} included.
#'     \item \code{std_err} : Estimated standard errors with respect to
#'       \code{direction}.
#'     \item \code{lower_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"lower"}. Lower limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{upper_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"upper"}. Upper limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{distribution} : Specified distribution (determined when calling
#'     \code{\link{ml_estimation}}).
#'     \item \code{bounds} : Specified bound(s).
#'     \item \code{direction} : Specified direction.
#'     \item \code{method} : A character that is always \code{"conf_null"}. Due to
#'       generic visualization functions column \code{method} has to be provided.
#'   }
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @examples
#' # Reliability data preparation:
#' ## Data for two-parametric model:
#' data_2p <- reliability_data(
#'   data = shock,
#'   x = distance,
#'   status = status
#' )
#'
#' ## Data for three-parametric model:
#' data_3p <- reliability_data(
#'   data = alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Model estimation with ml_estimation():
#' ml_2p <- ml_estimation(
#'   x = data_2p,
#'   distribution = "weibull"
#' )
#'
#' ml_3p <- ml_estimation(
#'   x = data_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.90
#' )
#'
#'
#' # Example 1 - Two-sided 95% confidence interval for probabilities ('y'):
#' conf_fisher_1 <- confint_fisher(
#'   x = ml_2p,
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2 - One-sided lower/upper 90% confidence interval for quantiles ('x'):
#' conf_fisher_2_1 <- confint_fisher(
#'   x = ml_2p,
#'   bounds = "lower",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' conf_fisher_2_2 <- confint_fisher(
#'   x = ml_2p,
#'   bounds = "upper",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' # Example 3 - Two-sided 90% confidence intervals for both directions using
#' # a three-parametric model:
#'
#' conf_fisher_3_1 <- confint_fisher(
#'   x = ml_3p,
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "y"
#' )
#'
#' conf_fisher_3_2 <- confint_fisher(
#'   x = ml_3p,
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' @export
confint_fisher <- function(x, ...) {
  UseMethod("confint_fisher")
}



#' @rdname confint_fisher
#'
#' @export
confint_fisher.model_estimation <- function(
                              x,
                              b_lives = c(0.01, 0.1, 0.50),
                              bounds = c(
                                "two_sided", "lower", "upper"
                              ),
                              conf_level = 0.95,
                              direction = c("y", "x"),
                              ...
) {

  data <- x$data
  distribution <- x$distribution

  confint_fisher.default(
    x = data$x,
    status = data$status,
    loc_sc_params = x$loc_sc_params,
    loc_sc_varcov = x$loc_sc_varcov,
    distribution = distribution,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



#' Fisher Confidence Bounds for Quantiles and Probabilities
#'
#' @inherit confint_fisher description details references
#'
#' @inheritParams delta_method
#' @inheritParams confint_betabinom.default
#'
#' @param direction A character string specifying the direction of the
#'   confidence interval. One of \code{"y"} (failure probabilities) or \code{"x"}
#'   (quantiles).
#'
#' @return A tibble with class \code{"confint"} containing the following columns:
#'   \itemize{
#'     \item \code{x} : An ordered sequence of the lifetime characteristic regarding
#'       the failed units, starting at \code{min(x)} and ending up at \code{max(x)}.
#'       With \code{b_lives = c(0.01, 0.1, 0.5)} the 1\%, 10\% and 50\% quantiles
#'       are additionally included in \code{x}, but only if the specified
#'       probabilities are in the range of the estimated probabilities.
#'     \item \code{prob} : An ordered sequence of probabilities with specified
#'       \code{b_lives} included.
#'     \item \code{std_err} : Estimated standard errors with respect to
#'       \code{direction}.
#'     \item \code{lower_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"lower"}. Lower limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{upper_bound} : Provided, if \code{bounds} is one of
#'       \code{"two_sided"} or \code{"upper"}. Upper limit of the confidence region
#'       with respect to \code{direction}, i.e. quantiles or probabilities.
#'     \item \code{distribution} : Specified distribution (determined when calling
#'     \code{\link{ml_estimation}}).
#'     \item \code{bounds} : Specified bound(s).
#'     \item \code{direction} : Specified direction.
#'     \item \code{method} : A character that is always \code{"conf_null"}. Due to
#'       generic visualization functions column \code{method} has to be provided.
#'   }
#'
#' @seealso \code{\link{confint_fisher}}
#'
#' @examples
#' # Vectors:
#' obs <- seq(10000, 100000, 10000)
#' status_1 <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' cycles <- alloy$cycles
#' status_2 <- alloy$status
#'
#'
#' # Model estimation with ml_estimation():
#' ml <- ml_estimation(
#'   x = obs,
#'   status = status_1,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' ml_2 <- ml_estimation(
#'   x = cycles,
#'   status = status_2,
#'   distribution = "lognormal3"
#' )
#'
#' # Example 1 - Two-sided 95% confidence interval for probabilities ('y'):
#' conf_fisher_1 <- confint_fisher(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = ml$loc_sc_params,
#'   loc_sc_varcov = ml$loc_sc_varcov,
#'   distribution = "weibull",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2 - One-sided lower/upper 90% confidence interval for quantiles ('x'):
#' conf_fisher_2_1 <- confint_fisher(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = ml$loc_sc_params,
#'   loc_sc_varcov = ml$loc_sc_varcov,
#'   distribution = "weibull",
#'   bounds = "lower",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' conf_fisher_2_2 <- confint_fisher(
#'   x = obs,
#'   status = status_1,
#'   loc_sc_params = ml$loc_sc_params,
#'   loc_sc_varcov = ml$loc_sc_varcov,
#'   distribution = "weibull",
#'   bounds = "upper",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' # Example 3 - Two-sided 90% confidence intervals for both directions using
#' # a three-parametric model:
#'
#' conf_fisher_3_1 <- confint_fisher(
#'   x = cycles,
#'   status = status_2,
#'   loc_sc_params = ml_2$loc_sc_params,
#'   loc_sc_varcov = ml_2$loc_sc_varcov,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "y"
#' )
#'
#' conf_fisher_3_2 <- confint_fisher(
#'   x = cycles,
#'   status = status_2,
#'   loc_sc_params = ml_2$loc_sc_params,
#'   loc_sc_varcov = ml_2$loc_sc_varcov,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' @export
confint_fisher.default <- function(x,
                                   status,
                                   loc_sc_params,
                                   loc_sc_varcov,
                                   distribution = c(
                                     "weibull", "lognormal", "loglogistic",
                                     "normal", "logistic", "sev",
                                     "weibull3", "lognormal3", "loglogistic3"
                                   ),
                                   b_lives = c(0.01, 0.1, 0.50),
                                   bounds = c("two_sided", "lower", "upper"),
                                   conf_level = 0.95,
                                   direction = c("y", "x"),
                                   s...
) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  n <- length(x)
  x_ob <- x[status == 1]

  x_y_b_lives <- add_b_lives(x_ob, loc_sc_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  if (direction == "x") {
    se_delta <- delta_method(
      p = y_seq,
      loc_sc_params = loc_sc_params,
      loc_sc_varcov = loc_sc_varcov,
      distribution = distribution,
      direction = direction
    )

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

    list_output <- c(list(x = x_seq, prob = y_seq, std_err = se_delta),
                     list_confint)

    tbl_out <- tibble::as_tibble(list_output)
  } else {
    # Standard errors for z:
    se_delta <- delta_method(
      p = x_seq,
      loc_sc_params = loc_sc_params,
      loc_sc_varcov = loc_sc_varcov,
      distribution = distribution,
      direction = direction
    )

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

    list_output <- c(list(x = x_seq, prob = y_seq, std_err = se_delta),
                     list_confint)

    tbl_out <- tibble::as_tibble(list_output)
  }

  tbl_out <- tbl_out %>%
    dplyr::mutate(
      distribution = distribution,
      bounds = bounds,
      direction = direction,
      method = "conf_null"
    )

  # Make output usable for generics
  class(tbl_out) <- c("confint", class(tbl_out))

  return(tbl_out)
}



add_b_lives <- function(x,
                        loc_sc_params,
                        distribution,
                        b_lives
) {

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
