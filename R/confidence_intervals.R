#' Beta Binomial Confidence Bounds for Quantiles and Probabilities
#'
#' @description
#' This function computes the non-parametric beta binomial confidence bounds (BB)
#' for quantiles and failure probabilities.
#'
#' @details The procedure is similar to the *Median Ranks* method but with the
#' difference that instead of finding the probability for the *j*-th rank at the
#' 50% level the probability (probabilities) has (have) to be found at the given
#' confidence level.
#'
#' @param x A list with class `wt_model` (and further classes) returned by
#' [rank_regression].
#' @param b_lives A numeric vector indicating the probabilities \eqn{p} of the
#' \eqn{B_p}-lives (quantiles) to be considered.
#' @param bounds A character string specifying the bound(s) to be computed.
#' @param conf_level Confidence level of the interval.
#' @param direction A character string specifying the direction of the confidence
#' interval. `"y"` for failure probabilities or `"x"` for quantiles.
#' @template dots
#'
#' @template return-bb-confidence-intervals
#' @templateVar
#' cdf_estimation_method  Method for the estimation of failure probabilities which was specified in [estimate_cdf].
#' @return
#' Further information is stored in the attributes of this tibble:
#'
#' * `distribution` : Distribution which was specified in [rank_regression].
#' * `bounds` : Specified bound(s).
#' * `direction` : Specified direction.
#' * `model_estimation` : Input list with class `wt_model`.
#'
#' @examples
#' # Reliability data preparation:
#' ## Data for two-parametric model:
#' data_2p <- reliability_data(
#'   shock,
#'   x = distance,
#'   status = status
#' )
#'
#' ## Data for three-parametric model:
#' data_3p <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl_2p <- estimate_cdf(
#'   data_2p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_3p <- estimate_cdf(
#'   data_3p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_mult <- estimate_cdf(
#'   data_3p,
#'   methods = c("johnson", "mr")
#' )
#'
#' # Model estimation with rank_regression():
#' rr_2p <- rank_regression(
#'   prob_tbl_2p,
#'   distribution = "weibull"
#' )
#'
#' rr_3p <- rank_regression(
#'   prob_tbl_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.90
#' )
#'
#' rr_lists <- rank_regression(
#'   prob_tbl_mult,
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
#' @md
#'
#' @export
confint_betabinom <- function(x, ...) {
  UseMethod("confint_betabinom")
}



#' @rdname confint_betabinom
#'
#' @export
confint_betabinom.wt_model <- function(x,
                                       b_lives = c(0.01, 0.1, 0.50),
                                       bounds = c(
                                         "two_sided", "lower", "upper"
                                       ),
                                       conf_level = 0.95,
                                       direction = c("y", "x"),
                                       ...
) {
  stopifnot(
    inherits(x, "wt_model_estimation") ||
      inherits(x, "wt_model_estimation_list")
  )

  NextMethod()
}



#' @export
confint_betabinom.wt_model_estimation <- function(x,
                                                  b_lives = c(0.01, 0.1, 0.50),
                                                  bounds = c(
                                                    "two_sided", "lower",
                                                    "upper"
                                                  ),
                                                  conf_level = 0.95,
                                                  direction = c("y", "x"),
                                                  ...
) {
  bounds <- match.arg(bounds)
  direction <- match.arg(direction)

  confint_betabinom_(
    model_estimation = x,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



#' @export
confint_betabinom.wt_model_estimation_list <- function(
                                      x,
                                      b_lives = c(0.01, 0.1, 0.50),
                                      bounds = c("two_sided", "lower", "upper"),
                                      conf_level = 0.95,
                                      direction = c("y", "x"),
                                      ...
) {
  bounds <- match.arg(bounds)
  direction <- match.arg(direction)

  confint <- purrr::map_dfr(x, function(model_estimation) {
    confint <- confint_betabinom_(
      model_estimation = model_estimation,
      b_lives = b_lives,
      bounds = bounds,
      conf_level = conf_level,
      direction = direction
    )
  })

  attr(confint, "model_estimation") <- x
  confint
}



#' Beta Binomial Confidence Bounds for Quantiles and Probabilities
#'
#' @inherit confint_betabinom description details
#'
#' @inheritParams rank_regression.default
#' @inheritParams confint_betabinom
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param dist_params The parameters (`coefficients`) returned by [rank_regression].
#' @param distribution Supposed distribution of the random variable. Has to be in
#'   line with the specification made in [rank_regression].
#'
#' @template return-bb-confidence-intervals
#' @templateVar
#' cdf_estimation_method  A character that is always `NA_character`. Only needed for internal use.
#' @return
#' Further information is stored in the attributes of this tibble:
#'
#' * `distribution` : Distribution which was specified in [rank_regression].
#' * `bounds` : Specified bound(s).
#' * `direction` : Specified direction.
#'
#' @seealso [confint_betabinom]
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
#' prob_tbl <- estimate_cdf(
#'   x = obs,
#'   status = status_1,
#'   method = "johnson"
#' )
#'
#' prob_tbl_2 <- estimate_cdf(
#'   x = cycles,
#'   status = status_2,
#'   method = "johnson"
#' )
#'
#' # Model estimation with rank_regression():
#' rr <- rank_regression(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   distribution = "weibull",
#'   conf_level = 0.9
#' )
#'
#' rr_2 <- rank_regression(
#'   x = prob_tbl_2$x,
#'   y = prob_tbl_2$prob,
#'   status = prob_tbl_2$status,
#'   distribution = "lognormal3"
#' )
#'
#' # Example 1 - Two-sided 95% confidence interval for probabilities ('y'):
#' conf_betabin_1 <- confint_betabinom(
#'   x = prob_tbl$x,
#'   status = prob_tbl$status,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull",
#'   bounds = "two_sided",
#'   conf_level = 0.95,
#'   direction = "y"
#' )
#'
#' # Example 2 - One-sided lower/upper 90% confidence interval for quantiles ('x'):
#' conf_betabin_2_1 <- confint_betabinom(
#'   x = prob_tbl$x,
#'   status = prob_tbl$status,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull",
#'   bounds = "lower",
#'   conf_level = 0.9,
#'   direction = "x"
#' )
#'
#' conf_betabin_2_2 <- confint_betabinom(
#'   x = prob_tbl$x,
#'   status = prob_tbl$status,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull",
#'   bounds = "upper",
#'   conf_level = 0.9,
#'   direction = "x"
#' )
#'
#' # Example 3 - Two-sided 90% confidence intervals for both directions using
#' # a three-parametric model:
#'
#' conf_betabin_3_1 <- confint_betabinom(
#'   x = prob_tbl_2$x,
#'   status = prob_tbl_2$status,
#'   dist_params = rr_2$coefficients,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.9,
#'   direction = "y"
#' )
#'
#' conf_betabin_3_2 <- confint_betabinom(
#'   x = prob_tbl_2$x,
#'   status = prob_tbl_2$status,
#'   dist_params = rr_2$coefficients,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.9,
#'   direction = "x"
#' )
#'
#' @md
#'
#' @export
confint_betabinom.default <- function(x,
                                      status,
                                      dist_params,
                                      distribution = c(
                                        "weibull", "lognormal", "loglogistic",
                                        "sev", "normal", "logistic",
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

  # Construct fake wt_model_estimation:
  model_estimation <- list(
    data = tibble::tibble(
      x = x, status = status, cdf_estimation_method = NA_character_
    ),
    coefficients = dist_params,
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

  # Prepare function inputs:
  x <- model_estimation$data$x
  status <- model_estimation$data$status
  distribution <- model_estimation$distribution

  ## Information of survived units are indirectly included in dist_params and n:
  dist_params <- model_estimation$coefficients
  n <- length(x)

  ## Only range of failed units is considered:
  x_ob <- x[status == 1]

  # Step 1: Determine x-y ranges and add the appropriate B_p-lives:
  x_y_b_lives <- add_b_lives(x_ob, dist_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  # Step 2: Confidence intervals w.r.t 'direction' and 'bounds':
  ## Obtain virtual ranks by interpolation using Benard's formula:
  virt_rank <- y_seq * (n + 0.4) + 0.3

  ## Confidence intervals for direction = "y" w.r.t 'bounds':
  list_confint <- conf_bb_y(
    y = y_seq,
    n = n,
    r = virt_rank,
    bounds = bounds,
    conf_level = conf_level
  )

  ## Confidence intervals for direction = "x" if needed:
  if (direction == "x") {
    list_confint <- purrr::map(
      list_confint,
      predict_quantile,
      dist_params = dist_params,
      distribution = distribution
    )
  }

  # Step 3: Form output:
  list_output <- c(
    list(x = x_seq, rank = virt_rank, prob = y_seq),
    list_confint
  )

  tbl_out <- tibble::as_tibble(list_output)

  ## Add cdf_estimation_method as column to support different cdf_estimation_methods:
  cdf_estimation_method <- model_estimation$data$cdf_estimation_method[1]
  tbl_out$cdf_estimation_method <- cdf_estimation_method # recycling!

  tbl_out <- structure(
    tbl_out,
    distribution = distribution,
    bounds = bounds,
    direction = direction
  )

  ## Only add model_estimation if not faked by .default:
  if (inherits(model_estimation, "wt_model_estimation")) {
    attr(tbl_out, "model_estimation") <- model_estimation
  }

  ## Make output usable for generics:
  class(tbl_out) <- c("wt_confint", class(tbl_out))

  tbl_out
}



#' Fisher's Confidence Bounds for Quantiles and Probabilities
#'
#' @description
#' This function computes normal-approximation confidence intervals for quantiles
#' and failure probabilities.
#'
#' @details
#' The basis for the calculation of these confidence bounds are the standard errors
#' obtained by the [delta method][delta_method].
#'
#' The bounds on the probability are determined by the *z-procedure*. See
#' 'References' for more information on this approach.
#'
#' @inheritParams confint_betabinom
#' @param x A list with classes `wt_model` and `wt_ml_estimation` returned by
#' [ml_estimation].
#'
#' @template return-fisher-confidence-intervals
#' @return
#' Further information is stored in the attributes of this tibble:
#'
#' * `distribution` : Distribution which was specified in [ml_estimation].
#' * `bounds` : Specified bound(s).
#' * `direction` : Specified direction.
#' * `model_estimation` : Input list with classes `wt_model` and `wt_ml_estimation`.
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
#'   shock,
#'   x = distance,
#'   status = status
#' )
#'
#' ## Data for three-parametric model:
#' data_3p <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Model estimation with ml_estimation():
#' ml_2p <- ml_estimation(
#'   data_2p,
#'   distribution = "weibull"
#' )
#'
#' ml_3p <- ml_estimation(
#'   data_3p,
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
#' @md
#'
#' @export
confint_fisher <- function(x, ...) {
  UseMethod("confint_fisher")
}



#' @rdname confint_fisher
#'
#' @export
confint_fisher.wt_model <- function(x,
                                    b_lives = c(0.01, 0.1, 0.50),
                                    bounds = c(
                                      "two_sided", "lower", "upper"
                                    ),
                                    conf_level = 0.95,
                                    direction = c("y", "x"),
                                    ...
) {
  stopifnot(inherits(x, "wt_ml_estimation"))
  NextMethod()
}



#' @export
confint_fisher.wt_ml_estimation <- function(x,
                                            b_lives = c(0.01, 0.1, 0.50),
                                            bounds = c(
                                              "two_sided", "lower", "upper"
                                            ),
                                            conf_level = 0.95,
                                            direction = c("y", "x"),
                                            ...
) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)

  confint_fisher_(
    model_estimation = x,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



#' Fisher's Confidence Bounds for Quantiles and Probabilities
#'
#' @inherit confint_fisher description details references
#'
#' @inheritParams delta_method
#' @inheritParams confint_betabinom.default
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#' could be every characteristic influencing the reliability of a product, e.g.
#' operating time (days/months in service), mileage (km, miles), load cycles.
#' @param direction A character string specifying the direction of the confidence
#' interval. `"y"` for failure probabilities or `"x"` for quantiles.
#'
#' @template return-fisher-confidence-intervals
#' @return
#' Further information is stored in the attributes of this tibble:
#'
#' * `distribution` : Distribution which was specified in [ml_estimation].
#' * `bounds` : Specified bound(s).
#' * `direction` : Specified direction.
#'
#' @seealso [confint_fisher]
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
#'   dist_params = ml$coefficients,
#'   dist_varcov = ml$varcov,
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
#'   dist_params = ml$coefficients,
#'   dist_varcov = ml$varcov,
#'   distribution = "weibull",
#'   bounds = "lower",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' conf_fisher_2_2 <- confint_fisher(
#'   x = obs,
#'   status = status_1,
#'   dist_params = ml$coefficients,
#'   dist_varcov = ml$varcov,
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
#'   dist_params = ml_2$coefficients,
#'   dist_varcov = ml_2$varcov,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "y"
#' )
#'
#' conf_fisher_3_2 <- confint_fisher(
#'   x = cycles,
#'   status = status_2,
#'   dist_params = ml_2$coefficients,
#'   dist_varcov = ml_2$varcov,
#'   distribution = "lognormal3",
#'   bounds = "two_sided",
#'   conf_level = 0.90,
#'   direction = "x"
#' )
#'
#' @md
#'
#' @export
confint_fisher.default <- function(x,
                                   status,
                                   dist_params,
                                   dist_varcov,
                                   distribution = c(
                                     "weibull", "lognormal", "loglogistic",
                                     "sev", "normal", "logistic",
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
    data = tibble::tibble(
      x = x, status = status, cdf_estimation_method = NA_character_
    ),
    coefficients = dist_params,
    varcov = dist_varcov,
    distribution = distribution
  )

  confint_fisher_(
    model_estimation = model_estimation,
    b_lives = b_lives,
    bounds = bounds,
    conf_level = conf_level,
    direction = direction
  )
}



confint_fisher_ <- function(model_estimation,
                            b_lives,
                            bounds,
                            conf_level,
                            direction
) {

  # Prepare function inputs:
  x <- model_estimation$data$x
  status <- model_estimation$data$status
  dist_params <- model_estimation$coefficients
  dist_varcov <- model_estimation$varcov
  distribution <- model_estimation$distribution

  n <- length(x)

  ## Only range of failed units is considered:
  x_ob <- x[status == 1]

  # Step 1: Determine x-y ranges and add the appropriate B_p-lives:
  x_y_b_lives <- add_b_lives(x_ob, dist_params, distribution, b_lives)

  x_seq <- x_y_b_lives$x_seq
  y_seq <- x_y_b_lives$y_seq

  # Step 2: Normal approx. confidence intervals w.r.t 'direction' and 'bounds':

  ## Quantiles of normal distribution for chosen bounds (two-sided or one-sided):
  q_n <- switch(
    bounds,
    "two_sided" = stats::qnorm((1 + conf_level) / 2),
    stats::qnorm(conf_level)
  )

  ## Confidence intervals for direction = "x":
  if (direction == "x") {
    se_delta <- delta_method(
      x = y_seq,
      dist_params = dist_params,
      dist_varcov = dist_varcov,
      distribution = distribution,
      direction = direction
    )

    ## Ensure that confidence intervals are strictly positive:
    w <- exp((q_n * se_delta) / x_seq)

    w <- switch(
      bounds,
      "two_sided" = list(lower_bound = 1 / w, upper_bound = w),
      "lower" = list(lower_bound = 1 / w),
      "upper" = list(upper_bound = w)
    )

    list_confint <- purrr::map(w, `*`, x_seq)
    names(list_confint) <- names(w)

  } else {
    ## Confidence intervals for direction = "y":
    se_delta <- delta_method(
      x = x_seq,
      dist_params = dist_params,
      dist_varcov = dist_varcov,
      distribution = distribution,
      direction = direction
    )

    w <- q_n * se_delta

    ## Standardized random variable:
    z <- standardize(
      x = x_seq, dist_params = dist_params, distribution = distribution
    )

    zw <- switch(
      bounds,
      "two_sided" = list(lower_bound = z - w, upper_bound = z + w),
      "lower" = list(lower_bound = z - w),
      "upper" = list(upper_bound = z + w)
    )

    ## Model-specific standard distribution:
    pfun <- switch(
      std_parametric(distribution),
      "sev" = , "weibull" = psev,
      "normal" = , "lognormal" = stats::pnorm,
      "logistic" = , "loglogistic" = stats::plogis
    )

    list_confint <- purrr::map(zw, pfun)
    names(list_confint) <- names(zw)
  }

  # Step 3: Form output:
  list_output <- c(
    list(x = x_seq, prob = y_seq, std_err = se_delta),
    list_confint
  )

  tbl_out <- tibble::as_tibble(list_output)

  ## Add cdf_estimation_method (same colnames as confint_betabinom's output):
  tbl_out$cdf_estimation_method <- NA_character_

  tbl_out <- structure(
    tbl_out,
    distribution = distribution,
    bounds = bounds,
    direction = direction
  )

  ## Only add model_estimation if not faked by .default:
  if (inherits(model_estimation, "wt_model_estimation")) {
    attr(tbl_out, "model_estimation") <- model_estimation
  }

  # Make output usable for generics
  class(tbl_out) <- c("wt_confint", class(tbl_out))

  tbl_out
}
