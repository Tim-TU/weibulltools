#' Rank Regression for Parametric Lifetime Distributions
#'
#' @description
#' This function fits a regression model to a linearized two- or three-parameter
#' lifetime distribution for complete and (multiple) right-censored data.
#' The parameters are determined in the frequently used (log-)location-scale
#' parameterization.
#'
#' For the Weibull, estimates are additionally transformed such that they are in
#' line with the parameterization provided by the *stats* package
#' (see [Weibull][stats::Weibull]).
#'
#' @details
#' If `distribution` is `"weibull"` or `"weibull3"`, the approximated
#' confidence intervals for the parameters can only be estimated on the following
#' confidence levels (see 'References' *(Mock, 1995)*):
#'
#' * `conf_level = 0.90`
#' * `conf_level = 0.95`
#' * `conf_level = 0.99`
#'
#' If the distribution is not the Weibull, the confidence intervals of the
#' parameters are computed on the basis of a heteroscedasticity-consistent
#' covariance matrix. Here it should be said that there is no statistical foundation
#' to determine the standard errors of the parameters using *Least Squares*
#' in context of *Rank Regression*. For an accepted statistical method use
#' [maximum likelihood][ml_estimation].
#'
#' @param x A `tibble` of class `wt_cdf_estimation` returned by [estimate_cdf].
#' @param distribution Supposed distribution of the random variable.
#' @param conf_level Confidence level of the interval. If `distribution` is
#'   `"weibull"` this must be one of `0.9`, `0.95` or `0.99`.
#' @param direction Direction of the dependence in the regression model.
#' @param control A list of control parameters (see [optim][stats::optim]).
#'
#' `control` is in use only if a three-parametric distribution was specified.
#' If this is the case, `optim` (always with `method = "L-BFGS-B"` and
#' `control$fnscale = -1`) is called to determine the threshold parameter
#' (see [r_squared_profiling]).
#' @template dots
#'
#' @template return-rank-regression
#' @templateVar data A `tibble` with class `wt_cdf_estimation` returned by [estimate_cdf].
#' @return
#'   If more than one method was specified in [estimate_cdf], the resulting output
#'   is a list with class `wt_model_estimation_list`. In this case, each list element
#'   has classes `wt_rank_regression` and `wt_model_estimation`, and the items listed
#'   above, are included.
#'
#' @encoding UTF-8
#'
#' @references
#'
#' * Mock, R., Methoden zur Datenhandhabung in Zuverlässigkeitsanalysen,
#'   vdf Hochschulverlag AG an der ETH Zürich, 1995
#' * Meeker, William Q; Escobar, Luis A., Statistical methods for reliability data,
#'   New York: Wiley series in probability and statistics, 1998
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
#'   methods = c("johnson", "kaplan")
#' )
#'
#' # Example 1 - Fitting a two-parametric weibull distribution:
#' rr_2p <- rank_regression(
#'   x = prob_tbl_2p,
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Fitting a three-parametric lognormal distribution:
#' rr_3p <- rank_regression(
#'   x = prob_tbl_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.99
#' )
#'
#' # Example 3 - Fitting a three-parametric lognormal distribution using
#' # direction and control arguments:
#' rr_3p_control <- rank_regression(
#'   x = prob_tbl_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.99,
#'   direction = "y_on_x",
#'   control = list(trace = TRUE, REPORT = 1)
#' )
#'
#' # Example 4 - Fitting a three-parametric loglogistic distribution if multiple
#' # methods in estimate_cdf were specified:
#' rr_lists <- rank_regression(
#'   x = prob_tbl_mult,
#'   distribution = "loglogistic3",
#'   conf_level = 0.90
#' )
#'
#' @md
#'
#' @export
rank_regression <- function(x, ...) {
  UseMethod("rank_regression")
}



#' @rdname rank_regression
#'
#' @export
rank_regression.wt_cdf_estimation <- function(x,
                                              distribution = c(
                                                "weibull", "lognormal",
                                                "loglogistic", "normal",
                                                "logistic", "sev", "weibull3",
                                                "lognormal3", "loglogistic3"
                                              ),
                                              conf_level = 0.95,
                                              direction = c("x_on_y", "y_on_x"),
                                              control = list(),
                                              ...
) {

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  if (length(unique(x$cdf_estimation_method)) == 1) {
    rank_regression_(
      cdf_estimation = x,
      distribution = distribution,
      conf_level = conf_level,
      direction = direction,
      control = control
    )
  } else {
    # Apply rank_regression to each cdf estimation method separately
    x_split <- split(x, x$cdf_estimation_method)

    model_estimation_list <- purrr::map(x_split, function(cdf_estimation) {
      rank_regression_(
        cdf_estimation = cdf_estimation,
        distribution = distribution,
        conf_level = conf_level,
        direction = direction,
        control = control
      )
    })

    class(model_estimation_list) <- c(
      "wt_model", "wt_model_estimation_list",
      class(model_estimation_list)
    )

    model_estimation_list
  }
}



#' Rank Regression for Parametric Lifetime Distributions
#'
#' @inherit rank_regression description details references
#'
#' @inheritParams rank_regression
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in `x`.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is
#'   a right censored observation (= 0) or a failure (= 1).
#'
#' @template return-rank-regression
#' @templateVar data A `tibble` with columns `x`, `status` and `prob`.
#'
#' @seealso [rank_regression]
#'
#' @examples
#' # Vectors:
#' obs <- seq(10000, 100000, 10000)
#' status_1 <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' cycles <- alloy$cycles
#' status_2 <- alloy$status
#'
#' # Example 1 - Fitting a two-parametric weibull distribution:
#' tbl_john <- estimate_cdf(
#'   x = obs,
#'   status = status_1,
#'   method = "johnson"
#' )
#'
#' rr <- rank_regression(
#'   x = tbl_john$x,
#'   y = tbl_john$prob,
#'   status = tbl_john$status,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' # Example 2 - Fitting a three-parametric lognormal distribution:
#' tbl_kaplan <- estimate_cdf(
#'   x = cycles,
#'   status = status_2,
#'   method = "kaplan"
#' )
#'
#' rr_2 <- rank_regression(
#'   x = tbl_kaplan$x,
#'   y = tbl_kaplan$prob,
#'   status = tbl_kaplan$status,
#'   distribution = "lognormal3"
#' )
#'
#' @md
#'
#' @export
rank_regression.default <- function(x,
                                    y,
                                    status,
                                    distribution = c("weibull", "lognormal",
                                                     "loglogistic", "normal",
                                                     "logistic", "sev",
                                                     "weibull3", "lognormal3",
                                                     "loglogistic3"),
                                    conf_level = 0.95,
                                    direction = c("x_on_y", "y_on_x"),
                                    control = list(),
                                    ...
) {

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  cdf_estimation <- tibble::tibble(x = x, status = status, prob = y)

  rank_regression_(
    cdf_estimation = cdf_estimation,
    distribution = distribution,
    conf_level = conf_level,
    direction = direction,
    control = control
  )
}



# Function that performs the parameter estimation:
rank_regression_ <- function(cdf_estimation,
                             distribution,
                             conf_level,
                             direction,
                             control
) {

  # In terms of RR only failed items can be used:
  cdf_failed <- cdf_estimation %>% dplyr::filter(.data$status == 1)

  x_f <- cdf_failed$x
  y_f <- cdf_failed$prob

  # Pre-Step: Three-parametric models must be profiled w.r.t to threshold:
  if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {

    ## Force maximization:
    control$fnscale <- -1

    ## Optimization using `r_squared_profiling()`:
    opt_thres <- stats::optim(
      par = 0,
      fn = r_squared_profiling,
      method = "L-BFGS-B",
      upper = (1 - (1 / 1e+5)) * min(x_f),
      lower = 0,
      control = control,
      x = x_f,
      y = y_f,
      distribution = distribution
    )

    opt_thres <- opt_thres$par

    ## Preparation for lm:
    x_thres <- x_f - opt_thres
    subs <- x_thres > 0
    x_f <- x_thres[subs]
    y_f <- y_f[subs]
  }

  # Step 1: Model estimation using RR:
  rr <- lm_(
    x = x_f,
    y = y_f,
    distribution = distribution,
    direction = direction
  )

  ## Parameters:
  dist_params <- stats::coef(rr)
  names(dist_params) <- c("mu", "sigma")

  ### Three-parametric:
  if (exists("opt_thres")) {
    dist_params <- c(dist_params, opt_thres)
    names(dist_params)[3] <- "gamma"
  }

  # Step 2: Confidence intervals and return preparation:
  ## Confidence intervals according to Mock for 'weibull' and 'weibull3':
  if (distribution %in% c("weibull", "weibull3")) {
    output <- conf_mock(
      dist_params = dist_params,
      conf_level = conf_level,
      n = length(x_f),
      direction = direction
    )
  } else {
    ## Confidence intervals with HC standard errors for any other distribution:
    ### Estimating HC covariance matrix:
    dist_varcov <- sandwich::vcovHC(x = rr, type = "HC1")

    output <- conf_HC(
      dist_params = dist_params,
      dist_varcov = dist_varcov,
      conf_level = conf_level,
      n = length(x_f),
      direction = direction
    )
  }

  # Step 3: Form output:
  r_sq <- summary(rr)$r.squared

  rr_output <- c(
    output,
    r_squared = r_sq
  )

  rr_output$data = cdf_estimation
  rr_output$distribution = distribution
  rr_output$direction = direction

  class(rr_output) <- c(
    "wt_model", "wt_rank_regression", "wt_model_estimation",
    class(rr_output)
  )

  rr_output
}



#' @export
print.wt_rank_regression <- function(x,
                                     digits = max(3L, getOption("digits") - 3L),
                                     ...
) {
  cat("Rank Regression\n")
  NextMethod("print")
}



#' R-Squared-Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @description
#' This function evaluates the coefficient of determination with respect to a
#' given threshold parameter of a three-parametric lifetime distribution.
#' In terms of *Rank Regression* this function can be optimized
#' ([optim][stats::optim]) to estimate the threshold parameter.
#'
#' @inheritParams rank_regression
#' @param thres A numeric value for the threshold parameter.
#' @param distribution Supposed three-parametric distribution of the random variable.
#'
#' @return
#' Returns the coefficient of determination with respect to the threshold parameter
#' `thres`.
#'
#' @encoding UTF-8
#'
#' @examples
#' # Data:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   data,
#'   methods = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(
#'     dplyr::pull(
#'       dplyr::filter(
#'         prob_tbl,
#'         status == 1,
#'         x == min(x)
#'       ),
#'       x
#'     ) - 0.1
#'   ),
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = dplyr::filter(
#'     prob_tbl,
#'     status == 1
#'   ),
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
r_squared_profiling <- function(x, ...) {
  UseMethod("r_squared_profiling")
}



#' @rdname r_squared_profiling
#'
#' @export
r_squared_profiling.wt_cdf_estimation <- function(x,
                                                  thres,
                                                  distribution = c(
                                                    "weibull3", "lognormal3",
                                                    "loglogistic3"
                                                  ),
                                                  direction = c("x_on_y", "y_on_x"),
                                                  ...
) {

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  r_squared_profiling.default(
    x = x$x,
    y = x$prob,
    thres = thres,
    distribution = distribution,
    direction = direction
  )
}



#' R-Squared-Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @inherit r_squared_profiling description details return
#'
#' @inheritParams r_squared_profiling
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in x.
#'
#' @seealso [r_squared_profiling]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(cycles[status == 1]) - 0.1,
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = prob_tbl$x[prob_tbl$status == 1],
#'   y = prob_tbl$prob[prob_tbl$status == 1],
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the
#' ## coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
r_squared_profiling.default <- function(x,
                                        y,
                                        thres,
                                        distribution = c("weibull3",
                                                         "lognormal3",
                                                         "loglogistic3"),
                                        direction = c("x_on_y", "y_on_x"),
                                        ...
) {

  if (any(is.na(y))) {
    stop(
      "At least one of the failure probabilities ('y') is NA!",
      .call = FALSE
    )
  }

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  r_sq_prof_vectorized <- Vectorize(
    FUN = r_squared_profiling_,
    vectorize.args = "thres"
  )

  r_sq_prof_vectorized(
    x = x,
    y = y,
    thres = thres,
    distribution = distribution,
    direction = direction
  )
}



r_squared_profiling_ <- function(x,
                                 y,
                                 thres,
                                 distribution,
                                 direction

) {

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Rank Regression:
  rr <- lm_(
    x = x_thres,
    y = y,
    distribution = distribution,
    direction = direction
  )

  summary(rr)$r.squared
}
