#' Rank Regression for Parametric Lifetime Distributions
#'
#' @description
#' This function fits a regression model to a linearized parametric lifetime
#' distribution for complete and (multiple) right-censored data. The parameters
#' are determined in the frequently used (log-)location-scale parameterization.
#'
#' For the Weibull, estimates are additionally transformed such that they are in
#' line with the parameterization provided by the *stats* package
#' (see [Weibull][stats::Weibull]).
#'
#' @details
#' The confidence intervals of the parameters are computed on the basis of a
#' heteroscedasticity-consistent (**HC**) covariance matrix. Here it should be
#' said that there is no statistical foundation to determine the standard errors
#' of the parameters using *Least Squares* in context of *Rank Regression*.
#' For an accepted statistical method use [maximum likelihood][ml_estimation].
#'
#' If `options = list(conf_method = "Mock")`, the argument `distribution` must be
#' one of `"weibull"` and `"weibull3"`. The approximated confidence intervals
#' for the Weibull parameters can then only be estimated on the following
#' confidence levels (see 'References' *(Mock, 1995)*):
#'
#' * `conf_level = 0.90`
#' * `conf_level = 0.95`
#' * `conf_level = 0.99`
#'
#' @param x A `tibble` with class `wt_cdf_estimation` returned by [estimate_cdf].
#' @param distribution Supposed distribution of the random variable.
#' @param conf_level Confidence level of the interval.
#' @param direction Direction of the dependence in the regression model.
#' @param control A list of control parameters (see [optim][stats::optim]).
#'
#' `control` is in use only if a three-parametric distribution was specified.
#' If this is the case, `optim` (always with `method = "L-BFGS-B"` and
#' `control$fnscale = -1`) is called to determine the threshold parameter
#' (see [r_squared_profiling]).
#'
#' @param options A list of named options. See 'Options'.
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
#' @section Options:
#' Argument `options` is a named list of options:
#'
#' | Name             | Value                                     |
#' | :--------------- | :---------------------------------------  |
#' | `conf_method`    | `"HC"` (default) or `"Mock"`              |
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
rank_regression.wt_cdf_estimation <- function(
                                     x,
                                     distribution = c(
                                       "weibull", "lognormal", "loglogistic",
                                       "sev", "normal", "logistic",
                                       "weibull3", "lognormal3", "loglogistic3",
                                       "exponential", "exponential2"
                                     ),
                                     conf_level = 0.95,
                                     direction = c("x_on_y", "y_on_x"),
                                     control = list(),
                                     options = list(),
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
      control = control,
      options = options
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
        control = control,
        options = options
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
#' @inheritSection rank_regression Options
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
                                    distribution = c(
                                      "weibull", "lognormal", "loglogistic",
                                      "sev", "normal", "logistic",
                                      "weibull3", "lognormal3", "loglogistic3",
                                      "exponential", "exponential2"
                                    ),
                                    conf_level = 0.95,
                                    direction = c("x_on_y", "y_on_x"),
                                    control = list(),
                                    options = list(),
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
    control = control,
    options = options
  )
}



# Function that performs the parameter estimation:
rank_regression_ <- function(cdf_estimation,
                             distribution,
                             conf_level,
                             direction,
                             control,
                             options
) {

  # In terms of RR only failed items can be used:
  cdf_failed <- cdf_estimation %>% dplyr::filter(.data$status == 1)

  x_f <- cdf_failed$x
  y_f <- cdf_failed$prob

  # Pre-Step: Threshold models must be profiled w.r.t threshold:
  if (has_thres(distribution)) {

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
  if (std_parametric(distribution) == "exponential") {
    names(dist_params) <- "sigma"
  } else {
    names(dist_params) <- c("mu", "sigma")
  }

  ### Threshold parameter:
  if (exists("opt_thres")) {
    dist_params <- c(dist_params, opt_thres)
    names(dist_params)[length(dist_params)] <- "gamma"
  }

  # Step 2: Confidence intervals computation based on 'options':
  ## Value of argument 'options':
  conf_option <- options$conf_method %||% "HC"

  ## Confidence intervals with HC standard errors:
  if (conf_option == "HC") {
    ### Estimating HC covariance matrix:
    dist_varcov <- sandwich::vcovHC(x = rr, type = "HC1")

    output <- conf_HC(
      dist_params = dist_params,
      dist_varcov = dist_varcov,
      distribution = distribution,
      conf_level = conf_level,
      n = length(x_f),
      direction = direction
    )
  } else {
    if (distribution %in% c("weibull", "weibull3")) {
      ## Confidence intervals according to Mock for 'weibull' and 'weibull3':
      output <- conf_mock(
        dist_params = dist_params,
        conf_level = conf_level,
        n = length(x_f),
        direction = direction
      )
    } else {
      stop(
        "For option conf_method = 'Mock', the distribution argument needs to be",
        " 'weibull' or 'weibull3' but " , sQuote(distribution), " was used!",
        call. = FALSE
      )
    }
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
