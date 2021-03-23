#' ML Estimation for Parametric Lifetime Distributions
#'
#' @description
#' This function estimates the parameters of a parametric lifetime distribution
#' for complete and (multiple) right-censored data. The parameters
#' are determined in the frequently used (log-)location-scale parameterization.
#'
#' For the Weibull, estimates are additionally transformed such that they are in
#' line with the parameterization provided by the *stats* package
#' (see [Weibull][stats::Weibull]).
#'
#' @details
#' Within `ml_estimation`, [optim][stats::optim] is called with `method = "BFGS"`
#' and `control$fnscale = -1` to estimate the parameters that maximize the
#' log-likelihood (see [loglik_function]). For threshold models, the profile
#' log-likelihood is maximized in advance (see [loglik_profiling]). Once the
#' threshold parameter is determined, the threshold model is treated like a
#' distribution without threshold (lifetime is reduced by threshold estimate)
#' and the general optimization routine is applied.
#'
#' Normal approximation confidence intervals for the parameters are computed as well.
#'
#' @param x A `tibble` with class `wt_reliability_data` returned by [reliability_data].
#' @param distribution Supposed distribution of the random variable.
#' @param wts Optional vector of case weights. The length of `wts` must be equal
#' to the number of observations in `x`.
#' @param conf_level Confidence level of the interval.
#' @param start_dist_params Optional vector with initial values of the
#' (log-)location-scale parameters.
#' @param control A list of control parameters (see 'Details' and
#' [optim][stats::optim]).
#' @template dots
#'
#' @template return-ml-estimation
#' @templateVar data A `tibble` with class `wt_reliability_data` returned by
#' [reliability_data].
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
#' # Example 1 - Fitting a two-parametric weibull distribution:
#' ml_2p <- ml_estimation(
#'   data_2p,
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Fitting a three-parametric lognormal distribution:
#' ml_3p <- ml_estimation(
#'   data_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.99
#' )
#'
#' @md
#'
#' @export
ml_estimation <- function(x, ...) {
  UseMethod("ml_estimation")
}



#' @rdname ml_estimation
#'
#' @export
ml_estimation.wt_reliability_data <- function(x,
                                              distribution = c(
                                                "weibull", "lognormal",
                                                "loglogistic", "sev", "normal",
                                                "logistic", "weibull3",
                                                "lognormal3", "loglogistic3",
                                                "exponential", "exponential2"
                                              ),
                                              wts = rep(1, nrow(x)),
                                              conf_level = 0.95,
                                              start_dist_params = NULL,
                                              control = list(),
                                              ...
) {

  distribution <- match.arg(distribution)

  ml_estimation_(
    x,
    distribution = distribution,
    wts = wts,
    conf_level = conf_level,
    start_dist_params = start_dist_params,
    control = control
  )
}



#' ML Estimation for Parametric Lifetime Distributions
#'
#' @inherit ml_estimation description details references
#'
#' @inheritParams ml_estimation
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is
#'   a right censored observation (= 0) or a failure (= 1).
#'
#' @template return-ml-estimation
#' @templateVar data A `tibble` with columns `x` and `status`.
#'
#' @seealso [ml_estimation]
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
#' ml <- ml_estimation(
#'   x = obs,
#'   status = status_1,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' # Example 2 - Fitting a three-parametric lognormal distribution:
#' ml_2 <- ml_estimation(
#'   x = cycles,
#'   status = status_2,
#'   distribution = "lognormal3"
#' )
#'
#' @md
#'
#' @export
ml_estimation.default <- function(x,
                                  status,
                                  distribution = c(
                                    "weibull", "lognormal", "loglogistic",
                                    "sev", "normal", "logistic",
                                    "weibull3", "lognormal3", "loglogistic3",
                                    "exponential", "exponential2"
                                  ),
                                  wts = rep(1, length(x)),
                                  conf_level = 0.95,
                                  start_dist_params = NULL,
                                  control = list(),
                                  ...
) {

  distribution <- match.arg(distribution)

  data <- tibble::tibble(x = x, status = status)

  ml_estimation_(
    data = data,
    distribution = distribution,
    wts = wts,
    conf_level = conf_level,
    start_dist_params = start_dist_params,
    control = control
  )
}



# Function that performs the parameter estimation:
ml_estimation_ <- function(data,
                           distribution,
                           wts,
                           conf_level,
                           start_dist_params, # initial parameter vector
                           control # control of optims control argument
) {

  # Prepare function inputs:
  x <- x_origin <- data$x # Used to compute the hessian for threshold models:
  status <- data$status

  ## Set initial values:
  if (purrr::is_null(start_dist_params)) {
    ### Vector of length 1 (scale) or 2 (location-scale) parameter(s):
    start_dist_params <- start_params(
      x = x,
      status = status,
      distribution = distribution
    )
    ### Add 'NA' for general handling of 'start_dist_params' (length 2 or 3):
    start_dist_params <- c(start_dist_params, NA_real_)
  } else {
    check_dist_params(start_dist_params, distribution)
    if (length(start_dist_params) == 1L) {
      ### Add 'NA' in case of 'exponential' distribution to ensure length 2:
      start_dist_params <- c(start_dist_params, NA_real_)
    }
  }

  ## Number of parameters, could be either 2 or 3:
  n_par <- length(start_dist_params)

  # Pre-Step: Threshold models must be profiled w.r.t threshold:
  if (has_thres(distribution)) {

    ## Define upper bound for constraint optimization:
    ### For 'exponential2' gamma is smaller or equal to t_min:
    upper <- min(x[status == 1])

    ### For other threshold distributions gamma is smaller than t_min:
    if (distribution != "exponential2") {
      upper <- (1 - (1 / 1e+5)) * upper
    }

    ## Initial value for threshold parameter:
    t0 <- start_dist_params[n_par] %NA% 0

    ## Optimization of `loglik_profiling_()`:
    opt_thres <- stats::optim(
      par = t0,
      fn = loglik_profiling_,
      method = "L-BFGS-B",
      lower = 0,
      upper = upper,
      control = list(fnscale = -1), # no user input for profiling!
      hessian = FALSE,
      x = x,
      status = status,
      wts = wts,
      distribution = distribution
    )

    opt_thres <- opt_thres$par

    ## Preparation for ML:
    x <- x - opt_thres
  }

  # Step 1: Estimation of one or two-parametric model (x or x - thres) using ML:
  ## Preparation:
  ### 'n_par' is 2 or 3 and must be reduced by 1L:
  n_par <- n_par - 1L

  ### Remove 'NA' or t0 since the latter (if exists) is included (x - opt_thres):
  start_dist_params <- start_dist_params[1:n_par]

  ### Force maximization:
  control$fnscale <- -1

  ### Use log scale (sigma is the last element of 'start_dist_params'):
  start_dist_params[n_par] <- log(start_dist_params[n_par])

  ## Optimization of one or two-parametric model:
  ml <- stats::optim(
    par = start_dist_params,
    fn = loglik_function_,
    method = "BFGS",
    control = control,
    hessian = TRUE,
    x = x,
    status = status,
    wts = wts,
    distribution = distribution,
    log_scale = TRUE
  )

  ## Parameters:
  dist_params <- ml$par

  ## Names:
  if (n_par == 1L) {
    names_par <- "sigma"
  } else {
    names_par <- c("mu", "sigma")
  }

  ### Determine the hessian matrix for threshold distributions:
  if (exists("opt_thres", inherits = FALSE)) {

    #### Concatenate parameters:
    dist_params <- c(dist_params, opt_thres)
    names_par[n_par + 1] <- "gamma"

    #### Compute hessian w.r.t to 'dist_params' and original 'x':
    ml$hessian <- stats::optimHess(
      par = dist_params,
      fn = loglik_function_,
      control = control,
      x = x_origin,
      status = status,
      wts = wts,
      distribution = distribution,
      log_scale = TRUE
    )

  }

  ## Set parameter names:
  names(dist_params) <- names_par

  ## Value of the log-likelihood at optimum:
  logL <- ml$value

  ## Variance-covariance matrix on log scale which is the inverse of the hessian:
  dist_varcov_logsigma <- solve(-ml$hessian)

  ## scale parameter on original scale:
  dist_params[n_par] <- exp(dist_params[n_par])

  ## Transformation to obtain variance-covariance matrix on original scale:
  trans_mat <- diag(length(dist_params))
  diag(trans_mat)[n_par] <- dist_params[n_par]

  dist_varcov <- trans_mat %*% dist_varcov_logsigma %*% trans_mat
  colnames(dist_varcov) <- rownames(dist_varcov) <- names(dist_params)

  # Step 2: Normal approximation confidence intervals:
  confint <- conf_normal_approx(
    dist_params = dist_params,
    dist_varcov = dist_varcov,
    conf_level = conf_level
  )

  # Step 3: Form output:
  ## Alternative parameters and confidence intervals for Weibull:
  l_wb <- list()

  ## Weibull distribution; providing shape-scale coefficients and confint:
  if (distribution %in% c("weibull", "weibull3")) {
    estimates <- to_shape_scale_params(dist_params)
    conf_int <- to_shape_scale_confint(confint)

    l_wb <- list(
      shape_scale_coefficients = estimates,
      shape_scale_confint = conf_int
    )
  }

  ## Exponential distribution; renaming 'sigma' with 'theta':
  if (std_parametric(distribution) == "exponential") {
    names(dist_params)[1] <- rownames(confint)[1] <- "theta"
    rownames(dist_varcov)[1] <- colnames(dist_varcov)[1] <- "theta"
  }

  n <- length(x_origin) # sample size
  k <- length(dist_params) # number of parameters

  ml_output <- c(
    list(
      coefficients = dist_params,
      confint = confint
    ),
    l_wb, # Empty, if not Weibull!
    list(
      varcov = dist_varcov,
      logL = logL,
      aic = -2 * logL + 2 * k,
      bic = -2 * logL + log(n) * k
    )
  )

  ml_output$data <- data
  ml_output$distribution <- distribution

  class(ml_output) <- c(
    "wt_model", "wt_ml_estimation", "wt_model_estimation", class(ml_output)
  )

  ml_output
}



#' @export
print.wt_ml_estimation <- function(x,
                                   digits = max(3L, getOption("digits") - 3L),
                                   ...
) {
  cat("Maximum Likelihood Estimation\n")
  NextMethod("print")
}
