#' Log-Likelihood Function for Parametric Lifetime Distributions
#'
#' @description
#' This function computes the log-likelihood value with respect to a given set
#' of parameters. For two-parametric models the location and scale parameters
#' are required. If a three-parametric lifetime distribution is needed an
#' additional threshold parameter has to be provided. In terms of
#' *Maximum Likelihood Estimation* this function can be optimized
#' ([optim][stats::optim]) to estimate the parameters and variance-covariance
#' matrix of the parameters.
#'
#' @inheritParams ml_estimation
#' @inheritParams predict_quantile
#'
#' @return
#' Returns the log-likelihood value for the parameters in `dist_params` given
#' the data.
#'
#' @encoding UTF-8
#'
#' @template dist-params
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @examples
#' # Reliability data preparation:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Example 1 - Evaluating Log-Likelihood function of two-parametric weibull:
#' loglik_weib <- loglik_function(
#'   x = data,
#'   dist_params = c(5.29, 0.33),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Evaluating Log-Likelihood function of three-parametric weibull:
#' loglik_weib3 <- loglik_function(
#'   x = data,
#'   dist_params = c(4.54, 0.76, 92.99),
#'   distribution = "weibull3"
#' )
#'
#' @md
#'
#' @export
loglik_function <- function(x, ...) {
  UseMethod("loglik_function")
}



#' @rdname loglik_function
#'
#' @export
loglik_function.wt_reliability_data <- function(
                                      x,
                                      wts = rep(1, nrow(x)),
                                      dist_params,
                                      distribution = c(
                                        "weibull", "lognormal", "loglogistic",
                                        "sev", "normal", "logistic",
                                        "weibull3", "lognormal3", "loglogistic3"
                                      ),
                                      ...
) {

  # Call `loglik_function.default()`:
  loglik_function.default(
    x = x$x,
    status = x$status,
    wts = wts,
    dist_params = dist_params,
    distribution = distribution
  )
}

#' Log-Likelihood Function for Parametric Lifetime Distributions
#'
#' @inherit loglik_function description return references
#'
#' @inheritParams ml_estimation.default
#' @inheritParams predict_quantile
#'
#' @encoding UTF-8
#'
#' @template dist-params
#'
#' @seealso [loglik_function]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Example 1 - Evaluating Log-Likelihood function of two-parametric weibull:
#' loglik_weib <- loglik_function(
#'   x = cycles,
#'   status = status,
#'   dist_params = c(5.29, 0.33),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Evaluating Log-Likelihood function of three-parametric weibull:
#' loglik_weib3 <- loglik_function(
#'   x = cycles,
#'   status = status,
#'   dist_params = c(4.54, 0.76, 92.99),
#'   distribution = "weibull3"
#' )
#'
#' @md
#'
#' @export
loglik_function.default <- function(x,
                                    status,
                                    wts = rep(1, length(x)),
                                    dist_params,
                                    distribution = c(
                                      "weibull", "lognormal", "loglogistic",
                                      "sev", "normal", "logistic",
                                      "weibull3", "lognormal3", "loglogistic3"
                                    ),
                                    ...
) {

  distribution <- match.arg(distribution)

  check_dist_params(dist_params, distribution)

  loglik_function_(
    x = x,
    status = status,
    wts = wts,
    dist_params = dist_params,
    distribution = distribution
  )
}



# Helper function for log-likelihood calculation:
loglik_function_ <- function(x,
                             status,
                             wts,
                             dist_params,
                             distribution,
                             log_scale = FALSE

) {

  d <- status
  mu <- dist_params[1]
  sig <- dist_params[2]

  if (log_scale) sig <- exp(sig)

  # Three-parametric model:
  if (length(dist_params) == 3L) {
    thres <- dist_params[3]
    x <- x - thres
    ## Restriction of three-parametric models, i.e. x > threshold parameter:
    subs <- x > 0
    x <- x[subs]
    d <- d[subs]
    wts <- wts[subs]
  }

  distribution <- std_parametric(distribution)

  # Switch between distributions:
  switch(distribution,
         "weibull" = {
           z <- (log(x) - mu) / sig
           ds <- dsev(z) / (sig * x)
         },
         "lognormal" = {
           z <- (log(x) - mu) / sig
           ds <- stats::dnorm(z) / (sig * x)
         },
         "loglogistic" = {
           z <- (log(x) - mu) / sig
           ds <- stats::dlogis(z) / (sig * x)
         },
         "sev" = {
           z <- (x - mu) / sig
           ds <- dsev(z) / sig
         },
         "normal" = {
           z <- (x - mu) / sig
           ds <- stats::dnorm(z) / sig
         },
         "logistic" = {
           z <- (x - mu) / sig
           ds <- stats::dlogis(z) / sig
         }
  )

  ps <- p_std(z, distribution)

  # Compute log-likelihood:
  logL_i <- d * log(ds) + (1 - d) * log(1 - ps)
  logL <- sum(wts * logL_i)
  logL
}



#' Log-Likelihood Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @description
#' This function evaluates the log-likelihood with respect to a given threshold
#' parameter of a three-parametric lifetime distribution. In terms of
#' *Maximum Likelihood Estimation* this function can be optimized
#' ([optim][stats::optim]) to estimate the threshold parameter.
#'
#'
#' @inheritParams ml_estimation
#' @param thres A numeric value for the threshold parameter.
#' @param distribution Supposed three-parametric distribution of the random variable.
#'
#' @return
#' Returns the log-likelihood value for the threshold parameter `thres` given
#' the data.
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @examples
#' # Reliability data preparation:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Determining the optimal loglikelihood value:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(data$x[data$status == 1]) - 0.1,
#'   length.out = 50
#' )
#'
#' ## loglikelihood value with respect to threshold values:
#' profile_logL <- loglik_profiling(
#'   x = data,
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the
#' ## loglikelihood:
#' threshold[which.max(profile_logL)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_logL,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_logL)],
#'   h = max(profile_logL),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
loglik_profiling <- function(x, ...) {
  UseMethod("loglik_profiling")
}



#' @rdname loglik_profiling
#'
#' @export
loglik_profiling.wt_reliability_data <- function(
                                      x,
                                      wts = rep(1, nrow(x)),
                                      thres,
                                      distribution = c(
                                        "weibull3", "lognormal3", "loglogistic3"
                                      ),
                                      ...
) {

  # Call `loglik_profiling.default()`:
  loglik_profiling.default(
    x = x$x,
    status = x$status,
    wts = wts,
    thres = thres,
    distribution = distribution
  )
}



#' Log-Likelihood Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @inherit loglik_profiling description return references
#'
#' @inheritParams ml_estimation.default
#' @param thres A numeric value for the threshold parameter.
#' @param distribution Supposed three-parametric distribution of the random variable.
#'
#' @encoding UTF-8
#'
#' @seealso [loglik_profiling]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Determining the optimal loglikelihood value:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(cycles[status == 1]) - 0.1,
#'   length.out = 50
#' )
#'
#' ## loglikelihood value with respect to threshold values:
#' profile_logL <- loglik_profiling(
#'   x = cycles,
#'   status = status,
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the
#' ## loglikelihood:
#' threshold[which.max(profile_logL)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_logL,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_logL)],
#'   h = max(profile_logL),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
loglik_profiling.default <- function(x,
                                     status,
                                     wts = rep(1, length(x)),
                                     thres,
                                     distribution = c(
                                       "weibull3", "lognormal3", "loglogistic3"
                                     ),
                                     ...
) {

  distribution <- match.arg(distribution)

  loglik_prof_vectorized <- Vectorize(
    FUN = loglik_profiling_,
    vectorize.args = "thres"
  )

  loglik_prof_vectorized(
    x = x,
    status = status,
    wts = wts,
    thres = thres,
    distribution = distribution
  )
}



# Function to perform profiling with `optim()` routine:
loglik_profiling_ <- function(x,
                              status,
                              wts,
                              thres,
                              distribution
) {

  d <- status
  x <- x - thres

  ## Restriction of three-parametric models, i.e. x > threshold parameter:
  subs <- x > 0
  x <- x[subs]
  d <- d[subs]
  wts <- wts[subs]

  ## Set distribution to parametric version without threshold:
  distribution <- std_parametric(distribution)

  ## Initial parameters of two-parametric model:
  start_dist_params <- start_params(
    x = x,
    status = d,
    distribution = distribution
  )

  ## Use log scale:
  start_dist_params[2] <- log(start_dist_params[2])

  ## Log-Likelihood profiling:
  logL_profile <- stats::optim(
    par = start_dist_params,
    fn = loglik_function_,
    method = "BFGS",
    control = list(fnscale = -1),
    x = x,
    status = d,
    wts = wts,
    distribution = distribution,
    log_scale = TRUE
  )

  logL_profile$value
}
