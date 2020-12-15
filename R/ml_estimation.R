#' ML Estimation for Parametric Lifetime Distributions
#'
#' @description
#' This function estimates the parameters of a two- or three-parametric lifetime
#' distribution for complete and (multiple) right censored data. The parameters
#' are determined in the frequently used (log-)location-scale parameterization.
#'
#' For the Weibull, estimates are transformed such that they are in line with the
#' parameterization provided by the \emph{stats} package (see \link[stats]{Weibull}).
#'
#' @details
#' \code{ml_estimation} calls \code{\link[SPREDA:lifedata.MLE]{Lifedata.MLE}},
#' which is implemented in \emph{SPREDA}, to obtain the estimates. Normal
#' approximation confidence intervals for the parameters are computed as well.
#'
#' @param x Object of class \code{reliability_data} returned by
#'   \code{\link{reliability_data}}.
#' @param distribution Supposed distribution of the random variable.
#' @param wts Optional vector of case weights. The length of \code{wts} must be the
#'   same as the number of observations in \code{x}.
#' @param conf_level Confidence level of the interval.
#' @template dots
#'
#' @return Returns a list with the classes \code{"ml_estimation"} and
#'   \code{"model_estimation"} containing the following elements:
#'   \itemize{
#'     \item \code{coefficients} : If \code{distribution} is \code{"weibull"}, the
#'       estimated scale (\eqn{\eta}) and shape (\eqn{\beta}) parameters are provided
#'       (and additionally the threshold parameter \eqn{\gamma} if \code{distribution}
#'       is \code{"weibull3"}). For any other distribution, \code{coefficients} is
#'       identical to \code{loc_sc_params}.
#'     \item \code{confint} : Only included if \code{distribution} is \code{"weibull"}
#'       or \code{"weibull3"}. Confidence intervals for \eqn{\eta} and \eqn{\beta}
#'       (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'     \item \code{loc_sc_params} : Estimated (log-)location-scale parameters.
#'       Threshold parameter is included if \code{distribution} is \code{"weibull3"},
#'       \code{"lognormal3"} or \code{"loglogistic3"}.
#'     \item \code{loc_sc_confint} : Confidence intervals the (log-)location-scale
#'       parameters.
#'     \item \code{loc_sc_varcov} : Estimated variance-covariance matrix for the
#'       (log-)location-scale parameters.
#'     \item \code{logL} : The log-likelihood value.
#'     \item \code{aic} : Akaike Information Criterion.
#'     \item \code{bic} : Bayesian Information Criterion.
#'     \item \code{data} : A tibble with class \code{"reliability_data"} returned
#'       by \code{\link{reliability_data}}.
#'     \item \code{distribution} : Specified distribution.
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
#' # Example 1 - Fitting a two-parametric weibull distribution:
#' ml_2p <- ml_estimation(
#'   x = data_2p,
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Fitting a three-parametric lognormal distribution:
#' ml_3p <- ml_estimation(
#'   x = data_3p,
#'   distribution = "lognormal3",
#'   conf_level = 0.99
#' )
#'
#' @export
ml_estimation <- function(x, ...) {
  UseMethod("ml_estimation")
}



#' @rdname ml_estimation
#'
#' @export
ml_estimation.reliability_data <- function(x,
                                           distribution = c(
                                             "weibull", "lognormal", "loglogistic",
                                             "normal", "logistic", "sev",
                                             "weibull3", "lognormal3", "loglogistic3"
                                           ),
                                           wts = rep(1, nrow(x)),
                                           conf_level = 0.95,
                                           ...
) {

  distribution <- match.arg(distribution)

  ml_estimation_(
    x,
    distribution = distribution,
    wts = wts,
    conf_level = conf_level
  )
}



#' ML Estimation for Parametric Lifetime Distributions
#'
#' @inherit ml_estimation description details return references
#'
#' @inheritParams ml_estimation
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#'
#' @return Returns a list with the classes \code{"ml_estimation"} and
#'   \code{"model_estimation"} containing the following elements:
#'   \itemize{
#'     \item \code{coefficients} : If \code{distribution} is \code{"weibull"}, the
#'       estimated scale (\eqn{\eta}) and shape (\eqn{\beta}) parameters are provided
#'       (and additionally the threshold parameter \eqn{\gamma} if \code{distribution}
#'       is \code{"weibull3"}). For any other distribution, \code{coefficients} is
#'       identical to \code{loc_sc_params}.
#'     \item \code{confint} : Only included if \code{distribution} is \code{"weibull"}
#'       or \code{"weibull3"}. Confidence intervals for \eqn{\eta} and \eqn{\beta}
#'       (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'     \item \code{loc_sc_params} : Estimated (log-)location-scale parameters.
#'       Threshold parameter is included if \code{distribution} is \code{"weibull3"},
#'       \code{"lognormal3"} or \code{"loglogistic3"}.
#'     \item \code{loc_sc_confint} : Confidence intervals the (log-)location-scale
#'       parameters.
#'     \item \code{loc_sc_varcov} : Estimated variance-covariance matrix for the
#'       (log-)location-scale parameters.
#'     \item \code{logL} : The log-likelihood value.
#'     \item \code{aic} : Akaike Information Criterion.
#'     \item \code{bic} : Bayesian Information Criterion.
#'     \item \code{data} : A tibble with columns \code{x} and \code{status}.
#'     \item \code{distribution} : Specified distribution.
#'   }
#'
#' @seealso \code{\link{ml_estimation}}
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
#' @export
ml_estimation.default <- function(x,
                                  status,
                                  distribution = c(
                                    "weibull", "lognormal", "loglogistic",
                                    "normal", "logistic", "sev",
                                    "weibull3", "lognormal3", "loglogistic3"
                                  ),
                                  wts = rep(1, length(x)),
                                  conf_level = 0.95,
                                  ...
) {

  distribution <- match.arg(distribution)

  data <- reliability_data(x = x, status = status, id = "")

  ml_estimation_(data, distribution, wts, conf_level)
}



ml_estimation_ <- function(data,
                           distribution,
                           wts,
                           conf_level
) {

  x <- if (inherits(data, "reliability_data")) get_characteristic(data) else data$x
  status <- data$status
  id <- data$id

  # Log-Location-Scale Models:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    # ML - Estimation: Location-Scale Parameters
    ml <- SPREDA::Lifedata.MLE(survival::Surv(x, status) ~ 1, dist = distribution,
                               weights = wts)
    estimates_loc_sc <- c(SPREDA::coef.Lifedata.MLE(ml)[[1]],
                          SPREDA::coef.Lifedata.MLE(ml)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # ML-Estimation: Var-Cov-Matrix of Location-Scale-Parameters
    vcov_loc_sc <- SPREDA::summary.Lifedata.MLE(ml)$vcov
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)

    # Standard Error
    se_loc_sc <- sqrt(diag(vcov_loc_sc))

    # Confidence Intervals: Location and Scale
    conf_mu <- c(
      estimates_loc_sc[[1]] + stats::qnorm((1 - conf_level) / 2) * se_loc_sc[[1]],
      estimates_loc_sc[[1]] + stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[1]])

    w <- exp(
      (stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[2]]) / estimates_loc_sc[[2]])
    conf_sig <- c(estimates_loc_sc[[2]] /  w, estimates_loc_sc[[2]] * w)

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig), byrow = TRUE, ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100,"%"),
                                    paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    if (distribution == "weibull") {
      # Alternative Parameterization, only for Weibull:
      estimates <- c(exp(estimates_loc_sc[[1]]), 1 / estimates_loc_sc[[2]])
      names(estimates) <- c("eta", "beta")

      conf_ints <- matrix(c(exp(conf_mu), rev(1 / conf_sig)), byrow = TRUE,
                          ncol = 2)
      colnames(conf_ints) <- colnames(conf_ints_loc_sc)
      rownames(conf_ints) <- names(estimates)

      ml_output <- list(
        coefficients = estimates,
        confint = conf_ints,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_varcov = vcov_loc_sc, logL = -ml$min,
        aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
        bic = (-2 * (-ml$min) + log(length(x)) * length(estimates_loc_sc))
      )
    } else {
      ml_output <- list(
        coefficients = estimates_loc_sc,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_varcov = vcov_loc_sc, logL = -ml$min,
        aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
        bic = (-2 * (-ml$min) + log(length(x)) * length(estimates_loc_sc))
      )
    }
  }

  # Log-Location-Scale Models with threshold parameter:
  if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {

    # Log-Location-Scale with threshold:
    ## Problem:  With functions SPREDA::Lifedata.MLE and survival::survreg it is
    ##           not possible to estimate var-cov-matrix of log-location scale parameters and
    ##           threshold.
    ## Solution: A preliminary parameter estimation is done with likelihood profiling for
    ##           threshold and MLE (using Lifedata.MLE) for location and scale parameters.
    ##           These estimates will be used as initial values to maximize the log-likelihood of
    ##           log-location scale distributions with threshold.

    ## Optimization of profile log-likelihood function:
    optim_gamma <- stats::optim(par = 0, fn = loglik_profiling, method = "L-BFGS-B",
                                upper = (1 - (1 / 1e+5)) * min(x), lower = 0, control = list(fnscale = -1),
                                x = x, status = status, distribution = distribution)

    ## Estimate of Threshold:
    estimate_gamma <- optim_gamma$par

    ## Subtracting Threshold
    x_gamma <- x - estimate_gamma

    ## Defining subset for unusual case of x_gamma being smaller or equal to 0:
    subs <- x_gamma > 0

    ml_init <- SPREDA::Lifedata.MLE(survival::Surv(x_gamma[subs], status[subs]) ~ 1,
                                    dist = substr(distribution, start = 1, stop = nchar(distribution) - 1),
                                    weights = wts)

    ## Initial parameters:
    estimates_init <- c(SPREDA::coef.Lifedata.MLE(ml_init)[[1]],
                        SPREDA::coef.Lifedata.MLE(ml_init)[[2]], estimate_gamma)

    ## Optimization of log-likelihood function with threshold:
    ml <- stats::optim(par = c(estimates_init[[1]], estimates_init[[2]],
                               estimates_init[[3]]), fn = loglik_function, method = "L-BFGS-B",
                       lower = c(0, 0, 0), upper = c(Inf, Inf, (1 - (1 / 1e+5)) * min(x[status == 1])),
                       control = list(fnscale = -1), x = x, status = status, wts = wts,
                       distribution = distribution, hessian = TRUE)

    ## Estimated parameters:
    estimates_loc_sc <- ml$par
    names(estimates_loc_sc) <- c("mu", "sigma", "gamma")

    # ML-Estimation: Var-Cov-Matrix of Log-Location-Scale-Parameters and Threshold:
    vcov_loc_sc <- solve(-ml$hessian)
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)

    # Standard Error:
    se_loc_sc <- sqrt(diag(vcov_loc_sc))

    # Confidence Intervals - Location, Scale and Threshold:
    conf_mu <- c(
      estimates_loc_sc[[1]] + stats::qnorm((1 - conf_level) / 2) * se_loc_sc[[1]],
      estimates_loc_sc[[1]] + stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[1]])

    w <- exp(
      (stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[2]]) / estimates_loc_sc[[2]])
    conf_sig <- c(estimates_loc_sc[[2]] /  w, estimates_loc_sc[[2]] * w)

    conf_gamma <- c(
      estimates_loc_sc[[3]] + stats::qnorm((1 - conf_level) / 2) * se_loc_sc[[3]],
      estimates_loc_sc[[3]] + stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[3]])

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig, conf_gamma), byrow = TRUE, ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100,"%"),
                                    paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    if (distribution == "weibull3") {
      # Alternative Parameterization, only for Weibull:
      estimates <- c(exp(estimates_loc_sc[[1]]), 1 / estimates_loc_sc[[2]],
                     estimates_loc_sc[[3]])
      names(estimates) <- c("eta", "beta", "gamma")

      conf_ints <- matrix(c(exp(conf_mu), rev(1 / conf_sig), conf_gamma), byrow = TRUE,
                          ncol = 2)
      colnames(conf_ints) <- colnames(conf_ints_loc_sc)
      rownames(conf_ints) <- names(estimates)

      ml_output <- list(
        coefficients = estimates,
        confint = conf_ints,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_varcov = vcov_loc_sc, logL = ml$value,
        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
        bic = (-2 * (ml$value) + log(length(x)) * length(estimates_loc_sc))
      )

    } else {
      ml_output <- list(
        coefficients = estimates_loc_sc,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_varcov = vcov_loc_sc, logL = ml$value,
        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
        bic = (-2 * (ml$value) + log(length(x)) * length(estimates_loc_sc))
      )
    }
  }
  # Location-Scale Models:
  if (distribution %in% c("sev", "normal", "logistic")) {
    # Location-Scale:
    ## Problem:  With functions SPREDA::Lifedata.MLE it is not possible to estimate
    ##           the parameters of a location-scale distribution and with survival::survreg it is
    ##           not possible to estimate var-cov-matrix of location-scale parameters.
    ## Solution: A preliminary parameter estimation is done with MLE (using survreg)
    ##           for location-scale distributions.
    ##           These estimates will be used as initial values to maximize the log-likelihood of
    ##           location-scale distributions.

    ## rename distributions:
    if (distribution == "sev") {
      distr = "extreme"
    } else if (distribution == "normal") {
      distr = "gaussian"
    } else {
      distr = distribution
    }
    ## Initial estimation step:
    ml_init <- survival::survreg(survival::Surv(x, status) ~ 1, dist = distr,
                                 weights = wts)

    ## Initial parameters:
    estimates_init <- c(stats::coef(ml_init)[[1]], ml_init$scale)

    ## Optimization of log-likelihood function:
    ml <- stats::optim(par = c(estimates_init[[1]], estimates_init[[2]]),
                       fn = loglik_function, method = "BFGS",
                       control = list(fnscale = -1), x = x, status = status, wts = wts,
                       distribution = distribution, hessian = TRUE)

    ## Estimated parameters:
    estimates_loc_sc <- ml$par
    names(estimates_loc_sc) <- c("mu", "sigma")

    # ML-Estimation: Var-Cov-Matrix of Location-Scale-Parameters:
    vcov_loc_sc <- solve(-ml$hessian)
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)

    # Standard Error:
    se_loc_sc <- sqrt(diag(vcov_loc_sc))

    # Confidence Intervals - Location and Scale:
    conf_mu <- c(
      estimates_loc_sc[[1]] + stats::qnorm((1 - conf_level) / 2) * se_loc_sc[[1]],
      estimates_loc_sc[[1]] + stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[1]])

    w <- exp(
      (stats::qnorm((1 + conf_level) / 2) * se_loc_sc[[2]]) / estimates_loc_sc[[2]])
    conf_sig <- c(estimates_loc_sc[[2]] /  w, estimates_loc_sc[[2]] * w)

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig), byrow = TRUE, ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100,"%"),
                                    paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    ml_output <- list(
      coefficients = estimates_loc_sc,
      loc_sc_params = estimates_loc_sc,
      loc_sc_confint = conf_ints_loc_sc,
      loc_sc_varcov = vcov_loc_sc, logL = ml$value,
      aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
      bic = (-2 * (ml$value) +
               log(length(x)) * length(estimates_loc_sc))
    )
  }

  class(ml_output) <- c("ml_estimation", "model_estimation", class(ml_output))

  if (all(id == "", na.rm = TRUE)) {
    ml_output$data <- tibble::tibble(
      x = x, status = status
    )
  } else {
    ml_output$data <- data
  }

  ml_output$distribution <- distribution

  return(ml_output)
}



#' @export
print.ml_estimation <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                ...
) {
  cat("Maximum Likelihood Estimation\n")
  NextMethod("print")
}



#' Log-Likelihood Profile Function for Log-Location-Scale Distributions with Threshold
#'
#' @description
#' This function evaluates the log-likelihood with respect to a given threshold
#' parameter of a three-parametric lifetime distribution. In terms of
#' \emph{Maximum Likelihood Estimation} this function can be optimized (\code{\link{optim}})
#' to estimate the threshold parameter.
#'
#' @inheritParams r_squared_profiling.default
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#'
#' @return
#' Returns the log-likelihood value for the data with respect to the threshold
#' parameter \code{thres}.
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
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
#'   length.out = 100
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
#' @export
loglik_profiling <- function(x,
                             status,
                             thres,
                             distribution = c(
                               "weibull3", "lognormal3", "loglogistic3"
                             )
) {

  distribution <- match.arg(distribution)

  loglik_prof_vectorized <- Vectorize(
    FUN = loglik_profiling_,
    vectorize.args = "thres"
  )

  loglik_prof_vectorized(
    x = x,
    status = status,
    thres = thres,
    distribution = distribution
  )
}


loglik_profiling_ <- function(x,
                              status,
                              thres,
                              distribution = c(
                                "weibull3", "lognormal3", "loglogistic3"
                              )
) {

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Log-Likelihood profiling:
  ml_thres <- SPREDA::Lifedata.MLE(
    survival::Surv(x_thres, status) ~ 1,
    dist = substr(distribution, start = 1, stop = nchar(distribution) - 1)
  )

  -ml_thres$min
}



#' Log-Likelihood Function for (Log-) Location-Scale Distributions (with Threshold)
#'
#' @description
#' This function computes the log-likelihood value with respect to a given set
#' of parameters. For two-parametric models the location and scale parameters
#' are required. If a three-parametric lifetime distribution is needed an
#' additional threshold parameter has to be provided. In terms of
#' \emph{Maximum Likelihood Estimation} this function can be optimized (\code{\link{optim}})
#' to estimate the parameters and variance-covariance matrix of the parameters.
#'
#' @inheritParams ml_estimation.default
#' @inheritParams predict_quantile
#'
#' @return
#' Returns the log-likelihood value for the data with respect to the parameters
#' given in \code{loc_sc_params}.
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
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
#'   loc_sc_params = c(5.29, 0.33),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Evaluating Log-Likelihood function of three-parametric weibull:
#' loglik_weib3 <- loglik_function(
#'   x = cycles,
#'   status = status,
#'   loc_sc_params = c(4.54, 0.76, 92.99),
#'   distribution = "weibull3"
#' )
#'
#' @export
loglik_function <- function(x,
                            status,
                            wts = rep(1, length(x)),
                            loc_sc_params,
                            distribution = c(
                              "weibull", "lognormal", "loglogistic",
                              "normal", "logistic", "sev",
                              "weibull3", "lognormal3", "loglogistic3"
                            )
) {

  distribution <- match.arg(distribution)

  d <- status
  mu <- loc_sc_params[1]
  sig <- loc_sc_params[2]
  thres <- loc_sc_params[3]

  if (is.na(thres)) {
    # Log- and Location-Scale Models:
    if (distribution == "weibull") {
      logL <- sum(wts * (log(((1 / (sig * x)) * SPREDA::dsev((log(x) - mu) / sig)) ^ d *
                               (1 - SPREDA::psev((log(x) - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "lognormal") {
      logL <- sum(wts * (log(((1 / (sig * x)) * stats::dnorm((log(x) - mu) / sig)) ^ d *
                               (1 - stats::pnorm((log(x) - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "loglogistic") {
      logL <- sum(wts * (log(((1 / (sig * x)) * stats::dlogis((log(x) - mu) / sig)) ^ d *
                               (1 - stats::plogis((log(x) - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "sev") {
      logL <- sum(wts * (log(((1 / sig) * SPREDA::dsev((x - mu) / sig)) ^ d *
                               (1 - SPREDA::psev((x - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "normal") {
      logL <- sum(wts * (log(((1 / sig) * stats::dnorm((x - mu) / sig)) ^ d *
                               (1 - stats::pnorm((x - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "logistic") {
      logL <- sum(wts * (log(((1 / sig) * stats::dlogis((x - mu) / sig)) ^ d *
                               (1 - stats::plogis((x - mu) / sig)) ^ (1 - d))))
    }
  } else {
    # Log-Location-Scale Models with threshold parameter:

    ### Defining subset for unusual case that difference between x and thres is smaller or equal to 0:
    subs <- (x - thres) > 0
    xs <- x[subs]
    d <- d[subs]
    wts <- wts[subs]

    if (distribution == "weibull3") {
      logL <- sum(wts * (log(((1 / (sig * (xs - thres))) * SPREDA::dsev((log(xs - thres) - mu) / sig)) ^ d *
                               (1 - SPREDA::psev((log(xs - thres) - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "lognormal3") {
      logL <- sum(wts * (log(((1 / (sig * (xs - thres))) * stats::dnorm((log(xs - thres) - mu) / sig)) ^ d *
                               (1 - stats::pnorm((log(xs - thres) - mu) / sig)) ^ (1 - d))))
    }
    if (distribution == "loglogistic3") {
      logL <- sum(wts * (log(((1 / (sig * (xs - thres))) * stats::dlogis((log(xs - thres) - mu) / sig)) ^ d *
                               (1 - stats::plogis((log(xs - thres) - mu) / sig)) ^ (1 - d))))
    }
  }
  logL
}
