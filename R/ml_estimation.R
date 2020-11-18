#' ML Estimation for Parametric Lifetime Distributions
#'
#' This method estimates the parameters and calculates normal approximation confidence
#' intervals for a two- or three-parametric lifetime distribution in the frequently used
#' (log-) location-scale parameterization. \code{ml_estimation} uses the
#' \code{\link{Lifedata.MLE}} function which is defined in the
#' \emph{SPREDA} package.
#' For the Weibull the estimates are transformed such that they are in line with
#' the parameterization provided by the \emph{stats} package like
#' \code{\link{pweibull}}. The method is applicable for complete and (multiple)
#' right censored data.
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=ml_estimation.reliability_data]{reliability_data}}}{
#'     Preferred. Provide the output of \code{\link{reliability_data}} directly.
#'   }
#'   \item{\code{\link[=ml_estimation.default]{default}}}{
#'     Provide \code{x} and \code{status} manually.
#'   }
#' }
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a list with the following components:
#'   \itemize{
#'   \item \code{coefficients} : Provided, if \code{distribution} is \code{"weibull"}.
#'     \eqn{\eta} is the estimated scale and \eqn{\beta} the estimated shape parameter.
#'   \item \code{confint} : Provided, if \code{distribution} is \code{"weibull"}.
#'     Confidence interval for \eqn{\eta} and \eqn{\beta}.
#'   \item \code{loc_sc_params} : Estimated location-scale parameters.
#'   \item \code{loc_sc_confint} : Confidence interval for location-scale parameters.
#'   \item \code{loc_sc_varcov} : Estimated Variance-Covariance matrix of the used
#'     location-scale distribution.
#'   \item \code{logL} : The log-likelihood value.
#'   \item \code{aic} : Akaike Information Criterion.
#'   \item \code{bic} : Bayesian Information Criterion.}
#'
#' @export
#'
ml_estimation <- function(x, ...) {
  UseMethod("ml_estimation")
}



#' ML Estimation for Parametric Lifetime Distributions
#'
#' @inherit ml_estimation description details return references
#'
#' @param x An object of class \code{reliability_data} returned by
#'   \code{\link{reliability_data}}.
#' @param distribution Supposed distribution of the random variable.
#' @param wts Optional vector of case weights. The length of \code{wts} must be the
#'   same as the number of observations \code{x}. Default is that \code{wts} is a
#'   vector with all components being 1 (same weights).
#' @param conf_level Confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#'
#' @examples
#' # Example 1: Fitting a two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' data <- reliability_data(x = obs, status = status)
#'
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' # Example 2: Fitting a three-parameter Weibull:
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#' data_2 <- reliability_data(x = cycles, status = status)
#'
#' mle_weib3 <- ml_estimation(
#'   data,
#'   distribution = "weibull3",
#'   conf_level = 0.95
#' )
#'
#' @export
#'
ml_estimation.reliability_data <- function(
  data,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"),
  wts = rep(1, nrow(data)),
  conf_level = .95
) {
  distribution <- match.arg(distribution)

  ml_estimation_(
    data,
    distribution = distribution,
    wts = wts,
    conf_level = conf_level
  )
}



#' ML Estimation for Parametric Lifetime Distributions
#'
#' @inherit ml_estimation description details return references
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution Supposed distribution of the random variable.
#' @param wts Optional vector of case weights. The length of \code{wts} must be the
#'   same as the number of observations \code{x}. Default is that \code{wts} is a
#'   vector with all components being 1 (same weights).
#' @param conf_level Confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#'
#' @examples
#' # Example 1: Fitting a two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' data <- reliability_data(x = obs, status = status)
#'
#' mle <- ml_estimation(
#'   data,
#'   distribution = "weibull",
#'   conf_level = 0.90
#' )
#'
#' # Example 2: Fitting a three-parameter Weibull:
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#' data_2 <- reliability_data(x = cycles, status = status)
#'
#' mle_weib3 <- ml_estimation(
#'   data,
#'   distribution = "weibull3",
#'   conf_level = 0.95
#' )
#'
#' @export
#'
ml_estimation.default <- function(
  x, status,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"),
  wts = rep(1, length(x)),
  conf_level = .95
) {

  distribution <- match.arg(distribution)

  data <- reliability_data(x = x, status = status, id = "")

  ml_estimation_(data, distribution, wts, conf_level)
}

ml_estimation_ <- function(data, distribution, wts, conf_level) {
  x <- data$x
  status <- data$status

  # Log-Location-Scale Models:
  if (distribution == "weibull" | distribution == "lognormal" | distribution == "loglogistic") {
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

      ml_output <- list(coefficients = estimates, confint = conf_ints,
                        loc_sc_params = estimates_loc_sc,
                        loc_sc_confint = conf_ints_loc_sc,
                        loc_sc_varcov = vcov_loc_sc, logL = -ml$min,
                        aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
                        bic = (-2 * (-ml$min) +
                                 log(length(x)) * length(estimates_loc_sc))
      )
    } else {
      ml_output <- list(loc_sc_params = estimates_loc_sc,
                        loc_sc_confint = conf_ints_loc_sc,
                        loc_sc_varcov = vcov_loc_sc, logL = -ml$min,
                        aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
                        bic = (-2 * (-ml$min) +
                                 log(length(x)) * length(estimates_loc_sc))
      )
    }
  }

  # Log-Location-Scale Models with threshold parameter:
  if (distribution == "weibull3" | distribution == "lognormal3" | distribution == "loglogistic3") {

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

      ml_output <- list(coefficients = estimates, confint = conf_ints,
                        loc_sc_params = estimates_loc_sc,
                        loc_sc_confint = conf_ints_loc_sc,
                        loc_sc_varcov = vcov_loc_sc, logL = ml$value,
                        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
                        bic = (-2 * (ml$value) +
                                 log(length(x)) * length(estimates_loc_sc))
      )

    } else {
      ml_output <- list(loc_sc_params = estimates_loc_sc,
                        loc_sc_confint = conf_ints_loc_sc,
                        loc_sc_varcov = vcov_loc_sc, logL = ml$value,
                        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
                        bic = (-2 * (ml$value) +
                                 log(length(x)) * length(estimates_loc_sc))
      )
    }
  }
  # Location-Scale Models:
  if (distribution == "sev" | distribution == "normal" | distribution == "logistic") {
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

    ml_output <- list(loc_sc_params = estimates_loc_sc,
                      loc_sc_confint = conf_ints_loc_sc,
                      loc_sc_varcov = vcov_loc_sc, logL = ml$value,
                      aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
                      bic = (-2 * (ml$value) +
                               log(length(x)) * length(estimates_loc_sc))
    )
  }

  class(ml_output) <- c("model_estimation", class(ml_output))

  ml_output$data <- tibble::tibble(
    characteristic = x, status = status
  )

  ml_output$distribution <- distribution

  return(ml_output)
}

#' Log-Likelihood Profile Function for Log-Location-Scale Distributions with Threshold
#'
#' This function evaluates the log-likelihood with respect to a given threshold
#' parameter of a three-parametric lifetime distribution. In terms of
#' \emph{Maximum Likelihood Estimation} this function can be optimized (\code{\link{optim}})
#' to estimate the threshold parameter.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param thres A numeric value of the threshold parameter.
#' @param distribution Supposed distribution of the random variable. The
#'   value can be \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#' @return Returns the log-likelihood value for a specified threshold value.
#' @export
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#'
#' # Determining threshold parameter for which the log-likelihood is maximized
#' # subject to the condition that the threshold parameter must be smaller
#' # as the first failure cycle, i.e 94:
#' threshold <- seq(0, min(cycles[state == 1]) - 0.1, length.out = 100)
#' profile_logL <- sapply(threshold, loglik_profiling,
#'                      x = cycles,
#'                      status = state,
#'                      distribution = "weibull3")
#' threshold[which.max(profile_logL)]
#'
#' # plot:
#' # plot(threshold, profile_logL, type = "l")
#' # abline(v = threshold[which.max(profile_logL)], h = max(profile_logL), col = "red")
loglik_profiling <- function(
  x,
  status,
  thres,
  distribution = c(
    "weibull3", "lognormal3", "loglogistic3"
  )
) {

  distribution <- match.arg(distribution)

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Log-Likelihood profiling:
  ml_thres <- SPREDA::Lifedata.MLE(survival::Surv(x_thres, status) ~ 1,
                                   dist = substr(distribution, start = 1, stop = nchar(distribution) - 1))
  -ml_thres$min
}

#' Log-Likelihood Function for (Log-) Location-Scale Distributions (with Threshold)
#'
#' This function computes the log-likelihood value with respect to a given set
#' of parameters. For two-parametric models the location and scale parameters
#' are required. If a three-parametric lifetime distribution is needed an
#' additional threshold parameter is required. In terms of
#' \emph{Maximum Likelihood Estimation} this function can be optimized (\code{\link{optim}})
#' to estimate the parameters and variance-covariance matrix of the parameters.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param wts Optional vector of case weights. The length of \code{wts} must be the
#'   same as the number of observations \code{x}. Default is that \code{wts} is a
#'   vector with all components being 1 (same weights).
#' @param pars A numeric vector of parameters. The first element is the location
#'   parameter (\eqn{\mu}), the second is the scale parameter (\eqn{\sigma}) and if
#'   a three-parametric model is used the third element is the threshold parameter
#'   (\eqn{\gamma}).
#' @param distribution Supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#'   Other distributions have not been implemented yet.
#' @export
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#'
#' # Example 1: Evaluating Log-Likelihood function of two-parametric weibull:
#' loglik_weib <- loglik_function(x = cycles, status = state, pars = c(5.29, 0.33),
#'                                distribution = "weibull")
#'
#' # Example 2: Evaluating Log-Likelihood function of three-parametric weibull:
#' loglik_weib3 <- loglik_function(x = cycles, status = state,
#'                                 pars = c(4.54, 0.76, 92.99),
#'                                 distribution = "weibull3")

loglik_function <- function(
  x,
  status,
  wts = rep(1, length(x)),
  pars,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"
  )
) {

  distribution <- match.arg(distribution)

  d <- status
  mu <- pars[1]
  sig <- pars[2]
  thres <- pars[3]

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
