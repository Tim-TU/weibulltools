#' ML Estimation for Parametric Lifetime Distributions
#'
#' @description
#' This function estimates the parameters of a two- or three-parameter lifetime
#' distribution for complete and (multiple) right-censored data. The parameters
#' are determined in the frequently used (log-)location-scale parameterization.
#'
#' For the Weibull, estimates are additionally transformed such that they are in
#' line with the parameterization provided by the *stats* package
#' (see [Weibull][stats::Weibull]).
#'
#' @details
#' To obtain the estimates, `ml_estimation` calls [Lifedata.MLE][SPREDA::Lifedata.MLE]
#' which is implemented in package *SPREDA*. Normal approximation confidence
#' intervals for the parameters are computed as well.
#'
#' @param x A `tibble` of class `wt_reliability_data` returned by [reliability_data].
#' @param distribution Supposed distribution of the random variable.
#' @param wts Optional vector of case weights. The length of `wts` must be equal
#' to the number of observations in `x`.
#' @param conf_level Confidence level of the interval.
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
                                                "loglogistic", "normal",
                                                "logistic", "sev", "weibull3",
                                                "lognormal3", "loglogistic3"
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
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is
#'   a right censored observation (= 0) or a failure (= 1).
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



# Function that performs the parameter estimation:
ml_estimation_ <- function(data,
                           distribution,
                           wts,
                           conf_level
) {

  x <- data$x
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
        coefficients = estimates_loc_sc,
        confint = conf_ints_loc_sc,
        varcov = vcov_loc_sc,
        shape_scale_coefficients = estimates,
        shape_scale_confint = conf_ints,
        logL = -ml$min,
        aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
        bic = (-2 * (-ml$min) + log(length(x)) * length(estimates_loc_sc))
      )
    } else {
      ml_output <- list(
        coefficients = estimates_loc_sc,
        confint = conf_ints_loc_sc,
        varcov = vcov_loc_sc,
        logL = -ml$min,
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
                                x = x, status = status, wts = wts, distribution = distribution)

    ## Estimate of Threshold:
    estimate_gamma <- optim_gamma$par

    ## Subtracting Threshold
    x_gamma <- x - estimate_gamma

    ## Defining subset for unusual case of x_gamma being smaller or equal to 0:
    subs <- x_gamma > 0

    ml_init <- SPREDA::Lifedata.MLE(survival::Surv(x_gamma[subs], status[subs]) ~ 1,
                                    dist = two_parametric(distribution),
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
        coefficients = estimates_loc_sc,
        confint = conf_ints_loc_sc,
        varcov = vcov_loc_sc,
        shape_scale_coefficients = estimates,
        shape_scale_confint = conf_ints,
        logL = ml$value,
        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
        bic = (-2 * (ml$value) + log(length(x)) * length(estimates_loc_sc))
      )

    } else {
      ml_output <- list(
        coefficients = estimates_loc_sc,
        confint = conf_ints_loc_sc,
        varcov = vcov_loc_sc,
        logL = ml$value,
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
      confint = conf_ints_loc_sc,
      varcov = vcov_loc_sc,
      logL = ml$value,
      aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
      bic = (-2 * (ml$value) +
               log(length(x)) * length(estimates_loc_sc))
    )
  }

  class(ml_output) <- c(
    "wt_model", "wt_ml_estimation", "wt_model_estimation", class(ml_output)
  )

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
print.wt_ml_estimation <- function(x,
                                   digits = max(3L, getOption("digits") - 3L),
                                   ...
) {
  cat("Maximum Likelihood Estimation\n")
  NextMethod("print")
}
