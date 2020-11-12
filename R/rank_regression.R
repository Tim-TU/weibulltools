#' Rank Regression for Parametric Lifetime Distributions
#'
#' This method fits an \strong{x on y} regression to a linearized
#' two- or three-parameter cdf and is applicable for complete and (multiple) right
#' censored data. The parameters are estimated in the frequently used (log-) location-scale
#' parameterization. For the Weibull, estimates are transformed such that
#' they are in line with the parameterization provided by the \emph{stats} package
#' like \code{\link{pweibull}}.
#'
#' When using this method, the approximated confidence intervals for the Weibull
#' parameters (based on p. 51 of Ralf Mock) can only be estimated for the
#' following confidence levels:
#' \itemize{
#'   \item \code{conf_level} = 0.90,
#'   \item \code{conf_level} = 0.95,
#'   \item \code{conf_level} = 0.99.}
#'
#' If the distribution is not the Weibull, the confidence intervals of the
#' parameters are calculated using a heteroscedasticity-consistent covariance matrix.
#' Here it should be said that there is no statistical foundation to calculate the
#' standard errors for the parameters using \emph{Least Squares} in context of
#' \emph{Median Rank Regression}. For an accepted statistical method use MLE
#' (\code{\link{ml_estimation}}).
#'
#' @encoding UTF-8
#' @references
#'   \itemize{
#'     \item Mock, R., Methoden zur Datenhandhabung in
#'   Zuverlässigkeitsanalysen, vdf Hochschulverlag AG an der ETH Zürich, 1995
#'     \item Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998}
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param event A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution Supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#'   Other distributions have not been implemented yet.
#' @param conf_level Confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param data A tibble returned by \code{\link{estimate_cdf}}.
#'
#' @return Returns a list with the following components:
#'   \itemize{
#'   \item \code{coefficients} : Provided, if \code{distribution} is \code{"weibull"}
#'     or \code{"weibull3"}. \eqn{\eta} is the estimated scale and \eqn{\beta}
#'     the estimated shape parameter. The estimated threshold parameter \eqn{\gamma}
#'     is available if the three-parametric weibull is used.
#'   \item \code{confint} : Provided, if \code{distribution} is \code{"weibull"}
#'     or \code{"weibull3"}. Confidence intervals for \eqn{\eta} and \eqn{\beta}
#'     (and \eqn{\gamma} if the three-parametric weibull is used).
#'   \item \code{loc_sc_params} : Estimated location-scale parameters.
#'     Threshold parameter is provided for \code{"weibull3"}, \code{"lognormal3"}
#'     and \code{"loglogistic3"}.
#'   \item \code{loc_sc_confint} : Confidence intervals for location-scale parameters.
#'     If distribution is \code{"lognormal3"} or \code{"loglogistic3"} a confidence
#'     interval for the threshold is not computed.
#'   \item \code{loc_sc_varcov} : Provided, if \code{distribution} is not
#'     \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'     Variance-Covariance matrix of the used location-scale distribution.
#'   \item \code{r_squared} : Coefficient of determination.}
#'
#' @examples
#' # Example 1: Fitting a two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' df_john <- johnson_method(x = obs, event = state)
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull",
#'                        conf_level = .90)
#'
#' # Example 2: Fitting a three-parameter Weibull:
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#'
#' df_john <- johnson_method(x = cycles, event = state)
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#' @export
rank_regression <- function(x, ...) {
  UseMethod("rank_regression", x)
}

#' @describeIn rank_regression Perform the rank regression as part of the
#' reliability pipeline. \code{\link{estimate_cdf}} returns a data frame of class
#' \code{"cdf_estimation"}, which contains all information regarding x, y and
#' event.
#'
#' @export
rank_regression.cdf_estimation <- function(
  cdf_estimation,
  distribution = c("weibull", "lognormal", "loglogistic", "normal", "logistic",
                   "sev", "weibull3", "lognormal3", "loglogistic3"),
  conf_level = 0.95
) {
  rank_regression.default(
    x = cdf_estimation$characteristic,
    y = cdf_estimation$prob,
    event = cdf_estimation$status,
    distribution = match.arg(distribution),
    conf_level = conf_level
  )
}

#' @export
#' @describeIn rank_regression Provide x, y and event manually.
rank_regression.default <- function(
  x,
  y,
  event,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"
  ),
  conf_level = .95
) {

  distribution <- match.arg(distribution)

  # In terms of MRR only failed items can be used:
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  # Median-Rank-Regression Weibull:
  if (distribution == "weibull") {
    # Log-Location-Scale
    mrr <- stats::lm(log(x_f) ~ SPREDA::qsev(y_f))
    estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # Alternative Parameterization:
    estimates <- c(exp(stats::coef(mrr)[[1]]), 1 / stats::coef(mrr)[[2]])
    names(estimates) <- c("eta", "beta")

    if (conf_level == 0.90) {
      mock_val <- 1.4
    } else if (conf_level == 0.95) {
      mock_val <- 2
    } else if (conf_level == 0.99) {
      mock_val <- 3.4
    } else {
      stop("conf_level must be 0.90, 0.95 or 0.99")
    }

    conf_beta <- c(
      estimates[[2]] * 1 / (1 + sqrt(mock_val / length(x_f))),
      estimates[[2]] * (1 + sqrt(mock_val / length(x_f)))
    )
    conf_eta <- c(
      estimates[[1]] * (2 * length(x_f) / stats::qchisq(p = (1 + conf_level) / 2,
                                                        df = 2 * length(x_f) + 2)) ^ (1 / estimates[[2]]),
      estimates[[1]] * (2 * length(x_f) / stats::qchisq(p = (1 - conf_level) / 2,
                                                        df = 2 * length(x_f) + 2)) ^ (1 / estimates[[2]])
    )

    conf_ints <- matrix(c(conf_eta, conf_beta), byrow = TRUE, ncol = 2)
    colnames(conf_ints) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                             paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints) <- names(estimates)

    conf_ints_loc_sc <- matrix(c(log(conf_eta), rev(1 / conf_beta)),
                               byrow = TRUE, ncol = 2)
    colnames(conf_ints_loc_sc) <- colnames(conf_ints)
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    r_sq <- summary(mrr)$r.squared

    mrr_output <- list(coefficients = estimates, confint = conf_ints,
                       loc_sc_params = estimates_loc_sc,
                       loc_sc_confint = conf_ints_loc_sc,
                       r_squared = r_sq)
  }
  if (distribution == "weibull3" | distribution == "lognormal3" | distribution == "loglogistic3") {
    # Log-Location-Scale with threshold:
    ## Optimization of profile function:
    optim_gamma <- stats::optim(par = 0, fn = r_squared_profiling, method = "L-BFGS-B",
                                upper = (1 - (1 / 1e+5)) * min(x_f), lower = 0,
                                control = list(fnscale = -1), x = x_f, y = y_f,
                                distribution = distribution)

    ## Estimate of Threshold:
    estimate_gamma <- optim_gamma$par

    ## Subtracting Threshold and Estimation of Location-Scale:
    x_gamma <- x_f - estimate_gamma

    ### Defining subset for unusual case of x_gamma being smaller or equal to 0:
    subs <- x_gamma > 0

    if (distribution == "weibull3") {

      mrr <- stats::lm(log(x_gamma[subs]) ~ SPREDA::qsev(y_f[subs]))
      estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]], estimate_gamma)
      names(estimates_loc_sc) <- c("mu", "sigma", "gamma")

      # Alternative Parameterization
      estimates <- c(exp(stats::coef(mrr)[[1]]), 1 / stats::coef(mrr)[[2]], estimate_gamma)
      names(estimates) <- c("eta", "beta", "gamma")

      if (conf_level == 0.90) {
        mock_val <- 1.4
      } else if (conf_level == 0.95) {
        mock_val <- 2
      } else if (conf_level == 0.99) {
        mock_val <- 3.4
      } else {
        stop("conf_level must be 0.90, 0.95 or 0.99")
      }

      conf_beta <- c(
        estimates[[2]] * 1 / (1 + sqrt(mock_val / length(x_gamma[subs]))),
        estimates[[2]] * (1 + sqrt(mock_val / length(x_gamma[subs])))
      )
      conf_eta <- c(
        estimates[[1]] * (2 * length(x_gamma[subs]) / stats::qchisq(p = (1 + conf_level) / 2,
                                                                    df = 2 * length(x_gamma[subs]))) ^ (1 / estimates[[2]]),
        estimates[[1]] * (2 * length(x_gamma[subs]) / stats::qchisq(p = (1 - conf_level) / 2,
                                                                    df = 2 * length(x_gamma[subs]))) ^ (1 / estimates[[2]])
      )
      conf_gamma <- c(
        estimates[[3]] * (2 * length(x_gamma[subs]) / stats::qchisq(p = (1 + conf_level) / 2,
                                                                    df = 2 * length(x_gamma[subs]))) ^ (1 / estimates[[2]]),
        estimates[[3]] * (2 * length(x_gamma[subs]) / stats::qchisq(p = (1 - conf_level) / 2,
                                                                    df = 2 * length(x_gamma[subs]))) ^ (1 / estimates[[2]])
      )

      conf_ints <- matrix(c(conf_eta, conf_beta, conf_gamma), byrow = TRUE, ncol = 2)
      colnames(conf_ints) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                               paste(((1 + conf_level) / 2) * 100, "%"))
      rownames(conf_ints) <- names(estimates)

      conf_ints_loc_sc <- matrix(c(log(conf_eta), rev(1 / conf_beta), conf_gamma),
                                 byrow = TRUE, ncol = 2)
      colnames(conf_ints_loc_sc) <- colnames(conf_ints)
      rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

      r_sq <- summary(mrr)$r.squared

      mrr_output <- list(coefficients = estimates, confint = conf_ints,
                         loc_sc_params = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         r_squared = r_sq)

    } else if (distribution == "lognormal3" | distribution == "loglogistic3") {

      if (distribution == "lognormal3") {
        mrr <- stats::lm(log(x_gamma[subs]) ~ stats::qnorm(y_f[subs]))
      }

      if (distribution == "loglogistic3") {
        mrr <- stats::lm(log(x_gamma[subs]) ~ stats::qlogis(y_f[subs]))
      }

      estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]], estimate_gamma)
      names(estimates_loc_sc) <- c("mu", "sigma", "gamma")


      # Estimating a heteroscedasticity-consistent covariance matrix:
      vcov_loc_sc <- sandwich::vcovHC(x = mrr, type = "HC1")
      colnames(vcov_loc_sc) <- names(estimates_loc_sc)[-3]
      rownames(vcov_loc_sc) <- names(estimates_loc_sc)[-3]
      se <- sqrt(diag(vcov_loc_sc))

      # Confidence Intervals Location-Scale-Parameterization:
      conf_mu <- c(
        estimates_loc_sc[[1]] - stats::qt((1 + conf_level) / 2,
                                          df = length(x_gamma[subs]) - 2) * se[[1]] / sqrt(length(x_gamma[subs])),
        estimates_loc_sc[[1]] + stats::qt((1 + conf_level) / 2,
                                          df = length(x_gamma[subs]) - 2) * se[[1]] / sqrt(length(x_gamma[subs])))

      conf_sig <- c(
        estimates_loc_sc[[2]] - stats::qt((1 + conf_level) / 2,
                                          df = length(x_gamma[subs]) - 2) * se[[2]] / sqrt(length(x_gamma[subs])),
        estimates_loc_sc[[2]] + stats::qt((1 + conf_level) / 2,
                                          df = length(x_gamma[subs]) - 2) * se[[2]] / sqrt(length(x_gamma[subs])))

      conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig, NA, NA), byrow = TRUE,
                                 ncol = 2)
      colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                                      paste(((1 + conf_level) / 2) * 100, "%"))
      rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

      r_sq <- summary(mrr)$r.squared

      mrr_output <- list(loc_sc_params = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         loc_sc_varcov = vcov_loc_sc,
                         r_squared = r_sq)
    }
  }

  if (distribution == "lognormal" | distribution == "loglogistic" |
      distribution == "normal" | distribution == "logistic" | distribution == "sev") {

    if (distribution == "lognormal") {
      mrr <- stats::lm(log(x_f) ~ stats::qnorm(y_f))
    }
    if (distribution == "loglogistic") {
      mrr <- stats::lm(log(x_f) ~ stats::qlogis(y_f))
    }
    if (distribution == "normal") {
      mrr <- stats::lm(x_f ~ stats::qnorm(y_f))
    }
    if (distribution == "logistic") {
      mrr <- stats::lm(x_f ~ stats::qlogis(y_f))
    }
    if (distribution == "sev") {
      mrr <- stats::lm(x_f ~ SPREDA::qsev(y_f))
    }

    estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # Estimating a heteroscedasticity-consistent covariance matrix:
    vcov_loc_sc <- sandwich::vcovHC(x = mrr, type = "HC1")
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)
    se <- sqrt(diag(vcov_loc_sc))

    # Confidence Intervals Location-Scale-Parameterization:
    conf_mu <- c(
      estimates_loc_sc[[1]] - stats::qt((1 + conf_level) / 2,
                                        df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)),
      estimates_loc_sc[[1]] + stats::qt((1 + conf_level) / 2,
                                        df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)))

    conf_sig <- c(
      estimates_loc_sc[[2]] - stats::qt((1 + conf_level) / 2,
                                        df = length(x_f) - 2) * se[[2]] / sqrt(length(x_f)),
      estimates_loc_sc[[2]] + stats::qt((1 + conf_level) / 2,
                                        df = length(x_f) - 2) * se[[2]] / sqrt(length(x_f)))

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig), byrow = TRUE,
                               ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                                    paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    r_sq <- summary(mrr)$r.squared

    mrr_output <- list(loc_sc_params = estimates_loc_sc,
                       loc_sc_confint = conf_ints_loc_sc,
                       loc_sc_varcov = vcov_loc_sc,
                       r_squared = r_sq)
  }

  class(mrr_output) <- c("parameter_estimation", class(mrr_output))

  attr(mrr_output, "data") <- tibble::tibble(
    x = x, event = event
  )
  attr(mrr_output, "distribution") <- distribution

  return(mrr_output)
}
