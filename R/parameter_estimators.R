#' Rank Regression for Parametric Lifetime Distributions
#'
#' This method fits an \strong{x on y} regression to a linearized
#' two- or three-parameter cdf and is applicable for complete and (multiple) right
#' censored data. The parameters are estimated in the frequently used
#' location-scale parametrization. For the Weibull, estimates are transformed such that
#' they are in line with the parametrization provided by the \emph{stats} package
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
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y a numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#'   Other distributions have not been implemented yet.
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param details a logical variable, where the default value is \code{TRUE}.
#'   If \code{FALSE} the output consists of a list that only contains the
#'   estimated parameters. If \code{TRUE} the output is a detailed list with
#'   many more information. See below (\strong{Value}).
#'
#' @return Returns a list with the following components (depending on
#'   \code{details} argument):
#'   \itemize{
#'   \item \code{coefficients} : Provided, if \code{distribution} is \code{"weibull"}
#'     or \code{"weibull3"}. \eqn{\eta} is the estimated scale and \eqn{\beta}
#'     the estimated shape parameter. The estimated threshold parameter \eqn{\gamma}
#'     is available if the three-parametric weibull is used.
#'   \item \code{confint} : Provided, if \code{distribution} is \code{"weibull"}
#'     or \code{"weibull3"}. Confidence intervals for \eqn{\eta} and \eqn{\beta}
#'     (and \eqn{\gamma} if the three-parametric weibull is used).
#'   \item \code{loc_sc_coefficients} : Estimated location-scale parameters.
#'     Threshold parameter is provided for \code{"weibull3"}, \code{"lognormal3"}
#'     and \code{"loglogistic3"}.
#'   \item \code{loc_sc_confint} : Confidence intervals for location-scale parameters.
#'     If distribution is \code{"lognormal3"} or \code{"loglogistic3"} a confidence
#'     interval for the threshold is not computed.
#'   \item \code{loc_sc_vcov} : Provided, if \code{distribution} is not
#'     \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'     Variance-Covariance matrix of the used location-scale distribution.
#'   \item \code{r_squared} : Coefficient of determination.}
#' @export
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

rank_regression <- function(x, y, event,
                            distribution = c("weibull", "lognormal", "loglogistic",
                                             "normal", "logistic", "sev", "weibull3",
                                             "lognormal3", "loglogistic3"),
                            conf_level = .95, details = TRUE) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }

  # In terms of MRR only failed items can be used
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  # Median-Rank-Regression Weibull:
  if (distribution == "weibull") {
    # Log-Location-Scale
    mrr <- stats::lm(log(x_f) ~ SPREDA::qsev(y_f))
    estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # Alternative Parametrization
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
                                                 df = 2 * length(x_f))) ^ (1 / estimates[[2]]),
      estimates[[1]] * (2 * length(x_f) / stats::qchisq(p = (1 - conf_level) / 2,
                                                 df = 2 * length(x_f))) ^ (1 / estimates[[2]])
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

    if (details == TRUE) {
      mrr_output <- list(coefficients = estimates, confint = conf_ints,
                         loc_sc_coefficients = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         r_squared = r_sq)
    } else {
      mrr_output <- list(coefficients = estimates,
        loc_sc_coefficients = estimates_loc_sc)
    }
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

      # Alternative Parametrization
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

      if (details == TRUE) {
        mrr_output <- list(coefficients = estimates, confint = conf_ints,
          loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          r_squared = r_sq)
      } else {
        mrr_output <- list(coefficients = estimates,
          loc_sc_coefficients = estimates_loc_sc)
      }

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

      # Confidence Intervals Location-Scale-Parametrization:
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

      if (details == TRUE) {
        mrr_output <- list(loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          loc_sc_vcov = vcov_loc_sc,
          r_squared = r_sq)
      } else {
        mrr_output <- list(loc_sc_coefficients = estimates_loc_sc)
      }

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

    # Confidence Intervals Location-Scale-Parametrization:
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

    if (details == TRUE) {
      mrr_output <- list(loc_sc_coefficients = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         loc_sc_vcov = vcov_loc_sc,
                         r_squared = r_sq)
    } else {
      mrr_output <- list(loc_sc_coefficients = estimates_loc_sc)
    }
  }
  return(mrr_output)
}

#' \eqn{R²}-Profile Function for Log-Location-Scale Distributions with Threshold
#'
#' This function evaluates the coefficient of determination with respect to a
#' given threshold parameter of a three-parametric lifetime distribution.
#' In terms of \emph{Median Rank Regression} this function can be optimized
#' (\code{\link{optim}}) to estimate the threshold parameter.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y a numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param thres a numeric value of the threshold parameter.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#' @return Returns the coefficient of determination for a specified threshold value.
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
#' df_john <- johnson_method(x = cycles, event = state)
#'
#' # Determining threshold parameter for which the coefficient of determination is
#' # maximized subject to the condition that the threshold parameter must be smaller
#' # as the first failure cycle, i.e 94:
#' threshold <- seq(0, min(cycles[state == 1]) - 0.1, length.out = 1000)
#' profile_r2 <- sapply(threshold, r_squared_profiling,
#'                      x = df_john$characteristic[df_john$status == 1],
#'                      y = df_john$prob[df_john$status == 1],
#'                      distribution = "weibull3")
#' threshold[which.max(profile_r2)]
#'
#' # plot:
#' # plot(threshold, profile_r2, type = "l")
#' # abline(v = threshold[which.max(profile_r2)], h = max(profile_r2), col = "red")

r_squared_profiling <- function(x, y, thres, distribution = c("weibull3", "lognormal3",
                                                              "loglogistic3")) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull3", "lognormal3", "loglogistic3"))) {
    stop("No valid distribution!")
  }

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Rank Regression adjusted x on y:
  if (distribution == "weibull3") {
    mrr_thres <- stats::lm(log(x_thres) ~ SPREDA::qsev(y))
  }
  if (distribution == "lognormal3") {
    mrr_thres <- stats::lm(log(x_thres) ~ stats::qnorm(y))
  }
  if (distribution == "loglogistic3") {
    mrr_thres <- stats::lm(log(x_thres) ~ stats::qlogis(y))
  }
  summary(mrr_thres)$r.squared
}

#' ML Estimation for Parametric Lifetime Distributions
#'
#' This method estimates the parameters and calculates normal approximation confidence
#' intervals for a two- or three-parametric lifetime distribution in the frequently used
#' location-scale parametrization. \code{ml_estimation} uses the
#' \code{\link{Lifedata.MLE}} function which is defined in the
#' \emph{SPREDA} package.
#' For the Weibull the estimates are transformed such that they are in line with
#' the parametrization provided by the \emph{stats} package like
#' \code{\link{pweibull}}. The method is applicable for complete and (multiple)
#' right censored data.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#'   Other distributions have not been implemented yet.
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param details a logical variable, where the default value is \code{TRUE}.
#'   If \code{FALSE} the output consists of a list that only contains the
#'   estimated parameters. If \code{TRUE} the output is a detailed list with
#'   many more information. See below (\strong{Value}).
#'
#'
#' @return Returns a list with the following components (depending on
#' \code{details} argument):
#'   \itemize{
#'   \item \code{coefficients} : Provided, if \code{distribution} is \code{"weibull"}.
#'     \eqn{\eta} is the estimated scale and \eqn{\beta} the estimated shape parameter.
#'   \item \code{confint} : Provided, if \code{distribution} is \code{"weibull"}.
#'     Confidence interval for \eqn{\eta} and \eqn{\beta}.
#'   \item \code{loc_sc_coefficients} : Estimated location-scale parameters.
#'   \item \code{loc_sc_confint} : Confidence interval for location-scale parameters.
#'   \item \code{loc_sc_vcov} : Estimated Variance-Covariance matrix of the used
#'     location-scale distribution.
#'   \item \code{logL} : The log-likelihood value.
#'   \item \code{aic} : Akaike Information Criterion.
#'   \item \code{bic} : Bayesian Information Criterion.}
#' @export
#'
#' @examples
#' # Example 1: Fitting a two-parameter Weibull:
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' mle <- ml_estimation(x = obs, event = state,
#'                      distribution = "weibull", conf_level = 0.90)
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
#' mle_weib3 <- ml_estimation(x = cycles, event = state,
#'                            distribution = "weibull3", conf_level = 0.95)
#'
ml_estimation <- function(x, event,
                          distribution = c("weibull", "lognormal", "loglogistic",
                                           "normal", "logistic", "sev", "weibull3",
                                           "lognormal3", "loglogistic3"),
                          conf_level = .95, details = TRUE) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }

  # Log-Location-Scale Models:
  if (distribution == "weibull" | distribution == "lognormal" | distribution == "loglogistic") {
    # ML - Estimation: Location-Scale Parameters
    ml <- SPREDA::Lifedata.MLE(survival::Surv(x, event) ~ 1, dist = distribution)
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
      # Alternative Parametrization, only for Weibull:
      estimates <- c(exp(estimates_loc_sc[[1]]), 1 / estimates_loc_sc[[2]])
      names(estimates) <- c("eta", "beta")

      conf_ints <- matrix(c(exp(conf_mu), rev(1 / conf_sig)), byrow = TRUE,
        ncol = 2)
      colnames(conf_ints) <- colnames(conf_ints_loc_sc)
      rownames(conf_ints) <- names(estimates)
      if (details == TRUE) {
        ml_output <- list(coefficients = estimates, confint = conf_ints,
          loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          loc_sc_vcov = vcov_loc_sc, logL = -ml$min,
          aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
          bic = (-2 * (-ml$min) +
                log(length(x)) * length(estimates_loc_sc))
        )
      } else {
        ml_output <- list(coefficients = estimates,
          loc_sc_coefficients = estimates_loc_sc)
      }
    } else {
      if (details == TRUE) {
        ml_output <- list(loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          loc_sc_vcov = vcov_loc_sc, logL = -ml$min,
          aic = -2 * (-ml$min) + 2 * length(estimates_loc_sc),
          bic = (-2 * (-ml$min) +
            log(length(x)) * length(estimates_loc_sc))
        )
      } else {
        ml_output <- list(loc_sc_coefficients = estimates_loc_sc)
      }
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
      x = x, event = event, distribution = distribution)

    ## Estimate of Threshold:
    estimate_gamma <- optim_gamma$par

    ## Subtracting Threshold
    x_gamma <- x - estimate_gamma

    ## Defining subset for unusual case of x_gamma being smaller or equal to 0:
    subs <- x_gamma > 0

    ml_init <- SPREDA::Lifedata.MLE(survival::Surv(x_gamma[subs], event[subs]) ~ 1,
                                    dist = substr(distribution, start = 1, stop = nchar(distribution) - 1))

    ## Initial parameters:
    estimates_init <- c(SPREDA::coef.Lifedata.MLE(ml_init)[[1]],
      SPREDA::coef.Lifedata.MLE(ml_init)[[2]], estimate_gamma)

    ## Optimization of log-likelihood function with threshold:
    ml <- stats::optim(par = c(estimates_init[[1]], estimates_init[[2]],
      estimates_init[[3]]), fn = loglik_function, method = "L-BFGS-B",
      lower = c(0, 0, 0), upper = c(Inf, Inf, (1 - (1 / 1e+5)) * min(x[event == 1])),
      control = list(fnscale = -1), x = x, event = event,
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
      # Alternative Parametrization, only for Weibull:
      estimates <- c(exp(estimates_loc_sc[[1]]), 1 / estimates_loc_sc[[2]],
                     estimates_loc_sc[[3]])
      names(estimates) <- c("eta", "beta", "gamma")

      conf_ints <- matrix(c(exp(conf_mu), rev(1 / conf_sig), conf_gamma), byrow = TRUE,
        ncol = 2)
      colnames(conf_ints) <- colnames(conf_ints_loc_sc)
      rownames(conf_ints) <- names(estimates)

      if (details == TRUE) {
        ml_output <- list(coefficients = estimates, confint = conf_ints,
          loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          loc_sc_vcov = vcov_loc_sc, logL = ml$value,
          aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
          bic = (-2 * (ml$value) +
              log(length(x)) * length(estimates_loc_sc))
        )
      } else {
        ml_output <- list(coefficients = estimates,
          loc_sc_coefficients = estimates_loc_sc)
      }

    } else {
      if (details == TRUE) {
        ml_output <- list(loc_sc_coefficients = estimates_loc_sc,
          loc_sc_confint = conf_ints_loc_sc,
          loc_sc_vcov = vcov_loc_sc, logL = ml$value,
          aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
          bic = (-2 * (ml$value) +
              log(length(x)) * length(estimates_loc_sc))
        )
      } else {
        ml_output <- list(loc_sc_coefficients = estimates_loc_sc)
      }
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
    ml_init <- survival::survreg(survival::Surv(x, event) ~ 1, dist = distr)

    ## Initial parameters:
    estimates_init <- c(stats::coef(ml_init)[[1]], ml_init$scale)

    ## Optimization of log-likelihood function:
    ml <- stats::optim(par = c(estimates_init[[1]], estimates_init[[2]]),
                       fn = loglik_function, method = "BFGS",
                       control = list(fnscale = -1), x = x, event = event,
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

    if (details == TRUE) {
      ml_output <- list(loc_sc_coefficients = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_vcov = vcov_loc_sc, logL = ml$value,
        aic = -2 * (ml$value) + 2 * length(estimates_loc_sc),
        bic = (-2 * (ml$value) +
            log(length(x)) * length(estimates_loc_sc))
      )
    } else {
      ml_output <- list(loc_sc_coefficients = estimates_loc_sc)
    }
  }
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
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param thres a numeric value of the threshold parameter.
#' @param distribution supposed distribution of the random variable. The
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
#' threshold <- seq(0, min(cycles[state == 1]) - 0.1, length.out = 1000)
#' profile_logL <- sapply(threshold, loglik_profiling,
#'                      x = cycles,
#'                      event = state,
#'                      distribution = "weibull3")
#' threshold[which.max(profile_logL)]
#'
#' # plot:
#' # plot(threshold, profile_logL, type = "l")
#' # abline(v = threshold[which.max(profile_logL)], h = max(profile_logL), col = "red")

loglik_profiling <- function(x, event, thres, distribution = c("weibull3",
                                                               "lognormal3",
                                                               "loglogistic3")) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull3", "lognormal3", "loglogistic3"))) {
    stop("No valid distribution!")
  }

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Log-Likelihood profiling:
  ml_thres <- SPREDA::Lifedata.MLE(survival::Surv(x_thres, event) ~ 1,
    dist = substr(distribution, start = 1, stop = nchar(distribution) - 1))
 -ml_thres$min
}

#' Log-Likelihood Function for (Log-)Location-Scale Distributions (with Threshold)
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
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param pars a numeric vector of parameters. The first element is the location
#'   parameter (\eqn{\mu}), the second is the scale parameter (\eqn{\sigma}) and if
#'   a three-parametric model is used the third element is the threshold parameter
#'   (\eqn{\gamma}).
#' @param distribution supposed distribution of the random variable. The
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
#' loglik_weib <- loglik_function(x = cycles, event = state, pars = c(5.29, 0.33),
#'                                distribution = "weibull")
#'
#' # Example 2: Evaluating Log-Likelihood function of three-parametric weibull:
#' loglik_weib3 <- loglik_function(x = cycles, event = state,
#'                                 pars = c(4.54, 0.76, 92.99),
#'                                 distribution = "weibull3")

loglik_function <- function(x, event, pars,
                            distribution = c("weibull", "lognormal", "loglogistic",
                                             "normal", "logistic", "sev", "weibull3",
                                             "lognormal3", "loglogistic3")) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }

  d <- event
  mu <- pars[1]
  sig <- pars[2]
  thres <- pars[3]

  if (is.na(thres)) {
    # Log- and Location-Scale Models:
    if (distribution == "weibull") {
      logL <- sum(log(((1 / (sig * x)) * SPREDA::dsev((log(x) - mu) / sig)) ^ d *
                  (1 - SPREDA::psev((log(x) - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "lognormal") {
      logL <- sum(log(((1 / (sig * x)) * stats::dnorm((log(x) - mu) / sig)) ^ d *
                  (1 - stats::pnorm((log(x) - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "loglogistic") {
      logL <- sum(log(((1 / (sig * x)) * stats::dlogis((log(x) - mu) / sig)) ^ d *
                  (1 - stats::plogis((log(x) - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "sev") {
      logL <- sum(log(((1 / sig) * SPREDA::dsev((x - mu) / sig)) ^ d *
                  (1 - SPREDA::psev((x - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "normal") {
      logL <- sum(log(((1 / sig) * stats::dnorm((x - mu) / sig)) ^ d *
                  (1 - stats::pnorm((x - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "logistic") {
      logL <- sum(log(((1 / sig) * stats::dlogis((x - mu) / sig)) ^ d *
                  (1 - stats::plogis((x - mu) / sig)) ^ (1 - d)))
    }
  } else {
    # Log-Location-Scale Models with threshold parameter:

    ### Defining subset for unusual case that difference between x and thres is smaller or equal to 0:
    subs <- (x - thres) > 0
    xs <- x[subs]
    d <- d[subs]

    if (distribution == "weibull3") {
      logL <- sum(log(((1 / (sig * (xs - thres))) * SPREDA::dsev((log(xs - thres) - mu) / sig)) ^ d *
                  (1 - SPREDA::psev((log(xs - thres) - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "lognormal3") {
      logL <- sum(log(((1 / (sig * (xs - thres))) * stats::dnorm((log(xs - thres) - mu) / sig)) ^ d *
                  (1 - stats::pnorm((log(xs - thres) - mu) / sig)) ^ (1 - d)))
    }
    if (distribution == "loglogistic3") {
      logL <- sum(log(((1 / (sig * (xs - thres))) * stats::dlogis((log(xs - thres) - mu) / sig)) ^ d *
                  (1 - stats::plogis((log(xs - thres) - mu) / sig)) ^ (1 - d)))
    }
  }
  logL
}
