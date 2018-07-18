#' Rank Regression for Two-Parameter Lifetime Distributions
#'
#' This method fits an \strong{x on y} regression to the linearized
#' two-parameter cdf and is applicable for complete and (multiple) right
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
#'
#' @references Mock, R., Methoden zur Datenhandhabung in
#'   Zuverlässigkeitsanalysen, vdf Hochschulverlag AG an der ETH Zürich, 1995
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
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
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
#'   \item \code{coefficients} : Provided, if \code{distribution} is \code{"weibull"}.
#'     \eqn{\eta} is the estimated scale and \eqn{\beta} the estimated shape parameter.
#'   \item \code{confint} : Provided, if \code{distribution} is \code{"weibull"}.
#'     Confidence interval for \eqn{\eta} and \eqn{\beta}.
#'   \item \code{loc_sc_coefficients} : Estimated location-scale parameters.
#'   \item \code{loc_sc_confint} : Confidence interval for location-scale parameters.
#'   \item \code{loc_sc_vcov} : Provided, if \code{distribution} is not
#'     \code{"weibull"}. Estimated heteroscedasticity-consistent
#'     Variance-Covariance matrix of the used location-scale distribution.
#'   \item \code{r_squared} : Coefficient of determination.}
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' df_john <- johnson_method(x = obs, event = state)
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull",
#'                        conf_level = .90)

rank_regression <- function(x, y, event, distribution = c("weibull", "lognormal",
  "loglogistic"), conf_level = .95, details = TRUE) {

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  x_f <- x[event == 1]
  y_f <- y[event == 1]

  # Median-Rank-Regression Weibull:
  if (distribution == "weibull") {
    # Location-Scale
    mrr <- lm(log(x_f) ~ SPREDA::qsev(y_f))
    estimates_loc_sc <- c(coef(mrr)[[1]], coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # Alternative Parametrization
    estimates <- c(exp(coef(mrr)[[1]]), 1 / coef(mrr)[[2]])
    names(estimates) <- c("eta", "beta")

    if (conf_level == 0.95) {
      mock_val <- 2
    } else if (conf_level == 0.90) {
      mock_val <- 1.4
    } else if (conf_level == 0.99) {
      mock_val <- 3.4
    } else {
      stop("conf_level must be 0.90, 0.95 or 0.99")
    }

    conf_beta <- c(estimates[[2]] * 1 / (1 + sqrt(mock_val / length(x_f))),
                   estimates[[2]] * (1 + sqrt(mock_val / length(x_f))))
    conf_eta <- c(
      estimates[[1]] * (2 * length(x_f) / qchisq(p = (1 + conf_level) / 2,
                                                 df = 2 * length(x_f))) ^ (1 / estimates[[2]]),
      estimates[[1]] * (2 * length(x_f) / qchisq(p = (1 - conf_level) / 2,
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
  } else {
    if (distribution == "lognormal") {
      mrr <- lm(log(x_f) ~ qnorm(y_f))
    }
    if (distribution == "loglogistic") {
      mrr <- lm(log(x_f) ~ qlogis(y_f))
    }

    estimates_loc_sc <- c(coef(mrr)[[1]], coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    # Estimating a heteroscedasticity-consistent covariance matrix:
    vcov_loc_sc <- sandwich::vcovHC(x = mrr, type = "HC1")
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)
    se <- sqrt(diag(vcov_loc_sc))

    # Confidence Intervals Location-Scale-Parametrization:
    conf_mu <- c(
      estimates_loc_sc[[1]] - qt((1 + conf_level) / 2,
        df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)),
      estimates_loc_sc[[1]] + qt((1 + conf_level) / 2,
        df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)))

    conf_sig <- c(
      estimates_loc_sc[[2]] - qt((1 + conf_level) / 2,
        df = length(x_f) - 2) * se[[2]] / sqrt(length(x_f)),
      estimates_loc_sc[[2]] + qt((1 + conf_level) / 2,
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

#' ML Estimation for Two-Parameter Lifetime Distributions
#'
#' This method estimates the parameters and calculates normal approximation confidence
#' intervals for a two-parameter lifetime distribution in the frequently used
#' location-scale parametrization. \code{ml_estimation} uses the
#' \code{\link{Lifedata.MLE}} function that is defined in the
#' \emph{SPREDA} package.
#' For the Weibull the estimates are transformed such that they are in line with
#' the parametrization provided by the \emph{stats} package like
#' \code{\link{pweibull}}. The method is applicable for complete and (multiple)
#' right censored data.
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
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
#'   \item \code{logL} : The log-likelihood value.}
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' mle <- ml_estimation(x = obs, event = state,
#'                      distribution = "weibull", conf_level = 0.90)
#'
ml_estimation <- function(x, event, distribution = c("weibull", "lognormal",
  "loglogistic"), conf_level = 0.95, details = TRUE) {

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

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
    estimates_loc_sc[[1]] + qnorm((1 - conf_level) / 2) * se_loc_sc[[1]],
    estimates_loc_sc[[1]] + qnorm((1 + conf_level) / 2) * se_loc_sc[[1]])

  w <- exp(
    (qnorm((1 + conf_level) / 2) * se_loc_sc[[2]]) / estimates_loc_sc[[2]])
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
        loc_sc_vcov = vcov_loc_sc, logL = -ml$min)
    } else {
      ml_output <- list(coefficients = estimates,
        loc_sc_coefficients = estimates_loc_sc)
    }
  } else {
    if (details == TRUE) {
      ml_output <- list(loc_sc_coefficients = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_vcov = vcov_loc_sc, logL = -ml$min)
    } else {
      ml_output <- list(loc_sc_coefficients = estimates_loc_sc)
    }
  }
  return(ml_output)
}
