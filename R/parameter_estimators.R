#' Rank Regression for the Weibull distribution
#'
#' This method fits an \strong{x on y} regression to the linearized
#' two-parameter Weibull cdf and is applicable for complete and (multiple) right
#' censored data. The weibull-specific parameters are estimated in the frequently
#' used location-scale parametrization and afterwards are transformed such that
#' they are in line with the parametrization provided by the \emph{stats} package
#' like \code{\link{pweibull}}.
#'
#' When using this method, the approximated confidence intervals (based on p. 51
#' of Ralf Mock) can only be estimated for the following confidence levels:
#' \itemize{
#'   \item \code{conf_level} = 0.90,
#'   \item \code{conf_level} = 0.95,
#'   \item \code{conf_level} = 0.99.}
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
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"weibull"}.
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}
#'
#' @return Returns a list with the following components:
#'   \itemize{
#'   \item \code{coefficients} : Where \eqn{\eta} is the scale parameter and
#'     \eqn{\beta} is the shape parameter
#'   \item \code{confint} : The estimated confidence intervals of the
#'     parameters.
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
#'                        conf_level = .90)

rank_regression <- function(x, y, event, distribution = "weibull",
                            conf_level = .95, details = TRUE) {
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  if (distribution == "weibull") {
    mrr <- lm(log(x_f) ~ SPREDA::qsev(y_f))
    estimates_loc_sc <- c(coef(mrr)[[1]], coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

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

    conf_ints_loc_sc <- matrix(c(log(conf_eta), 1 / conf_beta), byrow = TRUE,
                               ncol = 2)
    colnames(conf_ints_loc_sc) <- colnames(conf_ints)
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    r_sq <- summary(mrr)$r.squared

    if (details == TRUE) {
      mrr_output <- list(coefficients = estimates, confint = conf_ints,
        loc_sc_coefficients = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        r_squared = r_sq)
    } else {
      mrr_output <- list(coefficients = estimates)
    }
  }
  return(mrr_output)
}

#' ML Estimation for the Weibull distribution
#'
#' This method estimates the parameters and normal approximated confidence
#' intervals of a two-parameter Weibull distribution in the frequently used
#' location-scale parametrization. \code{ml_estimation} uses the
#' \code{\link{Lifedata.MLE}} function that is defined in the
#' \emph{SPREDA} package.
#' Afterwards the estimates are transformed such that they are in line with the
#' parametrization provided by the \emph{stats} package like
#' \code{\link{pweibull}}. The method is applicable for complete and (multiple)
#' right censored data.
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"weibull"}.
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}
#'
#' @return Returns a list with the following components:
#'   \itemize{
#'   \item \code{coefficients} : Where \eqn{\eta} is the scale parameter and
#'     \eqn{\beta} is the shape parameter
#'   \item \code{confint} : The estimated confidence intervals of the
#'     parameters.
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
ml_estimation <- function(x, event, distribution = "weibull",
                          conf_level = 0.95, details = TRUE) {
  if (distribution == "weibull") {
    ml <- SPREDA::Lifedata.MLE(survival::Surv(x, event) ~ 1,
                               dist = distribution)

    estimates_loc_sc <- c(SPREDA::coef.Lifedata.MLE(ml)[[1]],
                          SPREDA::coef.Lifedata.MLE(ml)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    vcov_loc_sc <- SPREDA::summary.Lifedata.MLE(ml)$vcov
    colnames(vcov_loc_sc) <- names(estimates_loc_sc)
    rownames(vcov_loc_sc) <- names(estimates_loc_sc)

    se_loc_sc <- sqrt(diag(vcov_loc_sc))

    conf_mu <- c(
      estimates_loc_sc[[1]] + qnorm((1 - conf_level) / 2) * se_loc_sc[[1]],
      estimates_loc_sc[[1]] + qnorm((1 + conf_level) / 2) * se_loc_sc[[1]])

    w <- exp(
          (qnorm((1 + conf_level) / 2) * se_loc_sc[[2]]) / estimates_loc_sc[[2]]
            )
    conf_sig <- c(estimates_loc_sc[[2]] /  w, estimates_loc_sc[[2]] * w)

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig), byrow = TRUE,
      ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100,
      "%"),
      paste(((1 + conf_level) / 2) * 100,
        "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

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
      ml_output <- list(coefficients = estimates)
    }
  }
  return(ml_output)
}
