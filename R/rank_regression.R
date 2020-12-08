#' Rank Regression for Parametric Lifetime Distributions
#'
#' @description
#' This method fits an \strong{x on y} regression to a linearized two- or
#' three-parameter lifetime distribution for complete and (multiple) right
#' censored data. The parameters are determined in the frequently used
#' (log-)location-scale parameterization.
#'
#' For the Weibull, estimates are transformed such that they are in line with the
#' parameterization provided by the \emph{stats} package (see \link[stats]{Weibull}).
#'
#' @details
#' If \code{distribution} is \code{"weibull"} or \code{"weibull3"}, the approximated
#' confidence intervals for the parameters can only be estimated on the following
#' confidence levels (see 'References' \emph{(Mock, 1995)}):
#' \itemize{
#'   \item \code{conf_level} = 0.90,
#'   \item \code{conf_level} = 0.95,
#'   \item \code{conf_level} = 0.99.
#' }
#'
#' If the distribution is not the Weibull, the confidence intervals of the
#' parameters are computed on the basis of a heteroscedasticity-consistent
#' covariance matrix. Here it should be said that there is no statistical foundation
#' to determine the standard errors of the parameters using \emph{Least Squares}
#' in context of \emph{Rank Regression}. For an accepted statistical method use
#' \link[=ml_estimation]{maximum likelihood}.
#'
#' @param x An object of class \code{cdf_estimation} returned from
#'  \code{\link{estimate_cdf}}.
#' @param distribution Supposed distribution of the random variable.
#' @param conf_level Confidence level of the interval. If \code{distribution} is
#'   \code{"weibull"} this must be one of \code{0.9}, \code{0.95} or \code{0.99}.
#'
#' @return Returns a list with the classes \code{"rank_regression"} and
#'   \code{"model_estimation"} containing the following elements:
#'   \itemize{
#'     \item \code{coefficients} : If \code{distribution} is \code{"weibull"}, the
#'       estimated scale (\eqn{\eta}) and shape (\eqn{\beta}) parameters are provided
#'       (and additionally the threshold parameter \eqn{\gamma} if \code{distribution}
#'       is \code{"weibull3"}). For any other distribution, \code{coefficients} is
#'       identical to \code{loc_sc_params}.
#'     \item \code{confint} : Only included if \code{distribution} is \code{"weibull"}
#'       or \code{"weibull3"}. Approximated confidence intervals for \eqn{\eta} and
#'       \eqn{\beta} (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'     \item \code{loc_sc_params} : Estimated (log-)location-scale parameters.
#'       Threshold parameter is included if \code{distribution} is \code{"weibull3"},
#'       \code{"lognormal3"} or \code{"loglogistic3"}.
#'     \item \code{loc_sc_confint} : Confidence intervals for (log-)location-scale
#'       parameters. If distribution is \code{"lognormal3"} or \code{"loglogistic3"}
#'       a confidence interval for the threshold parameter is not computed.
#'     \item \code{loc_sc_varcov} : Provided, if \code{distribution} is not
#'       \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'       variance-covariance matrix for the (log-)location-scale parameters.
#'     \item \code{r_squared} : Coefficient of determination.
#'     \item \code{data} : A tibble with class \code{"cdf_estimation"} returned by
#'       \code{\link{estimate_cdf}}.
#'     \item \code{distribution} : Specified distribution.
#'   }
#'   If more than one method was specified in \code{\link{estimate_cdf}}, the
#'   resulting output is a list with class \code{"model_estimation_list"}. In
#'   this case each list element has classes \code{"rank_regression"} and
#'   \code{"model_estimation"} and the items listed above, are included.
#'
#' @encoding UTF-8
#'
#' @references
#'   \itemize{
#'     \item Mock, R., Methoden zur Datenhandhabung in
#'   Zuverlässigkeitsanalysen, vdf Hochschulverlag AG an der ETH Zürich, 1995
#'     \item Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998}
#'
#' @seealso \code{\link{rank_regression.default}}
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
#' # Probability estimation:
#' prob_tbl_2p <- estimate_cdf(
#'   x = data_2p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_3p <- estimate_cdf(
#'   x = data_3p,
#'   methods = "johnson"
#' )
#'
#' prob_tbl_mult <- estimate_cdf(
#'   x = data_3p,
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
#' # Example 3 - Fitting a three-parametric loglogistic distribution if multiple
#' # methods in estimate_cdf were specified:
#' rr_lists <- rank_regression(
#'   x = prob_tbl_mult,
#'   distribution = "loglogistic3",
#'   conf_level = 0.90
#' )
#'
#' @export
rank_regression <- function(x, ...) {
  UseMethod("rank_regression", x)
}



#' Rank Regression for Parametric Lifetime Distributions
#'
#' @inherit rank_regression description details references
#'
#' @inheritParams rank_regression
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in x.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is
#'   a right censored observation (= 0) or a failure (= 1).
#'
#' @return Returns a list with the classes \code{"rank_regression"} and
#'   \code{"model_estimation"} containing the following elements:
#'   \itemize{
#'     \item \code{coefficients} : If \code{distribution} is \code{"weibull"}, the
#'       estimated scale (\eqn{\eta}) and shape (\eqn{\beta}) parameters are provided
#'       (and additionally the threshold parameter \eqn{\gamma} if \code{distribution}
#'       is \code{"weibull3"}). For any other distribution, \code{coefficients} is
#'       identical to \code{loc_sc_params}.
#'     \item \code{confint} : Only included if \code{distribution} is \code{"weibull"}
#'       or \code{"weibull3"}. Approximated confidence intervals for \eqn{\eta} and
#'       \eqn{\beta} (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'     \item \code{loc_sc_params} : Estimated (log-)location-scale parameters.
#'       Threshold parameter is included if \code{distribution} is \code{"weibull3"},
#'       \code{"lognormal3"} or \code{"loglogistic3"}.
#'     \item \code{loc_sc_confint} : Confidence intervals for (log-)location-scale
#'       parameters. If distribution is \code{"lognormal3"} or \code{"loglogistic3"}
#'       a confidence interval for the threshold parameter is not computed.
#'     \item \code{loc_sc_varcov} : Provided, if \code{distribution} is not
#'       \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'       variance-covariance matrix for the (log-)location-scale parameters.
#'     \item \code{r_squared} : Coefficient of determination.
#'     \item \code{data} : A tibble with columns \code{x}, \code{status} and \code{prob}.
#'     \item \code{distribution} : Specified distribution.
#'   }
#'
#' @seealso \code{\link{rank_regression}}
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
#'   methods = "johnson"
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
#'   methods = "kaplan"
#' )
#'
#' rr_2 <- rank_regression(
#'   x = tbl_kaplan$x,
#'   y = tbl_kaplan$prob,
#'   status = tbl_kaplan$status,
#'   distribution = "lognormal3"
#' )
#'
#' @export
rank_regression.default <- function(x,
                                    y,
                                    status,
                                    distribution = c("weibull", "lognormal",
                                                     "loglogistic", "normal",
                                                     "logistic", "sev",
                                                     "weibull3", "lognormal3",
                                                     "loglogistic3"),
                                    conf_level = 0.95,
                                    ...
) {

  distribution <- match.arg(distribution)

  cdf_estimation <- tibble::tibble(x = x, status = status, prob = y)

  rank_regression_(cdf_estimation, distribution, conf_level)
}



#' @rdname rank_regression
#'
#' @export
rank_regression.cdf_estimation <- function(x,
                                           distribution = c("weibull", "lognormal",
                                                            "loglogistic", "normal",
                                                            "logistic", "sev",
                                                            "weibull3", "lognormal3",
                                                            "loglogistic3"),
                                           conf_level = 0.95,
                                           ...
) {

  distribution <- match.arg(distribution)

  if (length(unique(x$method)) == 1) {
    rank_regression_(
      cdf_estimation = x,
      distribution = distribution,
      conf_level = conf_level
    )
  } else {
    # Apply rank_regression to each method separately
    x_split <- split(x, x$method)

    model_estimation_list <- purrr::map(x_split, function(cdf_estimation) {
      rank_regression_(
        cdf_estimation = cdf_estimation,
        distribution = distribution,
        conf_level = conf_level
      )
    })

    class(model_estimation_list) <- c(
      "model_estimation_list", class(model_estimation_list)
    )

    model_estimation_list
  }
}



rank_regression_ <- function(cdf_estimation,
                             distribution,
                             conf_level
) {

  # In terms of MRR only failed items can be used:
  cdf_failed <- cdf_estimation %>% dplyr::filter(status == 1)

  x_f <- cdf_failed$x
  y_f <- cdf_failed$prob

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
      stop("'conf_level' must be 0.90, 0.95 or 0.99")
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

    mrr_output <- list(
      coefficients = estimates,
      confint = conf_ints,
      loc_sc_params = estimates_loc_sc,
      loc_sc_confint = conf_ints_loc_sc,
      r_squared = r_sq
    )
  }
  if (distribution %in% c("weibull3", "lognormal3", "loglogistic3")) {
    # Log-Location-Scale with threshold:
    ## Optimization of profile function:
    optim_gamma <- stats::optim(
      par = 0,
      fn = r_squared_profiling,
      method = "L-BFGS-B",
      upper = (1 - (1 / 1e+5)) * min(x_f),
      lower = 0,
      control = list(fnscale = -1),
      x = x_f,
      y = y_f,
      distribution = distribution
    )

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
        stop("'conf_level' must be 0.90, 0.95 or 0.99")
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

      mrr_output <- list(
        coefficients = estimates,
        confint = conf_ints,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        r_squared = r_sq
      )

    } else if (distribution %in% c("lognormal3", "loglogistic3")) {

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

      mrr_output <- list(
        coefficients = estimates_loc_sc,
        loc_sc_params = estimates_loc_sc,
        loc_sc_confint = conf_ints_loc_sc,
        loc_sc_varcov = vcov_loc_sc,
        r_squared = r_sq
      )
    }
  }

  if (distribution %in% c("lognormal", "loglogistic", "normal", "logistic", "sev")) {

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

    mrr_output <- list(
      coefficients = estimates_loc_sc,
      loc_sc_params = estimates_loc_sc,
      loc_sc_confint = conf_ints_loc_sc,
      loc_sc_varcov = vcov_loc_sc,
      r_squared = r_sq
    )
  }

  mrr_output$data <- cdf_estimation

  mrr_output$distribution <- distribution

  class(mrr_output) <- c("rank_regression", "model_estimation", class(mrr_output))

  return(mrr_output)
}



#' @export
print.rank_regression <- function(x,
                                  digits = max(3L, getOption("digits") - 3L),
                                  ...
) {
  cat("Rank Regression\n")
  NextMethod("print")
}



#' R-Squared-Profile Function for Log-Location-Scale Distributions with Threshold
#'
#' @description
#' This function evaluates the coefficient of determination with respect to a
#' given threshold parameter of a three-parametric lifetime distribution.
#' In terms of \emph{Rank Regression} this function can be optimized
#' (\code{\link{optim}}) to estimate the threshold parameter.
#'
#' @param x An object of class \code{cdf_estimation} returned from
#'   \code{\link{estimate_cdf}}.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param thres A numeric value for the threshold parameter.
#' @param distribution Supposed three-parametric distribution of the random variable.
#'
#' @return
#' Returns the coefficient of determination with respect to the threshold parameter
#' \code{thres}.
#'
#' @encoding UTF-8
#'
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @seealso \code{\link{r_squared_profiling.default}}
#'
#' @examples
#' # Data:
#' data <- reliability_data(
#'   data = alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   x = data,
#'   methods = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(
#'     dplyr::pull(
#'       dplyr::filter(
#'         prob_tbl,
#'         status == 1,
#'         x == min(x)
#'       ),
#'       x
#'     ) - 0.1
#'   ),
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = dplyr::filter(
#'     prob_tbl,
#'     status == 1
#'   ),
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @export
r_squared_profiling <- function(x,
                                ...
) {
  UseMethod("r_squared_profiling")
}



#' R-Squared-Profile Function for Log-Location-Scale Distributions with Threshold
#'
#' @inherit r_squared_profiling description details return references
#'
#' @inheritParams r_squared_profiling
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in x.
#'
#' @seealso \code{\link{r_squared_profiling}}
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   methods = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(cycles[status == 1]) - 0.1,
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = prob_tbl$x[prob_tbl$status == 1],
#'   y = prob_tbl$prob[prob_tbl$status == 1],
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the
#' ## coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @export
r_squared_profiling.default <- function(x,
                                        y,
                                        thres,
                                        distribution = c("weibull3",
                                                         "lognormal3",
                                                         "loglogistic3"),
                                        ...
) {

  if (any(is.na(y))) {
    stop("At least one of the failure probabilities ('y') is NA!")
  }

  distribution <- match.arg(distribution)

  r_sq_prof_vectorized <- Vectorize(
    FUN = r_squared_profiling_,
    vectorize.args = "thres"
  )

  r_sq_prof_vectorized(
    x = x,
    y = y,
    thres = thres,
    distribution = distribution
  )
}



#' @rdname r_squared_profiling
#'
#' @export
r_squared_profiling.cdf_estimation <- function(x,
                                               thres,
                                               distribution = c("weibull3",
                                                                "lognormal3",
                                                                "loglogistic3"),
                                               ...
) {
  distribution <- match.arg(distribution)

  r_squared_profiling.default(
    x = x$x,
    y = x$prob,
    thres = thres,
    distribution = distribution
  )
}



r_squared_profiling_ <- function(x,
                                 y,
                                 thres,
                                 distribution = c("weibull3",
                                                  "lognormal3",
                                                  "loglogistic3")

) {

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
