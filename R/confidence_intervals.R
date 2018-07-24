#' Estimation of Quantiles for Parametric Lifetime Distributions
#'
#' This function estimates the quantiles for a given set of estimated
#' location-scale parameters and specified failure probabilities.
#'
#' @param p a numeric vector which consists of failure probabilities
#'   regarding the lifetime data.
#' @param loc_sc_params a (named) numeric vector of estimated location
#'   and scale parameters for a specified distribution. The order of
#'   elements is important. First entry needs to be the location
#'   parameter \eqn{\mu} and the second element needs to be the scale
#'   parameter \eqn{\sigma}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#'
#' @return A vector containing the estimated quantiles for a given set of
#'   failure probabilities and estimated parameters.
#'
#' @export
#'
#' @examples
#' quants <- predict_quantile(p = c(0.01, 0.1, 0.5), loc_sc_params = c(5, 0.5),
#'                            distribution = "weibull")

predict_quantile <- function(p, loc_sc_params,
  distribution = c("weibull", "lognormal", "loglogistic")) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  if (distribution == "weibull") {
    quantiles <- SPREDA::qsev(p)
  }
  if (distribution == "lognormal") {
    quantiles <- stats::qnorm(p)
  }
  if (distribution == "loglogistic") {
    quantiles <- stats::qlogis(p)
  }

  x_pred <- exp(quantiles * loc_sc_params[[2]] + loc_sc_params[[1]])
  return(x_pred)
}


#' Estimation of Failure Probabilities for Parametric Lifetime Distributions
#'
#' This function estimates the failure probabilities for a given set of
#' estimated location-scale parameters and specified quantiles.
#'
#' @param q a numeric vector which consists of lifetime data.
#' @param loc_sc_params a (named) numeric vector of estimated location
#'   and scale parameters for a specified distribution. The order of
#'   elements is important. First entry needs to be the location
#'   parameter \eqn{\mu} and the second element needs to be the scale
#'   parameter \eqn{\sigma}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#'
#' @return A vector containing the estimated failure probabilities for a given
#'   set of quantiles and estimated parameters.
#' @export
#'
#' @examples
#' probs <- predict_prob(q = c(15, 48, 124), loc_sc_params = c(5, 0.5),
#'                            distribution = "weibull")

predict_prob <- function(q, loc_sc_params,
  distribution = c("weibull", "lognormal", "loglogistic")) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  # Standardize
  z <- (log(q) - loc_sc_params[[1]]) / loc_sc_params[[2]]

  if (distribution == "weibull") {
    cd <- SPREDA::psev(z)
  }
  if (distribution == "lognormal") {
    cd <- stats::pnorm(z)
  }
  if (distribution == "loglogistic") {
    cd <- stats::plogis(z)
  }

  y_pred <- cd
  return(y_pred)
}


#' Beta Binomial Confidence Bounds for Quantiles and/or Probabilities
#'
#' This non-parametric approach calculates confidence bounds for quantiles and/or
#' failure probabilities using a procedure that is similar to that used in
#' calculating median ranks. The location-scale parameters estimated by rank
#' regression are needed.
#'
#' @param x a numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the range of confidence region(s).
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param loc_sc_params a (named) numeric vector of estimated location and scale
#'   parameters for a specified distribution. The order of elements is
#'   important. First entry needs to be the location parameter \eqn{\mu} and the
#'   second element needs to be the scale parameter \eqn{\sigma}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param bounds a character string specifying the interval(s) which has/have to
#'   be computed. Must be one of "two_sided" (default), "lower" or "upper".
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param direction a character string specifying the direction of the computed
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#'
#' @return A data frame containing the lifetime characteristic, interpolated
#'   ranks as a function of probabilities, the probabilities which are used to
#'   compute the ranks and computed values for the specified confidence bound(s).
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
#'                        conf_level = .95)
#' conf_betabin <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_coefficients,
#'                                   distribution = "weibull",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")

confint_betabinom <- function(x, event, loc_sc_params,
                              distribution = c("weibull", "lognormal", "loglogistic"),
                              bounds = c("two_sided", "lower", "upper"),
                              conf_level = .95, direction = c("y", "x")) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  n <- length(x)
  x_ob <- x[event == 1]

  # Range of failed items
  x_min <- min(x_ob, na.rm = TRUE)
  x_max <- max(x_ob, na.rm = TRUE)
  x_seq <- seq(x_min, x_max, length.out = 100)

  # Range of probabilities calculated with estimated regression line
  y_seq <- predict_prob(q = x_seq, loc_sc_params = loc_sc_params,
                        distribution = distribution)

  # Probabilities of special interest (B-Lives)
  b_perc <- c(0.01, 0.1, 0.50)
  # Looking for these probabilities in range of estimated ones
  int_ind <- findInterval(x = b_perc, vec = c(y_seq[1], y_seq[length(y_seq)]),
                          rightmost.closed = TRUE)

  # Add them
  y_seq <- unique(c(y_seq, b_perc[which(int_ind == 1)]))
  y_seq <- y_seq[order(y_seq)]

  # Calculating and adding B-Lives to x-vector
  x_b <- predict_quantile(p = b_perc[which(int_ind == 1)],
                          loc_sc_params = loc_sc_params,
                          distribution = distribution)
  x_seq <- unique(c(x_seq, x_b))
  x_seq <- x_seq[order(x_seq)]

  # Caluclating virtual ranks, i.e. interpolating between realisations
  virt_rank <- y_seq * (n + 0.4) + 0.3


  if (bounds == "two_sided") {
    conf_up <- stats::qbeta((1 + conf_level) / 2, virt_rank, n - virt_rank + 1)
    conf_low <- stats::qbeta((1 - conf_level) / 2, virt_rank, n - virt_rank + 1)
    list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)
  } else if (bounds == "lower") {
    conf_low <- stats::qbeta(1 - conf_level, virt_rank, n - virt_rank + 1)
    list_confint <- list(lower_bound = conf_low)
  } else {
    conf_up <- stats::qbeta(conf_level, virt_rank, n - virt_rank + 1)
    list_confint <- list(upper_bound = conf_up)
  }

  if (direction == "y") {
    list_output <- c(list(characteristic = x_seq, rank = virt_rank, prob = y_seq),
                     list_confint)
    df_output <- as.data.frame(list_output)
  } else {
    x_confint <- lapply(list_confint, predict_quantile,
                        loc_sc_params = loc_sc_params,
                        distribution = distribution)
    list_output <- c(list(characteristic = x_seq, rank = virt_rank, prob = y_seq),
                     x_confint)

    df_output <- as.data.frame(list_output)
  }
  return(df_output)
}


#' Delta Method for Parametric Lifetime Distributions
#'
#' The delta method estimates the standard error for quantities that can be
#' written as non-linear functions of ML estimators like failure probabilities or
#' quantiles. I.e. the location-scale parameters and variance-covariance matrix
#' of these need to be estimated by Maximum Likelihood.
#'
#' @param p a numeric value of a probability or a quantile. If the standard error
#'   of probability is of interest a specific quantile needs to be supplied
#'   and vice versa.
#' @param loc_sc_params a (named) numeric vector of estimated
#'   (by Maximum Likelihood) location and scale parameters for a specified
#'   distribution. The order of elements is important. First entry needs to be
#'   the location parameter \eqn{\mu} and the second element needs to be the
#'   scale parameter \eqn{\sigma}.
#' @param loc_sc_varcov  a (named) numeric matrix of estimated
#'   (by Maximum Likelihood) location and scale variances and covariances for a
#'   specified distribution. The order of elements is important. First entry
#'   of the diagonal needs to be the variance of the location parameter
#'   Var(\eqn{\mu}) and the second element of the diagonal needs to be the
#'   variance of the scale parameter Var(\eqn{\sigma}).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param direction a character string specifying the direction of the computed
#'   standard errors. Must be either "y" (failure probability) or "x"
#'   (quantile). If \code{p} is a quantile then \emph{direction} needs to be "y"
#'   and vice versa.
#'
#' @return A numeric value with estimated standard error of failure probability
#'   or quantiles.
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#'
#' mle <- ml_estimation(x = obs, event = state,
#'                      distribution = "weibull", conf_level = 0.95)
#' delta_prob <- sapply(obs, delta_method,
#'                           loc_sc_params = mle$loc_sc_coefficients,
#'                           loc_sc_varcov = mle$loc_sc_vcov,
#'                           distribution = "weibull",
#'                           direction = "y")

delta_method <- function(p, loc_sc_params, loc_sc_varcov,
                         distribution = c("weibull", "lognormal", "loglogistic"),
                         direction = c("y", "x")) {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  if (direction == "x") {
    if (distribution == "weibull") {
      z <- SPREDA::qsev(p)
    }
    if (distribution == "lognormal") {
      z <- stats::qnorm(p)
    }
    if (distribution == "loglogistic") {
      z <- stats::qlogis(p)
    }

    q <- predict_quantile(p = p, loc_sc_params = loc_sc_params,
                          distribution = distribution)
    dq_dmu <- q
    dq_dsc <- z * q
    dq_dpar <- c(dq_dmu, dq_dsc)

    var_q <- t(dq_dpar) %*% loc_sc_varcov %*% dq_dpar
    std_err <- sqrt(var_q)
  } else {
    z <- (log(p) - loc_sc_params[[1]]) / loc_sc_params[[2]]
    if (distribution == "weibull") {
      pd <- SPREDA::dsev(z)
    }
    if (distribution == "lognormal") {
      pd <- stats::dnorm(z)
    }
    if (distribution == "loglogistic") {
      pd <- stats::dlogis(z)
    }

    dps_dmu <- (-1 / loc_sc_params[[2]]) * pd
    dps_dsc <- (-1 / loc_sc_params[[2]]) * z * pd
    dps_dpar <- c(dps_dmu, dps_dsc)

    var_ps <- t(dps_dpar) %*% loc_sc_varcov %*% dps_dpar
    std_err <- sqrt(var_ps)
  }
  return(std_err)
}


#' Fisher Confidence Bounds for Quantiles and/or Probabilities
#'
#' This method computes normal-approximation confidence intervals for quantiles
#' and/or failure probabilities using the \code{\link{delta_method}}. The
#' required location-scale parameters and variance-covariance matrix need to be
#' estimated by Maximum Likelihood.
#'
#' @param x a numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the range of confidence region(s).
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param loc_sc_params a (named) numeric vector of estimated
#'   (by Maximum Likelihood) location and scale parameters for a specified
#'   distribution. The order of elements is important. First entry needs to be
#'   the location parameter \eqn{\mu} and the second element needs to be the
#'   scale parameter \eqn{\sigma}.
#' @param loc_sc_varcov  a (named) numeric matrix of estimated
#'   (by Maximum Likelihood) location and scale variances and covariances for a
#'   specified distribution. The order of elements is important. First entry
#'   of the diagonal needs to be the variance of the location parameter
#'   Var(\eqn{\mu}) and the second element of the diagonal needs to be the
#'   variance of the scale parameter Var(\eqn{\sigma}).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"}.
#' @param bounds a character string specifying the interval(s) which has/have to
#'   be computed. Must be one of "two_sided" (default), "lower" or "upper".
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#' @param direction a character string specifying the direction of the computed
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#'
#' @return A data frame containing the lifetime characteristic, the
#'   probabilities, estimated standard errors by the delta method and computed
#'   values for the specified confidence bound(s).
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' df_john <- johnson_method(x = obs, event = state)
#' mle <- ml_estimation(x = obs, event = state,
#'                      distribution = "weibull", conf_level = 0.95)
#' conf_fish <- confint_fisher(x = df_john$characteristic,
#'                             event = df_john$status,
#'                             loc_sc_params = mle$loc_sc_coefficients,
#'                             loc_sc_varcov = mle$loc_sc_vcov,
#'                             distribution = "weibull",
#'                             bounds = "two_sided",
#'                             conf_level = 0.95,
#'                             direction = "y")

confint_fisher <- function(x, event, loc_sc_params, loc_sc_varcov,
                           distribution = c("weibull", "lognormal", "loglogistic"),
                           bounds = c("two_sided", "lower", "upper"),
                           conf_level = .95, direction = c("y", "x")) {

  bounds <- match.arg(bounds)
  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  n <- length(x)
  x_ob <- x[event == 1]

  x_min <- min(x_ob, na.rm = TRUE)
  x_max <- max(x_ob, na.rm = TRUE)
  x_seq <- seq(x_min, x_max, length.out = 100)

  y_seq <- predict_prob(q = x_seq, loc_sc_params = loc_sc_params,
    distribution = distribution)

  b_perc <- c(0.01, 0.1, 0.50)
  int_ind <- findInterval(x = b_perc, vec = c(y_seq[1], y_seq[length(y_seq)]),
    rightmost.closed = TRUE)

  y_seq <- unique(c(y_seq, b_perc[which(int_ind == 1)]))
  y_seq <- y_seq[order(y_seq)]

  x <- predict_quantile(p = y_seq, loc_sc_params = loc_sc_params,
    distribution = distribution)

  if (direction == "x") {
    se_delta <- sapply(y_seq, delta_method, loc_sc_params = loc_sc_params,
                       loc_sc_varcov = loc_sc_varcov,
                       distribution = distribution, direction = direction)

    if (bounds == "two_sided") {
      w <- exp((stats::qnorm((1 + conf_level) / 2) * se_delta) / x)
      conf_up <- x * w
      conf_low <- x / w
      list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)
    } else if (bounds == "lower") {
      w <- exp((stats::qnorm(conf_level) * se_delta) / x)
      conf_low <- x / w
      list_confint <- list(lower_bound = conf_low)
    } else {
      w <- exp((stats::qnorm(conf_level) * se_delta) / x)
      conf_up <- x * w
      list_confint <- list(upper_bound = conf_up)
    }
    list_output <- c(list(characteristic = x, prob = y_seq, std_err = se_delta),
                     list_confint)
    df_output <- as.data.frame(list_output)
  } else {
    prob <- predict_prob(q = x, loc_sc_params = loc_sc_params,
                         distribution = distribution)
    se_delta <- sapply(x, delta_method, loc_sc_params = loc_sc_params,
      loc_sc_varcov = loc_sc_varcov,
      distribution = distribution, direction = direction)

    if (bounds == "two_sided") {
      w <- exp((stats::qnorm((1 + conf_level) / 2) * se_delta) / (prob * (1 - prob)))
      conf_up <- prob / (prob + (1 - prob) / w)
      conf_low <- prob / (prob + (1 - prob) * w)
      list_confint <- list(lower_bound = conf_low, upper_bound = conf_up)
    } else if (bounds == "lower") {
      w <- exp((stats::qnorm(conf_level) * se_delta) / (prob * (1 - prob)))
      conf_low <- prob / (prob + (1 - prob) * w)
      list_confint <- list(lower_bound = conf_low)
    } else {
      w <- exp((stats::qnorm(conf_level) * se_delta) / (prob * (1 - prob)))
      conf_up <- prob / (prob + (1 - prob) / w)
      list_confint <- list(upper_bound = conf_up)
    }
    list_output <- c(list(characteristic = x, prob = y_seq, std_err = se_delta),
      list_confint)
    df_output <- as.data.frame(list_output)
  }
}
