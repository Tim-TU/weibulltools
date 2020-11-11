#' Mixture Model Identification using Segmented Regression
#'
#' This method uses piecewise linear regression to separate the data in
#' subgroups, if appropriate. Since this happens in an automated fashion
#' the function tends to overestimate the number of breakpoints and
#' therefore returns too many subgroups. This problem is already stated in
#' the documentation of the function \link{segmented.lm}, which is part of
#' the \emph{segmented} package. A maximum of three subgroups can be obtained.
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a
#'   product, e.g. operating time (days/months in service), mileage (km,
#'   miles), load cycles.
#' @param y A numeric vector which consists of estimated failure
#'   probabilities regarding the lifetime data in \code{x}.
#' @param event A vector of binary data (0 or 1) indicating whether
#'   unit \emph{i} is a right censored observation (= 0) or a
#'   failure (= 1).
#' @param distribution Supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param conf_level Confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}.
#'
#' @return Returns a list where the length of the list depends on
#'   the number of identified subgroups. Each list has the same
#'   information as provided by \link{rank_regression}. Additionally each list
#'   has an element that specifies the range regarding the lifetime data for
#'   every subgroup.
#' @export
#'
#' @examples
#' # Data is taken from given reference:
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' john <- johnson_method(x = hours, event = state)
#'
#' mix_mod <- mixmod_regression(x = john$characteristic,
#'                              y = john$prob,
#'                              event = john$status,
#'                              distribution = "weibull")
#'
mixmod_regression <- function(x, y, event,
                              distribution = c("weibull", "lognormal", "loglogistic"),
                              conf_level = .95) {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  # Preparing for segmented regression
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  if (distribution == "weibull") {
    mrr <- stats::lm(log(x_f) ~ SPREDA::qsev(y_f))
  }
  if (distribution == "lognormal") {
    mrr <- stats::lm(log(x_f) ~ qnorm(y_f))
  }
  if (distribution == "loglogistic") {
    mrr <- stats::lm(log(x_f) ~ qlogis(y_f))
  }

  # segmented regression
  seg_mrr <- try(segmented::segmented.lm(mrr,
                   control = segmented::seg.control(it.max = 20, n.boot = 20)),
                   silent = TRUE)
  mrr_0 <- rank_regression(x = x, y = y, event = event,
                           distribution = distribution,
                           conf_level = conf_level)
  r_sq0 <- mrr_0$r_squared
  mrr_0$x_range <- range(x)

  mrr_output <- mrr_0

  # test for successful segmentation
  if ("try-error" %in% class(seg_mrr)) {
    message("An admissible breakpoint could not be found!
            Simple linear regression model was estimated!")
  } else if (length(x_f[seg_mrr$id.group != 0]) < 5) {
        message("Second segment contains less than 5 elements.
                Simple linear regression model was estimated!")
  } else {
    groups <- seg_mrr$id.group
    x_1 <- x_f[groups == 0]
    y_1 <- y_f[groups == 0]
    mrr_1 <- rank_regression(x = x_1, y = y_1,
                             event = rep(1, length(x_1)),
                             distribution = distribution,
                             conf_level = conf_level)

    r_sq1 <- mrr_1$r_squared
    mrr_1$x_range <- range(x_1)

    x_rest <- x_f[groups != 0]
    y_rest <- y_f[groups != 0]

    mrr_23 <- rank_regression(x = x_rest, y = y_rest,
                              event = rep(1, length(x_rest)),
                              distribution = distribution,
                              conf_level = conf_level)

    r_sq23 <- mrr_23$r_squared
    mrr_23$x_range <- range(x_rest)

    if (distribution == "weibull") {
      mrr2 <- stats::lm(log(x_rest) ~ SPREDA::qsev(y_rest))
    }
    if (distribution == "lognormal") {
      mrr2 <- stats::lm(log(x_rest) ~ qnorm(y_rest))
    }
    if (distribution == "loglogistic") {
      mrr2 <- stats::lm(log(x_rest) ~ qlogis(y_rest))
    }

    seg_mrr2 <- try(segmented::segmented.lm(mrr2,
                    control = segmented::seg.control(it.max = 20, n.boot = 20)),
                    silent = TRUE)

    if ("try-error" %in% class(seg_mrr2) || length(x_rest[seg_mrr2$id.group != 0]) < 5) {

      if (mean(c(r_sq1, r_sq23)) < r_sq0) {
        print("Simple linear regression model was estimated!")
          mrr_output <- mrr_0
      } else {
        mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_23)
      }

      if (is.list(seg_mrr2)) {
        message("Third segment contains less than 5 elements.
                Simple linear regression model was estimated for all elements after first breakpoint!")
      }
    } else {
      groups2 <- seg_mrr2$id.group
        x_2 <- x_rest[groups2 == 0]
        y_2 <- y_rest[groups2 == 0]

        mrr_2 <- rank_regression(x = x_2, y = y_2, event = rep(1, length(x_2)),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq2 <- mrr_2$r_squared
        mrr_2$x_range <- range(x_2)

        mrr_12 <- rank_regression(x = c(x_1, x_2), y = c(y_1, y_2), event = rep(1, length(c(x_1, x_2))),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq12 <- mrr_12$r_squared
        mrr_12$x_range <- range(c(x_1, x_2))
        x_3 <- x_rest[groups2 == 1]
        y_3 <- y_rest[groups2 == 1]

        mrr_3 <- rank_regression(x = x_3, y = y_3, event = rep(1, length(x_3)),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq3 <- mrr_3$r_squared
        mrr_3$x_range <- range(x_3)

        mean_r_sq <- mean(c(r_sq1, r_sq2, r_sq3))

        if (mean_r_sq < r_sq0 | mean_r_sq < mean(c(r_sq1, r_sq23)) | mean_r_sq < mean(c(r_sq12, r_sq3))) {
          if (mean(c(r_sq1, r_sq23)) > r_sq0 && mean(c(r_sq1, r_sq23)) > mean(c(r_sq12, r_sq3))) {
            mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_23)
          } else if (mean(c(r_sq12, r_sq3)) > r_sq0) {
            mrr_output <- list(mod_1 = mrr_12, mod_2 = mrr_3)
          } else {
            mrr_output <- mrr_0
          }
        } else {
          mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_2, mod_3 = mrr_3)
          message("Problem of overestimation may have occured.
                   Further investigations are recommended!")
        }
      }
    }
  return(mrr_output)
}

#' Mixture Model Estimation using EM-Algorithm
#'
#' This method uses the EM-Algorithm to estimate the parameters of a univariate
#' mixture model. Until now, the mixture model can consist of k two-parametric
#' Weibull distributions. If no mixture of k components can be estimated,
#' the function is forced to stop and a message with instructions is given.
#'
#' In \code{mixmod_em} the function \code{\link{mixture_em_cpp}} is called. The
#' computed posterior probabilities are then used as weights inside function
#' \code{\link{ml_estimation}} to model a weighted log-likelihood. This strategy
#' enables the computation of confidence intervals for the parameters of the
#' separated sub-distributions, since \code{ml_estimation} provides a variance-covariance
#' matrix. Using this strategy, a potential problem that can occur is,
#' that the value of the complete log-likelihood, computed by \code{mixture_em_cpp},
#' differs considerably from the complete log-likelihood after re-estimating
#' parameters with \code{ml_estimation}. If so, the estimated quantities like
#' prior and posterior probabilities, as well as the model parameters are not
#' reliable anymore and the function is forced to stop with the message:
#' "Parameter estimation was not successful!"
#' But if the log-likelihood values are close to each other, the presence of the
#' mixture is strengthened and a reasonable fit is provided.
#' Thus, a check of the absolute differences in the log-likelihood values is made
#' and the critical difference has to be specified in argument \code{diff_loglik}.
#'
#' @encoding UTF-8
#' @references
#'   \itemize{
#'     \item Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'       Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'     \item Blog posts by Stefan Gelissen: \url{http://blogs2.datall-analyse.nl/2016/02/18/rcode_mixture_distribution_censored};
#'       last access on 19th January 2019}
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'  data could be every characteristic influencing the reliability of a product,
#'  e.g. operating time (days/months in service), mileage (km, miles), load
#'  cycles.
#' @param event A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param post A numeric matrix specifiying initial a-posteriori probabilities.
#'   If post is \code{NULL} (default) a-posteriori probabilities are assigned
#'   randomly using the Dirichlet distribution (\code{\link{rdirichlet}} from
#'   \emph{LearnBayes} Package), which is the conjugate prior of a Multinomial
#'   distribution. This idea was taken from the blog post of Mr. Gelissen
#'   (linked under \emph{references}).
#' @param distribution Supposed mixture model. Only \code{"weibull"} can be used.
#'   Other distributions have not been implemented yet.
#' @param conf_level Confidence level for the confidence intervals of the parameters
#'   of every component \code{k}. The default value is \code{conf_level = 0.95}.
#' @param k Integer of mixture components, default is 2.
#' @param method Default method is \code{"EM"}. Other methods have not been implemented
#'   yet.
#' @param n_iter Integer defining the maximum number of iterations.
#' @param conv_limit Numeric value defining the convergence limit.
#' @param diff_loglik Numeric value defining the maximum difference between
#'   log-likelihood values, which seems permissible. The default value is \code{0.5}.
#'   See \strong{Details} for the usage of this argument.
#'
#' @return Returns a list where the length of the list depends on the number of
#'    k subgroups. The first \code{k} lists have the same information as provided by
#'    \link{ml_estimation}, but the values \code{logL}, \code{aic} and \code{bic} are
#'    the results of a log-likelihood function, which is weighted by a-posteriori
#'    probabilities. The last list summarizes further results of the EM-Algorithm and
#'    is therefore called \code{em_results}. It contains the following elements:
#'   \itemize{
#'   \item \code{a_priori} : A vector with estimated a-priori probabilities.
#'   \item \code{a_posteriori} : A matrix with estimated a-posteriori probabilities.
#'   \item \code{groups} : Numeric vector specifying the group membership of every
#'     observation.
#'   \item \code{logL} : The value of the complete log-likelihood.
#'   \item \code{aic} : Akaike Information Criterion.
#'   \item \code{bic} : Bayesian Information Criterion.}
#' @export
#' @examples
#' # Data is taken from given reference of Doganaksoy, Hahn and Meeker:
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'          1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'          1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'          0, 1, 1, 1, 1, 1, 1)
#'
#' mix_mod_em <- mixmod_em(x = hours,
#'                         event = state,
#'                         distribution = "weibull",
#'                         conf_level = 0.95,
#'                         k = 2,
#'                         method = "EM",
#'                         n_iter = 150)
#'
#'
mixmod_em <- function(x, event, post = NULL, distribution = "weibull",
                      conf_level = .95, k = 2, method = "EM", n_iter = 100L,
                      conv_limit = 1e-6, diff_loglik = 0.5) {

  distribution <- match.arg(distribution)
  method <- match.arg(method)

  if (!(distribution %in% "weibull")) {
    stop("No valid distribution!")
  }

  # Providing initial random a-posteriors (see references, blog post Mr. Gelissen):
  if (is.null(post)) {
    post <- LearnBayes::rdirichlet(n = length(x), par = rep(0.1, k))
  }

  # mixture_em_cpp() for applying EM-Algorithm:
  mix_est <- mixture_em_cpp(x = x,
                            event = event,
                            post = post,
                            distribution = distribution,
                            k = k,
                            method = method,
                            n_iter = n_iter,
                            conv_limit = conv_limit)

  ############## New Approach ##############
  # Try to apply ml_estimation where observations are weighted with a-posterioris:
  ml <- try(apply(mix_est$posteriori, MARGIN = 2, FUN = ml_estimation, x = x, event = event,
    distribution = distribution, conf_level = conf_level), silent = TRUE)
  if (class(ml) == "try-error") {
        stop(paste(ml[1], sprintf("\n For k = %s subcomponents the above problem occured!", k),
                   paste("\n Hint: Reduce k in function call and try again. If not",
                         "succeed a mixture model seems not to be appropriate. \n Instead use k = 1 to perform ml_estimation().")))
  }

  # calculate complete log-likelihood and information criteria for EM.
  logL_comps <- sapply(ml, "[[", "logL")
  logL_complete <- sum(logL_comps) + sum(mix_est$posteriori %*% log(mix_est$priori))
  aic_complete <- -2 * logL_complete + 2 * (2 * k + (k - 1))
  bic_complete <- -2 * logL_complete + log(length(x)) * (2 * k + (k - 1))

  # Check whether log-likelihood from mixture_em_cpp() and complete log-likelihood
  # after recalculating parameters with ml_estimation() are close to each other.
  # If so, appearance of a mixture is strengthened and a good fit is reliable.
  # Otherwise, stop() function should be called, since posterioris and prioris are
  # not valid anymore!!!!

   if (abs(logL_complete - mix_est$logL) > diff_loglik) {
    stop("Parameter estimation was not successful!")
  }

  # separate observations using maximum a-posteriori method (MAP):
  split_obs <- apply(mix_est$posteriori, 1, which.max)

  names(ml) <- sprintf("mod_%i", 1:k)

  ml$em_results <- list(a_priori = mix_est$priori, a_posteriori = mix_est$posteriori,
    groups = split_obs, logL = logL_complete, aic = aic_complete, bic = bic_complete)

  ml
}
