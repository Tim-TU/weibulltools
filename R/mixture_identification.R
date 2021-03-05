#' Mixture Model Identification using Segmented Regression
#'
#' @description
#' This function uses piecewise linear regression to divide the data into
#' subgroups. See 'Details'.
#'
#' @details
#' The segmentation process is based on the lifetime realizations of failed
#' units and their corresponding estimated failure probabilities for which intact
#' items are taken into account. It is performed with the support of
#' [segmented.lm][segmented::segmented].
#'
#' Segmentation can be done with a specified number of subgroups or in an automated
#' fashion (see argument `k`). The algorithm tends to overestimate the number of
#' breakpoints when the separation is done automatically (see 'Warning' in
#' [segmented.lm][segmented::segmented]).
#'
#' In the context of reliability analysis it is important that the main types of
#' failures can be identified and analyzed separately. These are
#'
#' * early failures,
#' * random failures and
#' * wear-out failures.
#'
#' In order to reduce the risk of overestimation as well as being able to consider
#' the main types of failures, a maximum of three subgroups (`k = 3`) is recommended.
#'
#' @inheritParams rank_regression.wt_cdf_estimation
#' @param k Number of mixture components. If the data should be split in an
#' automated fashion, `k` must be set to `NULL`. The argument `fix.psi` of
#' `control` is then set to `FALSE`.
#' @param control Output of the call to [seg.control][segmented::seg.control],
#' which is passed to [segmented.lm][segmented::segmented]. See 'Examples' for usage.
#'
#' @return A list with classes `wt_model` and `wt_rank_regression` if no breakpoint
#' was detected. See [rank_regression].
#'
#' A list with classes `wt_model` and `wt_mixmod_regression` if at least one
#' breakpoint was determined. The length of the list depends on the number of
#' identified subgroups. Each list element contains the information provided by
#' [rank_regression]. In addition, the returned tibble `data` of each list element
#' only retains information on the failed units and has two more columns:
#'
#' * `q` : Quantiles of the standard distribution calculated from column `prob`.
#' * `group` : Membership to the respective segment.
#'
#' If more than one method was specified in [estimate_cdf], the resulting output
#' is a list with classes `wt_model` and `wt_mixmod_regression_list` where each
#' list element has classes `wt_model` and `wt_mixmod_regression`.
#'
#' @encoding UTF-8
#'
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @examples
#' # Reliability data preparation:
#' ## Data for mixture model:
#' data_mix <- reliability_data(
#'   voltage,
#'   x = hours,
#'   status = status
#' )
#'
#' ## Data for simple unimodal distribution:
#' data <- reliability_data(
#'   shock,
#'   x = distance,
#'   status = status
#' )
#'
#' # Probability estimation with one method:
#' prob_mix <- estimate_cdf(
#'   data_mix,
#'   methods = "johnson"
#' )
#'
#' prob <- estimate_cdf(
#'   data,
#'   methods = "johnson"
#' )
#'
#' # Probability estimation for multiple methods:
#' prob_mix_mult <- estimate_cdf(
#'   data_mix,
#'   methods = c("johnson", "kaplan", "nelson")
#' )
#'
#' # Example 1 - Mixture identification using k = 2 two-parametric Weibull models:
#' mix_mod_weibull <- mixmod_regression(
#'   x = prob_mix,
#'   distribution = "weibull",
#'   conf_level = 0.99,
#'   k = 2
#' )
#'
#' # Example 2 - Mixture identification using k = 3 two-parametric lognormal models:
#' mix_mod_lognorm <- mixmod_regression(
#'   x = prob_mix,
#'   distribution = "lognormal",
#'   k = 3
#' )
#'
#' # Example 3 - Mixture identification for multiple methods specified in estimate_cdf:
#' mix_mod_mult <- mixmod_regression(
#'   x = prob_mix_mult,
#'   distribution = "loglogistic"
#' )
#'
#' # Example 4 - Mixture identification using control argument:
#' mix_mod_control <- mixmod_regression(
#'   x = prob_mix,
#'   distribution = "weibull",
#'   control = segmented::seg.control(display = TRUE)
#' )
#'
#' # Example 5 - Mixture identification performs rank_regression for k = 1:
#' mod <- mixmod_regression(
#'   x = prob,
#'   distribution = "weibull",
#'   k = 1
#' )
#'
#' @md
#'
#' @export
mixmod_regression <- function(x, ...) {
  UseMethod("mixmod_regression")
}



#' @rdname mixmod_regression
#'
#' @export
mixmod_regression.wt_cdf_estimation <- function(
                               x,
                               distribution = c(
                                 "weibull", "lognormal", "loglogistic"
                               ),
                               conf_level = .95,
                               k = 2,
                               control = segmented::seg.control(),
                               ...
) {

  distribution <- match.arg(distribution)

  x_split <- split(x, x$cdf_estimation_method)

  if (length(unique(x$cdf_estimation_method)) == 1) {
    out <- mixmod_regression_(
      cdf_estimation = x,
      distribution = distribution,
      conf_level = conf_level,
      k = k,
      control = control
    )
  } else {
    out <- purrr::map(x_split, function(cdf_estimation) {
      mixmod_regression_(
        cdf_estimation = cdf_estimation,
        distribution = distribution,
        conf_level = conf_level,
        k = k,
        control = control
      )
    })

    class(out) <- c("wt_model", "wt_mixmod_regression_list", class(out))
  }

  out
}



#' Mixture Model Identification using Segmented Regression
#'
#' @inherit mixmod_regression description details references
#'
#' @inheritParams rank_regression.default
#' @inheritParams mixmod_regression.wt_cdf_estimation
#' @param control Output of the call to [seg.control][segmented::seg.control],
#' which is passed to [segmented.lm][segmented::segmented]. See 'Examples' for usage.
#'
#' @return A list with classes `wt_model` and `wt_rank_regression` if no breakpoint
#' was detected. See [rank_regression]. The returned tibble `data` is of class
#' `wt_cdf_estimation` and contains the dummy columns `cdf_estimation_method` and
#' `id`. The former is filled with `NA_character`, due to internal usage and the
#' latter is filled with `"XXXXXX"` to point out that unit identification is not
#' possible when using the vector-based approach.
#'
#' A list with classes `wt_model` and `wt_mixmod_regression` if at least one
#' breakpoint was determined. The length of the list depends on the number of
#' identified subgroups. Each list contains the information provided by
#' [rank_regression]. The returned tibble `data` of each list element only retains
#' information on the failed units and has modified and additional columns:
#'
#' * `id` : Modified id, overwritten with `"XXXXXX"` to point out that unit
#'   identification is not possible when using the vector-based approach.
#' * `cdf_estimation_method` : A character that is always `NA_character`. Only
#'   needed for internal use.
#' * `q` : Quantiles of the standard distribution calculated from column `prob`.
#' * `group` : Membership to the respective segment.
#'
#' @encoding UTF-8
#'
#' @seealso [mixmod_regression]
#'
#' @examples
#' # Vectors:
#' ## Data for mixture model:
#' hours <- voltage$hours
#' status <- voltage$status
#'
#' ## Data for simple unimodal distribution:
#' distance <- shock$distance
#' status_2 <- shock$status
#'
#' # Probability estimation with one method:
#' prob_mix <- estimate_cdf(
#'   x = hours,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' prob <- estimate_cdf(
#'   x = distance,
#'   status = status_2,
#'   method = "johnson"
#' )
#'
#'  # Example 1 - Mixture identification using k = 2 two-parametric Weibull models:
#' mix_mod_weibull <- mixmod_regression(
#'    x = prob_mix$x,
#'    y = prob_mix$prob,
#'    status = prob_mix$status,
#'    distribution = "weibull",
#'    conf_level = 0.99,
#'    k = 2
#' )
#'
#' # Example 2 - Mixture identification using k = 3 two-parametric lognormal models:
#' mix_mod_lognorm <- mixmod_regression(
#'    x = prob_mix$x,
#'    y = prob_mix$prob,
#'    status = prob_mix$status,
#'    distribution = "lognormal",
#'    k = 3
#' )
#'
#' # Example 3 - Mixture identification using control argument:
#' mix_mod_control <- mixmod_regression(
#'   x = prob_mix$x,
#'   y = prob_mix$prob,
#'   status = prob_mix$status,
#'   distribution = "weibull",
#'   k = 2,
#'   control = segmented::seg.control(display = TRUE)
#' )
#'
#' # Example 4 - Mixture identification performs rank_regression for k = 1:
#' mod <- mixmod_regression(
#'   x = prob$x,
#'   y = prob$prob,
#'   status = prob$status,
#'   distribution = "weibull",
#'   k = 1
#' )
#'
#' @md
#'
#' @export
mixmod_regression.default <- function(x,
                                      y,
                                      status,
                                      distribution = c(
                                        "weibull", "lognormal", "loglogistic"
                                      ),
                                      conf_level = .95,
                                      k = 2,
                                      control = segmented::seg.control(),
                                      ...
) {

  distribution <- match.arg(distribution)

  # mimic output of estimate_cdf
  cdf <- tibble::tibble(
    id = "XXXXXX",
    x = x,
    status = status,
    prob = y,
    cdf_estimation_method = NA_character_
  )

  class(cdf) <- c("wt_cdf_estimation", class(cdf))

  mixmod_regression_(
    cdf_estimation = cdf,
    distribution = distribution,
    conf_level = conf_level,
    k = k,
    control = control
  )
}



mixmod_regression_ <- function(cdf_estimation,
                               distribution,
                               conf_level,
                               k,
                               control
) {

  if (!purrr::is_null(k) && k < 1) {
    stop("'k' must be greater or equal than 1!", call. = FALSE)
  }

  # Preparation for segmented regression:
  cdf_failed <- dplyr::filter(cdf_estimation, .data$status == 1)

  cdf_failed$q <- q_std(cdf_failed$prob, distribution)

  mrr <- stats::lm(log(x) ~ q, cdf_failed)

  if (!purrr::is_null(k) && k == 1) {
    mrr_output <- rank_regression(
      cdf_estimation,
      distribution = distribution,
      conf_level = conf_level
    )

    return(mrr_output)
  }

  # Segmented regression:
  if (purrr::is_null(k)) {
    message("Automated segmentation process was used",
            " problem of overestimation may have occured!")

    control$fix.npsi <- FALSE

    seg_mrr <- with(
      cdf_failed,
      segmented::segmented.lm(
        mrr,
        psi = NA,
        control = control
      )
    )
  } else {
    seg_mrr <- with(
      cdf_failed,
      segmented::segmented.lm(
        mrr,
        psi = quantile(q, probs = 1 / k * (1:(k - 1))),
        control = control
      )
    )
  }

  # Group membership:
  group_seg <- seg_mrr$id.group

  # Test for successful segmentation of all failed units:
  if (purrr::is_null(group_seg)) {
    # Not succeeded:
    stop(
      "Segmentation has not succeeded. Reduce 'k' in the function call!",
      call. = FALSE
    )
  }

  # Succeeded:
  cdf_failed$group <- group_seg + 1

  cdf_split <- split(cdf_failed, cdf_failed$group)

  mrr_output <- purrr::map(
    cdf_split,
    rank_regression,
    distribution = distribution,
    conf_level = conf_level
  )

  names(mrr_output) <- paste("mod", seq_along(mrr_output), sep = "_")

  class(mrr_output) <- c("wt_model", "wt_mixmod_regression", class(mrr_output))

  return(mrr_output)
}



#' @export
print.wt_mixmod_regression <- function(x,
                                       digits = max(
                                         3L,
                                         getOption("digits") - 3L
                                       ),
                                       ...
) {
  cat("Mixmod Regression:\n")
  purrr::walk2(x, seq_along(x), function(model_estimation, i) {
    cat(paste0("Subgroup ", i, ":\n"))
    indent_by(print(model_estimation), 2)
  })
}



#' @export
print.wt_mixmod_regression_list <- function(x,
                                            digits = max(
                                              3L,
                                              getOption("digits") - 3L
                                            ),
                                            ...
) {
  cat(paste("List of", length(x), "mixmod regressions:\n"))
  purrr::walk2(x, names(x), function(mixmod_regression, method) {
    print(mixmod_regression)
    cat(paste("Method of CDF Estimation:", method, "\n"))
    cat("\n")
  })
  invisible(x)
}



#' Weibull Mixture Model Estimation using EM-Algorithm
#'
#' @description
#' This method applies the expectation-maximization (EM) algorithm to estimate the
#' parameters of a univariate Weibull mixture model. See 'Details'.
#'
#' @details
#' The EM algorithm is an iterative algorithm for which starting values must be
#' defined. Starting values can be provided for the unknown parameter vector as
#' well as for the posterior probabilities. This implementation employs initial
#' values for the posterior probabilities. These are assigned randomly
#' by using the Dirichlet distribution, the conjugate prior of a multinomial
#' distribution (see Mr. Gelissen's blog post listed under *references*).
#'
#' **M-Step** : On the basis of the initial posterior probabilities, the
#' parameter vector is estimated with *Newton-Raphson*.
#'
#' **E-Step** : The actual estimated parameter vector is used to perform an
#' update of the posterior probabilities.
#'
#' This procedure is repeated until the complete log-likelihood has converged.
#'
#' @param x A tibble of class `wt_reliability_data` returned by [reliability_data].
#' @param distribution `"weibull"` until further distributions are implemented.
#' @param conf_level Confidence level for the intervals of the Weibull parameters
#' of every component `k`.
#' @param k Number of mixture components.
#' @param method `"EM"` until other methods are implemented.
#' @param n_iter Integer defining the maximum number of iterations.
#' @param conv_limit Numeric value defining the convergence limit.
#' @param diff_loglik Numeric value defining the maximum difference between
#' log-likelihood values, which seems permissible.
#' @template dots
#'
#' @return A list with classes `wt_model` and `wt_mixmod_em`. The length of the
#' list depends on the number of specified subgroups `k`. The first `k` lists
#' contain information provided by [ml_estimation]. The values of `logL`, `aic`
#' and `bic` are the results of a weighted log-likelihood, where the weights are
#' the posterior probabilities determined by the algorithm. The last list summarizes
#' further results of the EM algorithm and is therefore called `em_results`. It
#' contains the following elements:
#'
#' * `a_priori` : A vector with estimated prior probabilities.
#' * `a_posteriori` : A matrix with estimated posterior probabilities.
#' * `groups` : Numeric vector specifying the group membership of every observation.
#' * `logL` : The value of the complete log-likelihood.
#' * `aic` : Akaike Information Criterion.
#' * `bic` : Bayesian Information Criterion.
#'
#' @encoding UTF-8
#'
#' @references
#'
#' * Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by Failure Mode,
#'   Quality Progress, 35(6), 47-52, 2002
#' * Blog post by Stefan Gelissen:
#'   [R code for fitting a mixture distribution to censored
#'   data](https://blogs2.datall-analyse.nl/2016/02/18/rcode_mixture_distribution_censored/) ;
#'   last accessed on 8th December 2020
#'
#' @examples
#' # Reliability data preparation:
#' ## Data for mixture model:
#' data_mix <- reliability_data(
#'   voltage,
#'   x = hours,
#'   status = status
#' )
#'
#' # Example 1 - EM algorithm with k = 2:
#' mix_mod_em <- mixmod_em(
#'   x = data_mix,
#'   conf_level = 0.95,
#'   k = 2,
#'   n_iter = 150
#' )
#'
#' # Example 2 - Maximum likelihood is applied when k = 1:
#' mix_mod_em_2 <- mixmod_em(
#'   x = data_mix,
#'   conf_level = 0.95,
#'   k = 1,
#'   n_iter = 150
#' )
#'
#' @md
#'
#' @export
mixmod_em <- function(x, ...) {
  UseMethod("mixmod_em")
}



#' @rdname mixmod_em
#'
#' @export
mixmod_em.wt_reliability_data <- function(x,
                                          distribution = "weibull",
                                          conf_level = .95,
                                          k = 2,
                                          method = "EM",
                                          n_iter = 100L,
                                          conv_limit = 1e-6,
                                          diff_loglik = 0.01,
                                          ...
) {

  distribution <- match.arg(distribution)
  method <- match.arg(method)

  mixmod_em_(
    data = x,
    distribution = distribution,
    conf_level = conf_level,
    k = k,
    method = method,
    n_iter = n_iter,
    conv_limit = conv_limit,
    diff_loglik = diff_loglik,
    drop_id = FALSE
  )
}



#' Weibull Mixture Model Estimation using EM-Algorithm
#'
#' @inherit mixmod_em description details return references
#'
#' @inheritParams mixmod_em
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#' could be every characteristic influencing the reliability of a product, e.g.
#' operating time (days/months in service), mileage (km, miles), load cycles.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is a
#' right censored observation (= 0) or a failure (= 1).
#'
#' @seealso [mixmod_em]
#'
#' @examples
#' # Vectors:
#' hours <- voltage$hours
#' status <- voltage$status
#'
#' # Example 1 - EM algorithm with k = 2:
#' mix_mod_em <- mixmod_em(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   conf_level = 0.95,
#'   k = 2,
#'   n_iter = 150
#' )
#'
#'#' # Example 2 - Maximum likelihood is applied when k = 1:
#' mix_mod_em_2 <- mixmod_em(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   conf_level = 0.95,
#'   k = 1,
#'   method = "EM",
#'   n_iter = 150
#' )
#'
#' @md
#'
#' @export
mixmod_em.default <- function(x,
                              status,
                              distribution = "weibull",
                              conf_level = 0.95,
                              k = 2,
                              method = "EM",
                              n_iter = 100L,
                              conv_limit = 1e-6,
                              diff_loglik = 0.01,
                              ...
) {

  distribution <- match.arg(distribution)
  method <- match.arg(method)

  data <- reliability_data(x = x, status = status)

  mixmod_em_(
    data = data,
    distribution = distribution,
    conf_level = conf_level,
    k = k,
    method = method,
    n_iter = n_iter,
    conv_limit = conv_limit,
    diff_loglik = diff_loglik,
    drop_id = TRUE
  )
}

mixmod_em_ <- function(data,
                       distribution,
                       conf_level,
                       k,
                       method,
                       n_iter,
                       conv_limit,
                       diff_loglik,
                       drop_id
) {

  x <- data$x
  status <- data$status

  # Providing initial random a-posteriors (see references, blog post Mr. Gelissen):
  post <- rdirichlet(n = length(x), par = rep(0.1, k))

  # mixture_em_cpp() for applying EM-Algorithm:
  mix_est <- mixture_em_cpp(
    x = x,
    status = status,
    post = post,
    distribution = distribution,
    k = k,
    method = method,
    n_iter = n_iter,
    conv_limit = conv_limit
  )

  ############## New Approach ##############
  # Try to apply ml_estimation where observations are weighted with a-posterioris:
  ml <- try(
    apply(
      mix_est$posteriori,
      MARGIN = 2,
      FUN = ml_estimation,
      x = data,
      distribution = distribution,
      conf_level = conf_level
    ),
    silent = TRUE
  )
  if (class(ml) == "try-error") {
    stop(
      paste(
        ml[1],
        sprintf("\n For k = %s subcomponents the above problem occured!", k),
        "\n Hint: Reduce k in function call and try again. If this does",
        "not succeed a mixture model seems not to be appropriate.",
        "\n Instead use k = 1 to perform ml_estimation()."
      ),
      call. = FALSE
    )
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
    stop("Parameter estimation was not successful!", call. = FALSE)
  }

  # separate observations using maximum a-posteriori method (MAP):
  split_obs <- apply(mix_est$posteriori, 1, which.max)

  # modify data of each model estimation accordingly
  for (i in seq_len(k)) {
    ml[[i]]$data <- ml[[i]]$data[i == split_obs,]

    # Drop id column in default case. The user did not supply id and therefore
    # does not expect the model data to include it. Data is ensured to have 'x'
    # as name of lifetime characteristic column
    data <- data[c("x", "status")]
    if (drop_id) ml[[i]]$data <- ml[[i]]$data[c("x", "status")]
  }

  names(ml) <- sprintf("mod_%i", 1:k)

  em_results <- list(
    a_priori = mix_est$priori,
    a_posteriori = mix_est$posteriori,
    groups = split_obs,
    logL = logL_complete,
    aic = aic_complete,
    bic = bic_complete
  )

  class(em_results) <- c("wt_em_results", class(em_results))

  ml$em_results <- em_results

  class(ml) <- c("wt_model", "wt_mixmod_em", class(ml))

  ml
}



#' @export
print.wt_mixmod_em <- function(x,
                               digits = max(3L, getOption("digits") - 3L),
                               ...
) {
  cat("Mixmod EM:\n")
  mods <- x[-length(x)]
  purrr::walk2(mods, seq_along(mods), function(model_estimation, i) {
    cat(paste0("Subgroup ", i, ":\n"))
    indent_by(print(model_estimation), 2)
  })
  print(x[[length(x)]])
}



#' @export
print.wt_em_results <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                ...
) {
  cat("EM Results:\n")
  indent_by({
    cat("A priori\n")
    cat(x$a_priori)
  }, 2)
}
