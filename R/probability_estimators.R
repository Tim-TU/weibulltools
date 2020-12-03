#' Estimation of Failure Probabilities
#'
#' @description
#' Estimate failure probabilities for uncensored or (multiple) right-censored
#' data.
#'
#' @details
#'
#' One or multiple of the following methods can be used as the \code{methods}
#' argument:
#' \itemize{
#'   \item \code{"mr"}: This non-parametric approach (\emph{Median Ranks}) is
#'     used to estimate the failure probabilities in terms of complete data.
#'     Tied observations can be handled in three ways, "max", "min" and "average"
#'     (usage is listed in 'Options')).
#'     Two methods are available to estimate the cumulative distribution
#'     function \emph{F(t)} (See 'Options'):
#'     \itemize{
#'       \item "benard"; Benard's approximation for Median Ranks.
#'       \item "invbeta"; Exact Median Ranks using the inverse beta distribution.
#'     }
#'   \item \code{"johnson"}: This non-parametric approach is used to estimate
#'     the failure probabilities in terms of uncensored or (multiple) right
#'     censored data. Compared to complete data the correction is done by
#'     calculating adjusted ranks which takes non-defective units into account.
#'   \item \code{"kaplan"}: Whereas the non-parametric Kaplan-Meier estimator is
#'     used to estimate the survival function \emph{S(t)} in terms of (multiple)
#'     right censored data, the complement is an estimate of the cumulative
#'     distribution function \emph{F(t)}. One modification is made in contrast
#'     to the orginial Kaplan-Meier estimator (based on
#'     \emph{NIST/SEMATECH e-Handbook of Statistical Methods}, 8.2.1.5.): If the
#'     last unit (unit with highest observed lifetime) is a defective unit, the
#'     estimator is adjusted in such a way that the survival estimate for this
#'     unit is not \emph{zero} and therefore the estimate for the failure
#'     probability is not equal to \emph{one}. Otherwise the estimate in this
#'     context would be too pessimisitc.
#'     Since the failure probability estimation in this function is not based on
#'     \emph{Median Ranks}, the Betabinomial confidence intervals \emph{cannot} be
#'     calculated on the basis of Kaplan-Meier failure probabilities.
#'   \item \code{"nelson"}: This non-parametric approach estimates the cumulative
#'     hazard rate in terms of (multiple) right censored data. By equating the
#'     definition of the hazard rate with the hazard rate according to
#'     Nelson-Aalen one can calculate the failure probabilities.
#'     Since the failure probability estimation in this function is not based on
#'     \emph{Median Ranks}, the Betabinomial confidence intervals cannot be calculated
#'     on the basis of Nelson-Aalen failure probabilities.
#' }
#'
#' @param x A tibble returned by \link{reliability_data}.
#' @param methods One or multiple methods of \code{"mr"}, \code{"johnson"},
#'   \code{"kaplan"} or \code{"nelson"} used for estimating the failure
#'   probabilities. See 'Details'.
#' @param options A list of named options. See 'Options'.
#'
#' @section Options:
#'
#' \itemize{
#'   \item \code{mr_method}: \code{"benard"} (default) or \code{"invbeta"}.
#'   \item \code{mr_ties.method}: \code{c("max", "min", "average")}, default is "max".
#' }
#'
#' @return A tibble containing the following columns:
#' \itemize{
#'   \item \code{id}: Identification.
#'   \item \code{x}: Lifetime characteristic.
#'   \item \code{status}: Binary data (0 or 1) indicating whether a unit is a
#'     right censored observation (=0) or a failure (=1).
#'   \item \code{rank}: Adjusted rank. Applicable for method \code{"mr"}
#'     and \code{"johnson"}, otherwise \code{NA}.
#'   \item \code{prob}: Estimated failure probability.
#'   \item \code{method}: Method, which was used for estimating the failure
#'     probability.
#' }
#'
#' @seealso estimate_cdf.default
#'
#' @examples
#'
#' @export
#'
estimate_cdf <- function(x, ...) {
  UseMethod("estimate_cdf")
}



#' Estimation of Failure Probabilities
#'
#' @inherit estimate_cdf description details return
#' @inheritSection estimate_cdf Options
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id A character vector for the identification of every unit.
#' @param methods One or multiple methods of \code{"mr"}, \code{"johnson"},
#'   \code{"kaplan"} or \code{"nelson"} used for estimating the failure
#'   probabilities. See 'Details'.
#' @param options A list of named options. See 'Options'.
#'
#' @seealso estimate_cdf
#'
#' @examples
#'
#' @export
estimate_cdf.default <- function(x,
                                 status,
                                 id = rep("XXXXXX", length(x)),
                                 methods = c("mr", "johnson", "kaplan", "nelson"),
                                 options,
                                 ...
) {
  data <- reliability_data(x = x, status = status, id = id)

  methods <- if (missing(methods)) {
    "mr"
  } else {
    unique(match.arg(methods, several.ok = TRUE))
  }

  estimate_cdf.reliability_data(
    x = data,
    methods = methods,
    options = options
  )
}



#' @rdname estimate_cdf
#'
#' @export
estimate_cdf.reliability_data <- function(
  x, methods = c("mr", "johnson", "kaplan", "nelson"), options = list(), ...
) {

  methods <- if (missing(methods)) {
    "mr"
  } else {
    unique(match.arg(methods, several.ok = TRUE))
  }

  method_funs <- list(
    mr = mr_method_,
    johnson = johnson_method_,
    kaplan = kaplan_method_,
    nelson = nelson_method_
  )

  purrr::map_dfr(methods, function(method) {
    if (method == "mr") {
      method_funs[[method]](
        data = x,
        method = if (is.null(options$mr_method)) "benard" else options$mr_method,
        ties.method = if (is.null(options$mr_ties.method)) "max" else options$mr_ties.method
      )
    } else {
      method_funs[[method]](data = x)
    }
  })
}

#' Estimation of Failure Probabilities using Median Ranks
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' The functionality of this function is incorporated into \code{\link{estimate_cdf}}.
#'
#' @details
#' This non-parametric approach (\emph{Median Ranks}) is used to estimate the
#' failure probabilities in terms of complete data. Two methods are available to
#' estimate the cumulative distribution function \emph{F(t)}:
#' \itemize{
#'   \item "benard"; Benard's approximation for Median Ranks
#'   \item "invbeta"; Exact Median Ranks using the inverse beta distribution
#' }
#'
#' @inheritParams estimate_cdf.default
#' @param status A vector of ones indicating that every unit \emph{i} has failed.
#' @param method Method for the estimation of the cdf. Can be "benard" (default)
#' or "invbeta".
#' @param ties.method A character string specifying how ties are treated, default is "max".
#'
#' @return A tibble containing id, lifetime characteristic, status of the
#'   unit, the rank and the estimated failure probability.
#'
#' @examples
#' # Example 1
#' obs   <- seq(10000, 100000, 10000)
#' state <- rep(1, length(obs))
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' tbl_mr <- mr_method(x = obs, status = state, id = uic,
#'                    method = "benard")
#'
#' # Example 2
#' tbl_mr_invbeta <- mr_method(x = obs, status = state, id = uic,
#'                            method = "invbeta")
#'
#' @export
mr_method <- function(
  x,
  status = rep(1, length(x)),
  id = rep("XXXXXX", length(x)),
  method = c("benard", "invbeta"),
  ties.method = c("max", "min", "average")
) {
  deprecate_soft("2.0.0", "mr_method()")

  method <- match.arg(method)
  ties.method <- match.arg(ties.method)

  if (!((length(x) == length(status)) && (length(x) == length(id)))) {
    stop("x, status and id must be of same length.")
  }

  data <- reliability_data(x = x, status = status, id = id)

  mr_method_(data, method, ties.method)
}

mr_method_ <- function(data, method = "benard", ties.method = "max") {

  if (!all(data$status == 1)) {
    message("The 'mr' method only considers failed units (status == 1) and does",
            " not retain intact units (status == 0).")
  }

  tbl_in <- data

  tbl_calc <- tbl_in %>%
    dplyr::filter(status == 1) %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(rank = rank(x, ties.method = ties.method))

  if (method == "benard") {
    tbl_calc <- dplyr::mutate(tbl_calc, prob = (rank - .3) / (length(x) + .4))
  } else {
    tbl_calc <- dplyr::mutate(tbl_calc, prob = stats::qbeta(.5, rank, length(x) - rank + 1))
  }

  tbl_out <- tbl_calc %>%
    dplyr::mutate(method = "mr") %>%
    dplyr::select(id, x, status, rank, prob, method)

  class(tbl_out) <- c("cdf_estimation", class(tbl_out))

  return(tbl_out)
}


#' Estimation of Failure Probabilities using Johnson's Method
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' The functionality of this function is incorporated into \code{\link{estimate_cdf}}.
#'
#' @details
#' This non-parametric approach is used to estimate the failure probabilities in
#' terms of uncensored or (multiple) right censored data. Compared to complete data the
#' correction is done by calculating adjusted ranks which takes non-defective
#' units into account.
#'
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @inheritParams mr_method
#'
#' @return A tibble containing id, lifetime characteristic, status of the
#'   unit, the adjusted rank and the estimated failure probability. For right
#'   censored observations the cells of the rank and probabilty columns are
#'   filled with NA values.
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' tbl_john <- johnson_method(x = obs, status = state, id = uic)
#'
#' @export
johnson_method <- function(x, status, id = rep("XXXXXX", length(x))) {

  deprecate_soft("2.0.0", "johnson_method()")

  if (!((length(x) == length(status)) && (length(x) == length(id)))) {
    stop("x, status and id must be of same length.")
  }

  data <- reliability_data(x = x, status = status, id = id)

  johnson_method_(data)
}

johnson_method_ <- function(data) {

  tbl_in <- data

  tbl_calc <- tbl_in %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(failure = sum(status == 1), survivor = sum(status == 0)) %>%
    dplyr::distinct(x, .keep_all = TRUE) %>%
    dplyr::arrange(x) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_i = failure + survivor,
      n_out = dplyr::lag(cumsum(n_i), n = 1L, default = 0)
    ) %>%
    dplyr::mutate(
      rank = calculate_ranks(
        f = failure,
        n_out = n_out,
        n = sum(n_i)
      )
    ) %>%
    dplyr::mutate(prob = (rank - .3) / (sum(n_i) + .4))

  tbl_out <- tbl_in %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(
      rank = ifelse(
        status == 1,
        tbl_calc$rank[match(x[order(x)], tbl_calc$x)],
        NA_real_
      ),
      prob = ifelse(
        status == 1,
        tbl_calc$prob[match(x[order(x)], tbl_calc$x)],
        NA_real_
      )
    ) %>%
    dplyr::mutate(method = "johnson") %>%
    dplyr::select(id, x, status, rank, prob, method)

  class(tbl_out) <- c("cdf_estimation", class(tbl_out))

  return(tbl_out)
}


#' Estimation of Failure Probabilities using Kaplan-Meier
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' The functionality of this function is incorporated into \code{\link{estimate_cdf}}.
#'
#' @details
#' Whereas the non-parametric Kaplan-Meier estimator is used to estimate the
#' survival function \emph{S(t)} in terms of (multiple) right censored data, the
#' complement is an estimate of the cumulative distribution function \emph{F(t)}.
#' One modification is made in contrast to the orginial Kaplan-Meier estimator
#' (based on \emph{NIST/SEMATECH e-Handbook of Statistical Methods}, 8.2.1.5.):
#' If the last unit (unit with highest observed lifetime) is a defective unit,
#' the estimator is adjusted in such a way that the survival estimate for this
#' unit is not \emph{zero} and therefore the estimate for the failure probability is
#' not equal to \emph{one}. Otherwise the estimate in this context would be too
#' pessimisitc.
#' Since the failure probability estimation in this function is not based on
#' \emph{Median Ranks}, the Betabinomial confidence intervals cannot be
#' calculated on the basis of Kaplan-Meier failure probabilities.
#'
#' @inheritParams johnson_method
#'
#' @return A tibble containing id, lifetime characteristic, status of the
#'   unit and the estimated failure probabilty. For right censored observations
#'   the cells of probability column are filled with NA.
#'
#' @references \emph{NIST/SEMATECH e-Handbook of Statistical Methods},
#' \emph{8.2.1.5. Empirical model fitting - distribution free (Kaplan-Meier) approach},
#' https://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm, 30/04/2018
#'
#' @examples
#' # Example 1
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' tbl_kap <- kaplan_method(x = obs, status = state, id = uic)
#'
#'# Example 2
#' tbl <- tibble(obs = c(10000, 10000, 20000, 20000, 30000,
#'                          30000, 30000, 30000, 40000, 50000,
#'                          50000, 60000, 70000, 70000, 70000,
#'                          70000, 80000, 80000, 80000, 80000,
#'                          90000, 90000, 100000),
#'                  state = rep(1, 23))
#'
#' tbl_kap2 <- kaplan_method(x = tbl$obs, status = tbl$state)
#'
#' @export
kaplan_method <- function(x, status, id = rep("XXXXXX", length(x))) {

  deprecate_soft("2.0.0", "kaplan_method()")

  if (!((length(x) == length(status)) && (length(x) == length(id)))) {
    stop("x, status and id must be of same length.")
  }

  data <- reliability_data(x = x, status = status, id = id)

  kaplan_method_(data)
}

kaplan_method_ <- function(data) {

  if (all(data$status == 1)) {
    warning('Use methods = "mr" since there is no censored data problem!')
  }

  tbl_in <- data

  tbl_calc <- tbl_in %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(
      failure = sum(status == 1),
      survivor = sum(status == 0)
    ) %>%
    dplyr::distinct(x, .keep_all = TRUE) %>%
    dplyr::arrange(x) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_i = failure + survivor,
      n_out = dplyr::lag(cumsum(n_i), n = 1L, default = 0),
      n_in = sum(n_i) - n_out
    )

  if (data$status[which.max(data$x)] == 0) {
    tbl_calc <- tbl_calc %>%
      dplyr::mutate(
        prob = 1 - cumprod((n_in - failure) / n_in)
      )
  } else {
    tbl_calc <- tbl_calc %>%
      dplyr::mutate(
        prob = 1 - (((n_in + .7) / (n_in + .4)) *
                      cumprod(((n_in + .7) - failure) / (n_in + 1.7)))
      )
  }

  tbl_out <- tbl_in %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(
      rank = NA_real_,
      prob = ifelse(
        status == 1,
        tbl_calc$prob[match(x[order(x)], tbl_calc$x)],
        NA_real_
      ),
      method = "kaplan"
    ) %>%
    dplyr::select(id, x, status, rank, prob, method)

  class(tbl_out) <- c("cdf_estimation", class(tbl_out))

  return(tbl_out)
}


#' Estimation of Failure Probabilities using the Nelson-Aalen Estimator
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' The functionality of this function is incorporated into \code{\link{estimate_cdf}}.
#'
#' @details
#' This non-parametric approach estimates the cumulative hazard rate in
#' terms of (multiple) right censored data. By equating the definition of the
#' hazard rate with the hazard rate according to Nelson-Aalen one can calculate
#' the failure probabilities.
#' Since the failure probability estimation in this function is not based on
#' \emph{Median Ranks}, the Betabinomial confidence intervals cannot be calculated
#' on the basis of Nelson-Aalen failure probabilities.
#'
#' @inheritParams johnson_method
#'
#' @return A tibble containing id, lifetime characteristic, status of the
#'   unit and the estimated failure probabilty. For right censored observations
#'   the cells of probability column are filled with NA.
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' tbl_nel <- nelson_method(x = obs, status = state, id = uic)
#'
#' @export
nelson_method <- function(x, status, id = rep("XXXXXX", length(x))) {

  deprecate_soft("2.0.0", "nelson_method()")

  if (!((length(x) == length(status)) && (length(x) == length(id)))) {
    stop("x, status and id must be of same length.")
  }

  data <- reliability_data(x = x, status = status, id = id)

  nelson_method_(data)
}

nelson_method_ <- function(data) {

  if (all(data$status == 1)) {
    warning('Use methods = "mr" since there is no censored data problem!')
  }

  tbl_in <- data

  tbl_calc <- tbl_in %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(
      failure = sum(status == 1),
      survivor = sum(status == 0)
    ) %>%
  dplyr::distinct(x, .keep_all = TRUE) %>%
  dplyr::arrange(x) %>%
  dplyr::ungroup(x) %>%
  dplyr::mutate(
    n_out = failure + survivor,
    n_in = nrow(data) - dplyr::lag(cumsum(n_out), n = 1L, default = 0),
    lam_nel = ifelse(status == 1, failure / n_in, 0),
    H_nel = cumsum(lam_nel),
    prob = 1 - exp(-H_nel)
  )

  tbl_out <- tbl_in %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(
      rank = NA_real_,
      prob = ifelse(
        status == 1,
        tbl_calc$prob[match(x[order(x)], tbl_calc$x)],
        NA_real_
      ),
      method = "nelson"
    ) %>%
    dplyr::select(id, x, status, rank, prob, method)

  class(tbl_out) <- c("cdf_estimation", class(tbl_out))

  return(tbl_out)
}
