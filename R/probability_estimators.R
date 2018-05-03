#' Estimation of failure probabilities using Johnson's method
#'
#' This non-parametric approach is used to estimate the failure probabilites in
#' terms of (multiple) right censored data. Compared to complete cases the
#' correction is done by calculating adjusted ranks which takes non-defective
#' units into account.
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#'
#' @return A data frame containing id, lifetime characteristic, status of the
#'   unit, the adjusted rank and the estimated failure probabilty. For right
#'   censored observations the cells of the rank and probabilty columns are
#'   filled with NA values.
#'
#' @export
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' df_john <- johnson_method(x = obs, event = state, id = uic)

johnson_method <- function(x, event, id = rep("XXXXXX", length(x))) {
  if (all(event == 1)) {
    warning("Use mr_method() since there is no censored data problem!")
  }
  df <- data.frame(time = x, status = event)
  df <- dplyr::group_by(df, time)
  df <- dplyr::mutate(df, failure = sum(status == 1),
                      survivor = sum(status == 0))
  df <- dplyr::distinct(df, time, .keep_all = TRUE)
  df <- dplyr::arrange(df, time)
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, n_i = failure + survivor,
                      n_out = dplyr::lag(cumsum(n_i), n = 1L, default = 0),
                      rank = failure)
  df <- dplyr::mutate(df, rank = calculate_ranks(f = failure,
                                                 n_out = n_out,
                                                 n = sum(n_i)))
  # df <- dplyr::mutate(df, prob = qbeta(.5, rank, sum(n_i) - rank + 1))
  df <- dplyr::mutate(df, prob = (rank - .3) / (sum(n_i) + .4))

  event <- event[order(x)]

  df_output <- data.frame(
    id = id[order(x)],
    characteristic = x[order(x)],
    status = event,
    rank = ifelse(event == 1, df$rank[match(x[order(x)], df$time)],
                  NA_real_),
    prob = ifelse(event == 1, df$prob[match(x[order(x)], df$time)],
                  NA_real_)
    )
  return(df_output)
}


#' Estimation of failure probabilities using Kaplan-Meier
#'
#' Whereas the non-parametric Kaplan-Meier estimator is used to estimate the
#' survival function \emph{S(t)} in terms of (multiple) right censored data, the
#' complement is an estimate of the cumulative distribution function \emph{F(t)}.
#' One modification is made in contrast to the orginial Kaplan-Meier estimator
#' (based on \emph{NIST/SEMATECH e-Handbook of Statistical Methods}, 8.2.1.5.):
#' If the last unit (unit with highest observed lifetime) is a defective unit,
#' the estimator is adjusted in such a way that the survival estimate for this
#' unit is not \emph{zero} and hence the estimate for the failure probability is
#' not equal to \emph{one}. Otherwise the estimate in this context would be very
#' pessimisitc.
#' Since the failure probability estimation in this function is not based on median ranks,
#' the Betabinomial and Fisher confidence intervals cannot be estimated on the basis of
#' Kaplan-Meier failure probabilities.
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#'
#' @return A data frame containing id, lifetime characteristic, status of the
#'   unit and the estimated failure probabilty. For right censored observations
#'   the cells of probability column are filled with NA.
#'
#' @references \emph{NIST/SEMATECH e-Handbook of Statistical Methods},
#' \emph{8.2.1.5. Empirical model fitting - distribution free (Kaplan-Meier) approach},
#' https://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm, 30/04/2018
#'
#' @export
#' @examples
#' # Example 1
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' df_kap <- kaplan_method(x = obs, event = state, id = uic)
#'
#'# Example 2
#'  df <- data.frame(obs = c(10000, 10000, 20000, 20000, 30000,
#'                          30000, 30000, 30000, 40000, 50000,
#'                          50000, 60000, 70000, 70000, 70000,
#'                          70000, 80000, 80000, 80000, 80000,
#'                          90000, 90000, 100000),
#'                  state = rep(1, 23))
#'
#' df_kap2 <- kaplan_method(x = df$obs, event = df$state)

kaplan_method <- function(x, event, id = rep("XXXXXX", length(x))) {
  if (all(event == 1)) {
    warning("Use mr_method() since there is no censored data problem!")
  }
  df <- data.frame(time = x, status = event)
  df <- dplyr::group_by(df, time)
  df <- dplyr::mutate(df, failure = sum(status == 1),
                      survivor = sum(status == 0))
  df <- dplyr::distinct(df, time, .keep_all = TRUE)
  df <- dplyr::arrange(df, time)
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, n_i = failure + survivor,
                      n_out = dplyr::lag(cumsum(n_i), n = 1L, default = 0),
                      n_in = sum(n_i) - n_out)
  if (event[which.max(x)] == 0) {
    df <- dplyr::mutate(df, prob = 1 - cumprod((n_in - failure) / n_in))
  } else {
    # df <- dplyr::mutate(df,
    #                     prob = 1 - cumprod(((n_in + 1) - failure) / (n_in + 1      # )))
    df <- dplyr::mutate(df,
          prob = 1 - (((n_in + .7) / (n_in + .4)) * cumprod(((n_in + .7)
            - failure) / (n_in + 1.7))))
  }
  event <- event[order(x)]

  df_output <- data.frame(
    id = id[order(x)],
    characteristic = x[order(x)],
    status = event,
    prob = ifelse(event == 1, df$prob[match(x[order(x)], df$time)],
                  NA_real_)
  )
  return(df_output)
}


#' Estimation of failure probabilities using Median Ranks
#'
#' This non-parametric approach (\emph{Median Ranks}) is used to estimate the
#' failure probabilites in terms of complete data. Two methods are available to
#' estimate the cumulative distribution function \emph{F(t)}:
#' \itemize{
#'   \item "benard"; Benard's approximation for Median Ranks
#'   \item "invbeta"; Exact Median Ranks using the inverse beta distribution
#' }
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of ones indicating that every unit \emph{i} has failed.
#' @param id a character vector for the identification of every unit.
#' @param method method for the estimation of the cdf. Can be "benard" (default)
#' or "invbeta".
#'
#' @return A data frame containing id, lifetime characteristic, status of the
#'   unit, the rank and the estimated failure probabilty.
#'
#' @export
#' @examples
#' # Example 1
#' obs   <- seq(10000, 100000, 10000)
#' state <- rep(1, length(obs))
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23", "Uy12", "kl1a")
#'
#' df_mr <- mr_method(x = obs, event = state, id = uic,
#'                    method = "benard")
#'
#' # Example 2
#' df_mr_invbeta <- mr_method(x = obs, event = state, id = uic,
#'                            method = "invbeta")

mr_method <- function(x, event = rep(1, length(x)),
                      id = rep("XXXXXX", length(x)),
                      method = "benard") {
  if (!all(event == 1)) {
    stop("Use johnson_method() or kaplan_method() since there is a censored data problem!")
  }
  df <- data.frame(time = x, status = event)
  df <- dplyr::distinct(df, time, .keep_all = TRUE)
  df <- dplyr::arrange(df, time)
  df <- dplyr::mutate(df, rank = rank(time, ties.method = "first"))
  if (method == "benard") {
    df <- dplyr::mutate(df, prob = (rank - .3) / (length(x) + .4))
  } else {
    df <- dplyr::mutate(df, prob = stats::qbeta(.5, rank, length(x) - rank + 1))
  }

  event <- event[order(x)]

  df_output <- data.frame(
    id = id[order(x)],
    characteristic = x[order(x)],
    status = event,
    rank = ifelse(event == 1, df$rank[match(x[order(x)], df$time)],
      NA_real_),
    prob = ifelse(event == 1, df$prob[match(x[order(x)], df$time)],
      NA_real_)
  )
  return(df_output)
}

#' Estimation of failure probabilities using the Nelson-Aalen estimator
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#'
#' @return A data frame containing id, lifetime characteristic, status of the
#'   unit and the estimated failure probabilty. For right censored observations
#'   the cells of probability column are filled with NA.
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
#'            "fl29", "AX23","Uy12", "kl1a")
#'
#' df_nel <- nelson_method(x = obs, event = state, id = uic)
#'
nelson_method <- function(x, event, id = rep("XXXXXX", length(x))) {
  if (all(event == 1)) {
    warning("Use mr_method() since there is no censored data problem!")
  }
  df <- data.frame(time = x, status = event)
  df <- dplyr::group_by(df, time)
  df <- dplyr::mutate(df, failure = sum(status == 1),
                      survivor = sum(status == 0))
  df <- dplyr::distinct(df, time, .keep_all = TRUE)
  df <- dplyr::arrange(df, time)
  df <- dplyr::ungroup(df)
  df <- dplyr::mutate(df, n_out = failure + survivor,
                      n_i = length(x) - dplyr::lag(cumsum(n_out), n = 1L, default = 0),
                      H_nel = cumsum(failure/n_out),
                      prob = 1 - exp(-H_nel))

  event <- event[order(x)]

  df_output <- data.frame(
    id = id[order(x)],
    characteristic = x[order(x)],
    status = event,
    prob = ifelse(event == 1, df$prob[match(x[order(x)], df$time)],
                  NA_real_)
  )
  return(df_output)
}
