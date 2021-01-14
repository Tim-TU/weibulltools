#' Parameter Estimation of a Delay Distribution
#'
#' @description
#' This function models a delay (in days) random variable (e.g. in logistic,
#' registration, report) using a supposed continuous distribution. First, the
#' element-wise differences in days of both vectors \code{date_1} and
#' \code{date_2} are calculated and then the parameter(s) of the assumed
#' distribution are estimated with maximum likelihood. See 'Details' for more
#' information.
#'
#' @details
#' The distribution parameter(s) are determined on the basis of complete cases,
#' i.e. there is no \code{NA} in one of the related vector elements
#' \code{c(date_1[i], date_2[i])}. Time differences less than or equal to zero are
#' not considered as well.
#'
#' @param date_1 A vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the earlier of the two dates. Use \code{NA}
#'   for missing elements.
#' @param date_2 A vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the later of the two dates. Use \code{NA}
#'   for missing elements.
#' @param distribution Supposed distribution of the random variable.
#'
#' @return A list of class \code{delay_estimation} which contains:
#'   \itemize{
#'     \item \code{coefficients} : A named vector of estimated parameter(s).
#'     \item \code{delay} : A numeric vector of element-wise computed differences
#'       in days.
#'     \item \code{distribution} : Specified distribution.
#'   }
#'
#' @examples
#' # Example 1 - Delay in registration:
#' date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
#'                           "2014-06-26", "2014-03-10", "2014-05-14",
#'                           "2014-05-06", "2014-03-07", "2014-03-09",
#'                           "2014-04-13", "2014-05-20", "2014-07-07",
#'                           "2014-01-27", "2014-01-30", "2014-03-17",
#'                           "2014-02-09", "2014-04-14", "2014-04-20",
#'                           "2014-03-13", "2014-02-23", "2014-04-03",
#'                           "2014-01-08", "2014-01-08")
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
#'                           NA, NA, "2014-06-16", NA, "2014-05-23",
#'                           "2014-05-09", "2014-05-31", NA, "2014-04-13",
#'                           NA, NA, "2014-03-12", NA, "2014-06-02",
#'                           NA, "2014-03-21", "2014-06-19", NA, NA)
#'
#' params_delay_regist  <- dist_delay(
#'   date_1 = date_of_production,
#'   date_2 = date_of_registration,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Delay in report:
#' date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                     NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                     "2015-06-12", NA, "2015-05-04", NA, NA,
#'                     "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
#'                     "2015-11-26", NA, NA)
#'
#' date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
#'                     NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
#'                     "2015-07-11", NA, "2015-08-14", NA, NA,
#'                     "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
#'                     "2015-12-02", NA, NA)
#'
#' params_delay_report  <- dist_delay(
#'   date_1 = date_of_repair,
#'   date_2 = date_of_report,
#'   distribution = "exponential"
#' )
#'
#' @export
dist_delay <- function(date_1,
                       date_2,
                       distribution = c("lognormal", "exponential")
) {

  distribution <- match.arg(distribution)

  # Defining delay variable (for estimation) and origin variable (output):
  t_delay <- t_delay_origin <- as.numeric(
    difftime(
      time1 = as.Date(date_2, format = "%Y-%m-%d"),
      time2 = as.Date(date_1, format = "%Y-%m-%d"),
      units = "days"
    )
  )

  # Checks:
  ## all NA:
  if (all(is.na(t_delay))) {
    stop("All differences are NA. No parameters can be estimated!")
  }
  ## any or all delays are smaller or equal to zero:
  if (any(t_delay <= 0, na.rm = TRUE)) {
    if (all(t_delay <= 0, na.rm = TRUE)) {
      ### all:
      stop("All differences are smaller or equal to 0. No parameters can be estimated!")
    } else {
      ### any:
      warning("At least one of the time differences is smaller or equal to 0 and is",
              " ignored for the estimation step!")
      t_delay <- t_delay[t_delay > 0]
    }
  }

  if (distribution == "lognormal") {
    # sample size used for the computation of the population standard deviation.
    n <- sum(!is.na(t_delay))
    ml_loc <- mean(log(t_delay), na.rm = TRUE)
    ml_sc <- stats::sd(log(t_delay), na.rm = TRUE) * (n - 1) / n

    estimates <- c(loc = ml_loc, sc = ml_sc)
  }

  if (distribution == "exponential") {
    ml_sc <- mean(t_delay, na.rm = TRUE)

    estimates <- c(sc = ml_sc)
  }

  dist_output <- list(
    coefficients = estimates,
    delay = t_delay_origin,
    distribution = distribution
  )

  class(dist_output) <- c("delay_estimation", class(dist_output))

  return(dist_output)
}



#' Adjustment of Operating Times by Delays using a Monte Carlo Approach
#'
#' @description
#' In general, the amount of available information about units in the field is very
#' different. During the warranty period, there are only a few cases with complete
#' data (mainly \emph{failed units}) but lots of cases with incomplete data (usually
#' \emph{censored units}). As a result, the operating time of units with incomplete
#' information is often inaccurate and must be adjusted by delays.
#'
#' This function reduces the operating times of incomplete observations by simulated
#' delays (in days). A unit is considered as incomplete if the later of the two
#' dates is unknown, i.e. \code{date_2 = NA}. See 'Details' for some practical examples.
#'
#' Random delay numbers are drawn from the distribution determined by complete cases
#' (described in 'Details' of \code{\link{dist_delay}}).
#'
#' @details
#' In field data analysis time-dependent characteristics (e.g. \emph{time in service})
#' are often imprecisely recorded. These inaccuracies are caused by unconsidered delays.
#'
#' For a better understanding of the MCS application in the context of field data,
#' two cases are described below.
#' \itemize{
#'   \item \strong{Delay in registration}: It is common that a supplier, which provides
#'     parts to the manufacturing industry does not know when the unit, in which
#'     its parts are installed, were put in service (due to unknown \code{date_2},
#'     i.e. registration or sales date). Without taking the described delay into
#'     account, the time in service of the failed units would be the difference
#'     between the repair date and \code{date_1} (i.e. the production date) and for
#'     intact units the difference between the present date and \code{date_1}. But
#'     the real operating times are (much) shorter, since the stress on the
#'     components have not started until the whole systems were put in service.
#'     Hence, units with incomplete data (missing \code{date_2}) must be reduced by
#'     the delays.
#'   \item \strong{Delay in report}: Authorized repairers often do not immediately
#'     notify the manufacturer or OEM of repairs that were made during the warranty
#'     period, but instead pass the information about these repairs in collected
#'     forms e.g. weekly, monthly or quarterly. The resulting time difference between
#'     the reporting (\code{date_2}) of the repair in the guarantee database and the
#'     actual repair date (\code{date_1}), which is often assumed to be the failure
#'     date, is called the reporting delay. For a given date where the analysis
#'     is made there could be units which had a failure but are not registered
#'     and therefore treated as censored units. In order to take this case into
#'     account and according to the principle of equal opportunities, the lifetime
#'     of units with no report date (\code{date_2 = NA}) is reduced by simulated
#'     reporting delays.
#' }
#'
#' @param date_1 A vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the earlier of the two dates. Use \code{NA}
#'   for missing elements.
#'
#'   If more than one delay should be considered it must be a list where the first
#'   element contains the earlier dates of the first delay and the second element
#'   contains the earlier dates of the second delay, and so forth.(See 'Examples').
#' @param date_2 A vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the later of the two dates. Use \code{NA}
#'   for missing elements.
#'
#'   If more than one delay should be considered it must be a list where the first
#'   element contains the later dates of the first delay and the second element
#'   contains the later dates of the second delay, and so forth. (See 'Examples').
#' @param time A numeric vector of operating times. Use \code{NA} for missing elements.
#' @param status Optional argument. If used it has to be a vector of binary
#'   data (0 or 1) indicating whether unit \emph{i} is a right censored observation
#'   (= 0) or a failure (= 1). The effect of \code{status} on the return is described
#'   in 'Value'.
#' @param id A vector for the identification of every unit.
#' @param distribution Supposed distribution of the delay random variable. If more
#'   than one delay is to be considered and different distributions are assumed
#'   for each delay, the argument \code{distribution} must have the same length
#'   as list \code{date_1} (and \code{date_2}). For example, in the case of
#'   two delays with different distributions, one has to specify the argument as
#'   \code{distribution = c("lognormal", "exponential")}. Then the lognormal
#'   distribution is applied to the first delay and the exponential distribution
#'   to the second (See 'Examples').
#'
#' @return A list containing the following elements:
#'   \itemize{
#'     \item \code{data} : A tibble with classes \code{wt_mcs_data} and
#'       \code{wt_reliability_data} if \code{status} is provided. Since the
#'       class \code{wt_reliability_data} enables the direct usage of
#'       \code{data} inside
#'       \code{\link[=estimate_cdf]{estimate_cdf.wt_reliability_data}}, the
#'       required lifetime characteristic is automatically set to the operating
#'       time \code{time}.
#'
#'       If \code{status = NULL} class is \code{wt_mcs_data}, which is not
#'       supported by \code{estimate_cdf} due to missing \code{status}.
#'
#'       The tibble contains the following columns:
#'       \itemize{
#'         \item \code{date_1} : Earlier dates. If argument \code{date_1} is a list
#'           of length \emph{i, i > 1} (described in \strong{Arguments}) multiple
#'           columns with names \code{date_1.1}, \code{date_1.2}, ..., \code{date_1.i}
#'           and the corresponding values of the earlier dates are used.
#'         \item \code{date_2} : Later dates. In the case of a list with length greater
#'           than 1, the routine described above is used.
#'         \item \code{time} : Adjusted operating times for incomplete observations
#'           and input operating times for the complete observations.
#'         \item \code{status} (\strong{optional}) :
#'           \itemize{
#'             \item If argument \code{status = NULL} column \code{status} does
#'               not exist.
#'             \item If argument \code{status} is provided the column contains
#'               the entered binary data (0 or 1).
#'           }
#'         \item \code{id} : Identification of every unit.
#'       }
#'     \item \code{sim_data} : A tibble with column \code{sim_delay} that holds the
#'       simulated delay-specific numbers for incomplete cases and \code{0} for
#'       complete cases. If more than one delay was considered multiple columns
#'       \code{sim_delay.1}, \code{sim_delay.2}, ..., \code{sim_delay.i} with
#'       corresponding delay-specific random numbers are presented.
#'     \item \code{model_estimation} : A list containing a named list
#'       (\code{"delay_distribution"}) with output of \code{\link{dist_delay}}. For
#'       multiple delays the list contains as many lists as there are delays, i.e.
#'       (\code{"delay_distribution.1"}, \code{"delay_distribution.2"}, ...,
#'       \code{"delay_distribution.i"}).
#'   }
#'
#' @references Verband der Automobilindustrie e.V. (VDA); Qualitätsmanagement in
#'   der Automobilindustrie. Zuverlässigkeitssicherung bei Automobilherstellern
#'   und Lieferanten. Zuverlässigkeits-Methoden und -Hilfsmittel.; 4th Edition, 2016,
#'   <ISSN:0943-9412>
#'
#' @seealso \code{\link{estimate_cdf}}
#'
#' @examples
#' # Data for examples:
#' date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
#'                           "2014-06-26", "2014-03-10", "2014-05-14",
#'                           "2014-05-06", "2014-03-07", "2014-03-09",
#'                           "2014-04-13", "2014-05-20", "2014-07-07",
#'                           "2014-01-27", "2014-01-30", "2014-03-17",
#'                           "2014-02-09", "2014-04-14", "2014-04-20",
#'                           "2014-03-13", "2014-02-23", "2014-04-03",
#'                           "2014-01-08", "2014-01-08")
#'
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
#'                           NA, NA, "2014-06-16", NA, "2014-05-23",
#'                           "2014-05-09", "2014-05-31", NA, "2014-04-13",
#'                           NA, NA, "2014-03-12", NA, "2014-06-02",
#'                           NA, "2014-03-21", "2014-06-19", NA, NA)
#'
#' date_of_repair <-       c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                           NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                           "2015-06-12", NA, "2015-05-04", NA, NA,
#'                           "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
#'                           "2015-11-26", NA, NA)
#'
#' date_of_report <-       c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
#'                           NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
#'                           "2015-07-11", NA, "2015-08-14", NA, NA,
#'                           "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
#'                           "2015-12-02", NA, NA)
#'
#' time_in_service <- rep(1000, length(date_of_production))
#' status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - MCS for delay in registration:
#' mcs_regist <- mcs_delay(
#'   date_1 = date_of_production,
#'   date_2 = date_of_registration,
#'   time = time_in_service,
#'   status = status,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for delay in report:
#' mcs_report <- mcs_delay(
#'   date_1 = date_of_repair,
#'   date_2 = date_of_report,
#'   time = time_in_service,
#'   status = status,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - Reproducibility of random numbers:
#' set.seed(1234)
#' mcs_report_reproduce <- mcs_delay(
#'   date_1 = date_of_repair,
#'   date_2 = date_of_report,
#'   time = time_in_service,
#'   status = status,
#'   distribution = "exponential"
#' )
#'
#' # Example 4 - MCS for delays in registration and report with same distribution:
#' mcs_delays <- mcs_delay(
#'   date_1 = list(date_of_production, date_of_repair),
#'   date_2 = list(date_of_registration, date_of_report),
#'   time = time_in_service,
#'   status = status,
#'   distribution = "lognormal"
#' )
#'
#' # Example 5 - MCS for delays in registration and report with different distributions:
#' ## Assuming lognormal registration and exponential reporting delays.
#' mcs_delays_2 <- mcs_delay(
#'   date_1 = list(date_of_production, date_of_repair),
#'   date_2 = list(date_of_registration, date_of_report),
#'   time = time_in_service,
#'   status = status,
#'   distribution = c("lognormal", "exponential")
#' )
#'
#' @export
mcs_delay <- function(date_1,
                      date_2,
                      time,
                      status = NULL,
                      id = paste0("ID", seq_len(length(time))),
                      distribution = c("lognormal", "exponential")
) {

  # Checks:
  ## Check for (multiple) distributions:
  if (missing(distribution)) {
    distribution <- "lognormal"
  }
  distribution <- match.arg(distribution, several.ok = TRUE)

  ## Convert date_1 and date_2 to lists if they are vectors:
  if (!is.list(date_1)) date_1 <- list(date_1)
  if (!is.list(date_2)) date_2 <- list(date_2)

  ## Check for different length in date_1 and date_2:
  purrr::walk2(date_1, date_2, function(e1, e2) {
    if (length(e1) != length(e2)) {
      stop("Elements of 'date_1' and 'date_2' differ in lengths!")
    }
  })

  # Step 1: Parameter estimation using complete cases:
  par_list <- purrr::pmap(
    list(
      date_1,
      date_2,
      distribution
    ),
    dist_delay
  )

  # Step 2: Simulation of random numbers:
  sim_list <- purrr::map2(
    date_2,
    par_list,
    mcs_helper
  )

  ## Adjustment of operating times:
  time <- time - purrr::reduce(sim_list, `+`)

  # Prepare data_list which has to be converted to a tibble:
  if (purrr::is_null(status)) {
    data_list <- c(date_1, date_2, list(time, id))
  } else {
    # check for status:
    if (!is_status(status)) {
      stop("'status' must be numeric with elements 0 or 1!")
    }
    data_list <- c(date_1, date_2, list(time, status, id))
  }

  # Defining and setting names for output elements:
  ## lengths of lists sim_list, par_list, date_1, date_2 remains the same:
  if (length(sim_list) > 1) {
    sim_list_names <- paste0("sim_delay.", seq_along(sim_list))
    par_list_names <- paste0("delay_distribution.", seq_along(sim_list))
    data_list_names <- c(
      paste0("date_1.", seq_along(date_1)),
      paste0("date_2.", seq_along(date_2))
    )
  } else {
    sim_list_names <- "sim_delay"
    par_list_names <- "delay_distribution"
    data_list_names <- c("date_1", "date_2")
  }

  names(sim_list) <- sim_list_names
  names(par_list) <- par_list_names

  if (purrr::is_null(status)) {
    names(data_list) <- c(data_list_names, "time", "id")
    class_assign <- "wt_mcs_data"
  } else {
    names(data_list) <- c(data_list_names, "time", "status", "id")
    class_assign <- c("wt_mcs_data", "wt_reliability_data")
  }

  # Defining data_tbl with class "wt_mcs_data" and/or "wt_reliability_data":
  data_tbl <- tibble::as_tibble(data_list)
  class(data_tbl) <- c(class_assign, class(data_tbl))

  if (!purrr::is_null(status)) {
    attr(data_tbl, "characteristic") <- "time"
  }

  mcs_output <- list(
     data = data_tbl,
     sim_data = tibble::as_tibble(sim_list),
     model_estimation = par_list
  )

  return(mcs_output)
}



# helper function to generate MCS random numbers:
mcs_helper <- function(x, par_list) {

  # adjustment can only be done for units that have a x entry of NA! Otherwise
  # data would be complete and no simulation is needed.
  replacable <- is.na(x)

  # generate random numbers:
  if (par_list$distribution == "lognormal") {
    x_sim <- stats::rlnorm(
      length(x),
      par_list$coefficients[1],
      par_list$coefficients[2]
    )
  }

  if (par_list$distribution == "exponential") {
    x_sim <- stats::rexp(
      length(x),
      1 / par_list$coefficients[1]
    )
  }

  x_sim[!replacable] <- 0

  return(x_sim)
}



#' Parameter Estimation of the Delay in Registration Distribution
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \code{dist_delay_register()} is no longer under active development, switching
#' to \code{\link{dist_delay}} is recommended.
#'
#' @details
#' This function introduces a delay random variable by calculating the time
#' difference between the registration and production date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using maximum likelihood.
#'
#' @param date_prod A vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   Use \code{NA} for missing elements.
#' @param date_register A vector of class \code{character} or \code{Date}, in
#'   the format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   Use \code{NA} for missing elements.
#' @param distribution Supposed distribution of the random variable. Only
#'   \code{"lognormal"} is implemented.
#'
#' @return A named vector of estimated parameters for the specified
#'   distribution.
#'
#' @examples
#' date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
#'                           "2014-06-26", "2014-03-10", "2014-05-14",
#'                           "2014-05-06", "2014-03-07", "2014-03-09",
#'                           "2014-04-13", "2014-05-20", "2014-07-07",
#'                           "2014-01-27", "2014-01-30", "2014-03-17",
#'                           "2014-02-09", "2014-04-14", "2014-04-20",
#'                           "2014-03-13", "2014-02-23", "2014-04-03",
#'                           "2014-01-08", "2014-01-08")
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
#'                           NA, NA, "2014-06-16", NA, "2014-05-23",
#'                           "2014-05-09", "2014-05-31", NA, "2014-04-13",
#'                           NA, NA, "2014-03-12", NA, "2014-06-02",
#'                           NA, "2014-03-21", "2014-06-19", NA, NA)
#'
#' params_delay_regist  <- dist_delay_register(
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   distribution = "lognormal"
#' )
#'
#' @md
#'
#' @export
dist_delay_register <- function(date_prod,
                                date_register,
                                distribution = "lognormal"
) {
  deprecate_soft("2.0.0", "dist_delay_register()", "dist_delay()")

  distribution <- match.arg(distribution)

  # delay variable:
  t_regist <- as.numeric(
    difftime(
      time1 = as.Date(date_register, format = "%Y-%m-%d"),
      time2 = as.Date(date_prod, format = "%Y-%m-%d"),
      units = "days"
    )
  )

  # test for delays: all NA and smaller or equal to 0.
  # all NA:
  if (all(is.na(t_regist))) {
    stop("All differences are NA. No parameters can be estimated!")
  }
  # all smaller or equal to zero:
  if (all(t_regist <= 0, na.rm = TRUE)) {
    stop("All differences are smaller or equal to 0. No parameters can be estimated!")
  }
  # any smaller or equal to zero:
  if (!all(t_regist <= 0, na.rm = TRUE) && any(t_regist <= 0, na.rm = TRUE)) {
    warning("At least one of the time differences is smaller or equal to 0 and is",
            " ignored for the estimation step!")

    t_regist <- t_regist[t_regist > 0]
  }

  # sample size used for the computation of the population standard deviation.
  n <- length(!is.na(t_regist))
  ml_loc <- mean(log(t_regist), na.rm = TRUE)
  ml_sc <- stats::sd(log(t_regist), na.rm = TRUE) * (n - 1) / n

  estimates <- c(loc = ml_loc, sc = ml_sc)

  return(estimates)
}



#' Adjustment of Operating Times by Delays in Registration using a Monte Carlo
#' Approach
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \code{mcs_delay_register()} is no longer under active development, switching
#' to \code{\link{mcs_delay}} is recommended.
#'
#' @details
#' In general the amount of information about units in the field, that have not
#' failed yet, are rare. For example it is common that a supplier, who provides
#' parts to the automotive industry does not know when a vehicle was put in
#' service and therefore does not know the exact operating time of the supplied
#' parts. This function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of registering
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data (see \code{\link{dist_delay_register}}).
#'
#' @inheritParams dist_delay_register
#' @param time A numeric vector of operating times.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution Supposed distribution of the random variable. Only
#'   \code{"lognormal"} is implemented.
#' @param details A logical. If \code{FALSE} the output consists of a vector with
#'   corrected operating times for the censored units and the input operating
#'   times for the failed units. If \code{TRUE} the output consists of a detailed
#'   list, i.e the same vector as described before, simulated random numbers and
#'   estimated distribution parameters.
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following elements:
#'   \itemize{
#'   \item \code{time} : Numeric vector of corrected operating times for the
#'     censored observations and input operating times for failed units.
#'   \item \code{x_sim} : Simulated random numbers of specified distribution with
#'     estimated parameters. The length of \code{x_sim} is equal to the number of
#'     censored observations.
#'   \item \code{coefficients} : Estimated coefficients of supposed
#'     distribution.
#'   }
#'
#' @examples
#' date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
#'                           "2014-06-26", "2014-03-10", "2014-05-14",
#'                           "2014-05-06", "2014-03-07", "2014-03-09",
#'                           "2014-04-13", "2014-05-20", "2014-07-07",
#'                           "2014-01-27", "2014-01-30", "2014-03-17",
#'                           "2014-02-09", "2014-04-14", "2014-04-20",
#'                           "2014-03-13", "2014-02-23", "2014-04-03",
#'                           "2014-01-08", "2014-01-08")
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
#'                           NA, NA, "2014-06-16", NA, "2014-05-23",
#'                           "2014-05-09", "2014-05-31", NA, "2014-04-13",
#'                           NA, NA, "2014-03-12", NA, "2014-06-02",
#'                           NA, "2014-03-21", "2014-06-19", NA, NA)
#'
#' op_time <- rep(1000, length(date_of_production))
#' status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delay_register(
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = FALSE
#' )
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_register(
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = TRUE
#' )
#'
#' @md
#'
#' @export
mcs_delay_register <- function(date_prod,
                               date_register,
                               time,
                               status,
                               distribution = "lognormal",
                               details = FALSE
) {
  deprecate_soft("2.0.0", "mcs_delay_register()", "mcs_delay()")

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(is.na(date_register))
  if (any(!stats::complete.cases(date_prod) | !stats::complete.cases(date_register))) {
    prod_date <- date_prod[(stats::complete.cases(date_prod) &
        stats::complete.cases(date_register))]
    register_date <- date_register[(stats::complete.cases(date_prod) &
        stats::complete.cases(date_register))]
  } else {
    prod_date <- date_prod
    register_date <- date_register
  }

  if (distribution == "lognormal") {
    params <- dist_delay_register(date_prod = prod_date,
                                  date_register = register_date,
                                  distribution = "lognormal")

    x_sim <- stats::rlnorm(n = n_rand, meanlog = params[[1]], sdlog = params[[2]])
  } else {
    stop("No valid distribution!")
  }

  time[is.na(date_register)] <- time[is.na(date_register)] - x_sim

  if (details == FALSE) {
    output <- time
  } else {
    output <- list(time = time, x_sim = x_sim, coefficients = params)
  }
  return(output)
}



#' Parameter Estimation of the Delay in Report Distribution
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \code{dist_delay_report()} is no longer under active development, switching
#' to \code{\link{dist_delay}} is recommended.
#'
#' @details
#' This function introduces a delay random variable by calculating the time
#' difference between the report and repair date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using maximum likelihood.
#'
#' @inheritParams dist_delay_register
#'
#' @param date_repair a vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the date of repair of a failed unit.
#'   Use \code{NA} for missing elements.
#' @param date_report a vector of class \code{character} or \code{Date}, in the
#'   format "yyyy-mm-dd", indicating the date of report of a failed unit.
#'   Use \code{NA} for missing elements.
#'
#' @return A named vector of estimated parameters for the specified
#'   distribution.
#'
#' @examples
#' date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                     NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                     "2015-06-12", NA, "2015-05-04", NA, NA,
#'                     "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
#'                     "2015-11-26", NA, NA)
#'
#' date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
#'                     NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
#'                     "2015-07-11", NA, "2015-08-14", NA, NA,
#'                     "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
#'                     "2015-12-02", NA, NA)
#'
#' params_delay_report  <- dist_delay_report(
#'   date_repair = date_of_repair,
#'   date_report = date_of_report,
#'   distribution = "lognormal"
#' )
#'
#' @md
#'
#' @export
dist_delay_report <- function(date_repair,
                              date_report,
                              distribution = "lognormal"
) {
  deprecate_soft("2.0.0", "dist_delay_report()", "dist_delay()")

  distribution <- match.arg(distribution)

  # delay variable:
  t_report <- as.numeric(
    difftime(
      time1 = as.Date(date_report, format = "%Y-%m-%d"),
      time2 = as.Date(date_repair, format = "%Y-%m-%d"),
      units = "days"
    )
  )

  # test for delays: all NA and smaller or equal to 0.
  # all NA:
  if (all(is.na(t_report))) {
    stop("All differences are NA. No parameters can be estimated!")
  }
  # all smaller or equal to zero:
  if (all(t_report <= 0, na.rm = TRUE)) {
    stop("All differences are smaller or equal to 0. No parameters can be estimated!")
  }
  # any smaller or equal to zero:
  if (!all(t_report <= 0, na.rm = TRUE) && any(t_report <= 0, na.rm = TRUE)) {
    warning("At least one of the time differences is smaller or equal to 0 and is",
            " ignored for the estimation step!")

    t_report <- t_report[t_report > 0]
  }

  # sample size used for the computation of the population standard deviation.
  n <- length(!is.na(t_report))
  ml_loc <- mean(log(t_report), na.rm = TRUE)
  ml_sc <- stats::sd(log(t_report), na.rm = TRUE) * (n - 1) / n

  estimates <- c(loc = ml_loc, sc = ml_sc)

  return(estimates)
}



#' Adjustment of Operating Times by Delays in Report using a Monte Carlo Approach
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \code{mcs_delay_report()} is no longer under active development, switching
#' to \code{\link{mcs_delay}} is recommended.
#'
#' @details
#' The delay in report describes the time between the occurrence of a damage and
#' the registration in the warranty database. For a given date where the analysis
#' is made there could be units which had a failure but are not registered in the
#' database and therefore treated as censored units. To overcome this problem
#' this function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of reporting
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data, i.e. failed items (see \code{\link{dist_delay_report}}).
#'
#' @inheritParams dist_delay_report
#' @inheritParams mcs_delay_register
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following elements:
#'   \itemize{
#'   \item \code{time} : Numeric vector of corrected operating times for the
#'     censored observations and input operating times for failed units.
#'   \item \code{x_sim} : Simulated random numbers of specified distribution with
#'     estimated parameters. The length of \code{x_sim} is equal to the number of
#'     censored observations.
#'   \item \code{coefficients} : Estimated coefficients of supposed
#'     distribution.
#'   }
#'
#' @examples
#' date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                    NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                     "2015-06-12", NA, "2015-05-04", NA, NA,
#'                     "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
#'                     "2015-11-26", NA, NA)
#'
#' date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
#'                     NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
#'                     "2015-07-11", NA, "2015-08-14", NA, NA,
#'                     "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
#'                     "2015-12-02", NA, NA)
#'
#' op_time <- rep(1000, length(date_of_repair))
#' status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delay_report(
#'   date_repair = date_of_repair,
#'   date_report = date_of_report,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = FALSE
#' )
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_report(
#'   date_repair = date_of_repair,
#'   date_report = date_of_report,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = TRUE
#' )
#'
#' @md
#'
#' @export
mcs_delay_report <- function(date_repair,
                             date_report,
                             time,
                             status,
                             distribution = "lognormal",
                             details = FALSE
) {
  deprecate_soft("2.0.0", "mcs_delay_report()", "mcs_delay()")

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(status == 0)

  if (any(!stats::complete.cases(date_repair) | !stats::complete.cases(date_report))) {
    repair_date <- date_repair[(stats::complete.cases(date_repair) &
                                stats::complete.cases(date_report))]
    report_date <- date_report[(stats::complete.cases(date_repair) &
                                stats::complete.cases(date_report))]
  } else {
    repair_date <- date_repair
    report_date <- date_report
  }

  if (distribution == "lognormal") {
    params <- dist_delay_report(date_repair = repair_date,
                                date_report = report_date,
                                distribution = "lognormal")

    x_sim <- stats::rlnorm(n = n_rand, meanlog = params[[1]], sdlog = params[[2]])
  } else {
    stop("No valid distribution!")
  }

  time[status == 0] <- time[status == 0] - x_sim

  if (details == FALSE) {
    output <- time
  } else {
    output <- list(time = time, x_sim = x_sim, coefficients = params)
  }
  return(output)
}



#' Adjustment of Operating Times by Delays using a Monte Carlo Approach
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' \code{mcs_delays()} is no longer under active development, switching
#' to \code{\link{mcs_delay}} is recommended.
#'
#' @details
#' This function is a wrapper that combines both, the
#' \code{\link{mcs_delay_register}} and \code{\link{mcs_delay_report}} function
#' for adjusting the operation times of censored units.
#'
#' @inheritParams mcs_delay_register
#' @inheritParams dist_delay_report
#'
#' @return A numerical vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following elements:
#'   \itemize{
#'   \item \code{time} : Numerical vector of corrected operating times for the
#'     censored observations and input operating times for failed units.
#'   \item \code{x_sim_regist} : Simulated random numbers of specified
#'     distribution with estimated parameters for delay in registration.
#'     The length of \code{x_sim_regist} is equal to the number of censored
#'     observations.
#'   \item \code{x_sim_report} : Simulated random numbers of specified
#'     distribution with estimated parameters for delay in report.
#'     The length of \code{x_sim_report} is equal to the number of censored
#'     observations.
#'   \item \code{coefficients_regist} : Estimated coefficients of supposed
#'     distribution for delay in registration.
#'   \item \code{coefficients_report} : Estimated coefficients of supposed
#'     distribution for delay in report
#'   }
#'
#' @examples
#' date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
#'                           "2014-06-26", "2014-03-10", "2014-05-14",
#'                           "2014-05-06", "2014-03-07", "2014-03-09",
#'                           "2014-04-13", "2014-05-20", "2014-07-07",
#'                           "2014-01-27", "2014-01-30", "2014-03-17",
#'                           "2014-02-09", "2014-04-14", "2014-04-20",
#'                           "2014-03-13", "2014-02-23", "2014-04-03",
#'                           "2014-01-08", "2014-01-08")
#' date_of_registration <- c("2014-08-17", "2014-03-29", "2014-12-06",
#'                           "2014-09-09", "2014-05-14", "2014-07-01",
#'                           "2014-06-16", "2014-04-03", "2014-05-23",
#'                           "2014-05-09", "2014-05-31", "2014-08-12",
#'                           "2014-04-13", "2014-02-15", "2014-07-07",
#'                           "2014-03-12", "2014-05-27", "2014-06-02",
#'                           "2014-05-20", "2014-03-21", "2014-06-19",
#'                           "2014-02-12", "2014-03-27")
#' date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                    NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                     "2015-06-12", NA, "2015-05-04", NA, NA,
#'                     "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
#'                     "2015-11-26", NA, NA)
#'
#' date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
#'                     NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
#'                     "2015-07-11", NA, "2015-08-14", NA, NA,
#'                     "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
#'                     "2015-12-02", NA, NA)
#'
#' op_time <- rep(1000, length(date_of_repair))
#' status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delays(
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   date_repair = date_of_repair,
#'   date_report = date_of_report,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = FALSE
#' )
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delays(
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   date_repair = date_of_repair,
#'   date_report = date_of_report,
#'   time = op_time,
#'   status = status,
#'   distribution = "lognormal",
#'   details = TRUE
#' )
#'
#' @md
#'
#' @export
mcs_delays <- function(date_prod,
                       date_register,
                       date_repair,
                       date_report,
                       time,
                       status,
                       distribution = "lognormal",
                       details = FALSE
) {
  deprecate_soft("2.0.0", "mcs_delays()", "mcs_delay()")

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.

  n_rand_regist <- sum(is.na(date_register))
  n_rand_report <- sum(status == 0)

  if (any(!stats::complete.cases(date_prod) | !stats::complete.cases(date_register))) {
    prod_date <- date_prod[(stats::complete.cases(date_prod) &
                           stats::complete.cases(date_register))]
    register_date <- date_register[(stats::complete.cases(date_prod) &
                                   stats::complete.cases(date_register))]
  } else {
    prod_date <- date_prod
    register_date <- date_register
  }

  if (any(!stats::complete.cases(date_repair) | !stats::complete.cases(date_report))) {
    repair_date <- date_repair[(stats::complete.cases(date_repair) &
                               stats::complete.cases(date_report))]
    report_date <- date_report[(stats::complete.cases(date_repair) &
                               stats::complete.cases(date_report))]
  } else {
    repair_date <- date_repair
    report_date <- date_report
  }

  if (distribution == "lognormal") {
    params_regist <- dist_delay_register(date_prod = prod_date,
                                         date_register = register_date,
                                         distribution = "lognormal")
    params_report <- dist_delay_report(date_repair = repair_date,
                                       date_report = report_date,
                                       distribution = "lognormal")

    x_sim_regist <- stats::rlnorm(n = n_rand_regist, meanlog = params_regist[[1]],
                           sdlog = params_regist[[2]])
    x_sim_report <- stats::rlnorm(n = n_rand_report, meanlog = params_report[[1]],
                           sdlog = params_report[[2]])
  } else {
    stop("No valid distribution!")
  }

  time[is.na(date_register)] <- time[is.na(date_register)] - x_sim_regist
  time[status == 0] <- time[status == 0] - x_sim_report

  if (details == FALSE) {
    output <- time
  } else {
    output <- list(time = time, x_sim_regist = x_sim_regist,
                   x_sim_report = x_sim_report,
                   coefficients_regist = params_regist,
                   coefficients_report = params_report)
  }
  return(output)
}
