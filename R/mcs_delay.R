#' Adjustment of Operating Times by Delays using a Monte Carlo Approach
#'
#' @description
#'
#' In general, the amount of available information about units in the field is very
#' different. During the warranty period, there are only a few cases with complete
#' data (mainly *failed units*) but lots of cases with incomplete data (usually
#' *censored units*). As a result, the operating time of units with incomplete
#' information is often inaccurate and must be adjusted by delays.
#'
#' This function reduces the operating times of incomplete observations by simulated
#' delays (in days). A unit is considered as incomplete if the later of the
#' related dates is unknown. See 'Details' for some practical examples.
#'
#' Random delay numbers are drawn from the distribution determined by complete cases
#' (described in 'Details' of [dist_delay]).
#'
#' @details
#' In field data analysis time-dependent characteristics (e.g. *time in service*)
#' are often imprecisely recorded. These inaccuracies are caused by unconsidered delays.
#'
#' For a better understanding of the MCS application in the context of field data,
#' two cases are described below.
#'
#' * **Delay in registration**: It is common that a supplier, which provides
#'   parts to the manufacturing industry does not know when the unit, in which
#'   its parts are installed, were put in service (due to unknown registration or
#'   sales date (`date_2`)). Without taking the described delay into account, the
#'   time in service of the failed units would be the difference between the
#'   repair date and the production date (`date_1`) and for intact units the
#'   difference between the present date and the production date. But the real
#'   operating times are (much) shorter, since the stress on the components have
#'   not started until the whole systems were put in service. Hence, units with
#'   incomplete data (missing `date_2`) must be reduced by the delays.
#' * **Delay in report**:: Authorized repairers often do not immediately
#'   notify the manufacturer or OEM of repairs that were made during the warranty
#'   period, but instead pass the information about these repairs in collected
#'   forms e.g. weekly, monthly or quarterly. The resulting time difference between
#'   the reporting (`date_2`) of the repair in the guarantee database and the
#'   actual repair date (`date_1`), which is often assumed to be the failure
#'   date, is called the reporting delay. For a given date where the analysis
#'   is made there could be units which had a failure but the failure isn't
#'   reported and therefore they are treated as censored units. In order to take
#'   this into account and according to the principle of equal opportunities, the
#'   lifetime of units with missing report date (`date_2[i] = NA`) is reduced by
#'   simulated reporting delays.
#'
#' @inheritParams dist_delay
#'
#' @return A list with class `wt_mcs_delay` containing the following elements:
#'
#' * `data` : A `tibble` returned by [mcs_delay_data] where two modifications
#'   has been made:
#'
#'   * If the column `status` exists, the `tibble` has additional classes
#'     `wt_mcs_data` and `wt_reliability_data`. Otherwise, the `tibble` only has
#'     the additional class `wt_mcs_data` (which is not supported by [estimate_cdf]).
#'   * The column `time` is renamed to `x` (to be in accordance with
#'     [reliability_data]) and contains the adjusted operating times for incomplete
#'     observations and input operating times for the complete observations.
#'
#' * `sim_data` : A `tibble` with column `sim_delay` that holds the simulated
#'   delay-specific numbers for incomplete cases and `0` for complete cases.
#'   If more than one delay was considered multiple columns with names `sim_delay_1`,
#'   `sim_delay_2`, ..., `sim_delay_i` and corresponding delay-specific random
#'   numbers are presented.
#' * `model_estimation` : A list returned by [dist_delay].
#'
#' @references Verband der Automobilindustrie e.V. (VDA); Qualitätsmanagement in
#'   der Automobilindustrie. Zuverlässigkeitssicherung bei Automobilherstellern
#'   und Lieferanten. Zuverlässigkeits-Methoden und -Hilfsmittel.; 4th Edition, 2016,
#'   ISSN:0943-9412
#'
#' @seealso [dist_delay] for the determination of a parametric delay distribution
#' and [estimate_cdf] for the estimation of failure probabilities.
#'
#' @examples
#' # MCS data preparation:
#' ## Data for delay in registration:
#' mcs_tbl_1 <- mcs_delay_data(
#'   field_data,
#'   date_1 = production_date,
#'   date_2 = registration_date,
#'   time = dis,
#'   status = status,
#'   id = vin
#' )
#'
#' ## Data for delay in report:
#' mcs_tbl_2 <- mcs_delay_data(
#'   field_data,
#'   date_1 = repair_date,
#'   date_2 = report_date,
#'   time = dis,
#'   status = status,
#'   id = vin
#' )
#'
#' ## Data for both delays:
#' mcs_tbl_both <- mcs_delay_data(
#'   field_data,
#'   date_1 = c(production_date, repair_date),
#'   date_2 = c(registration_date, report_date),
#'   time = dis,
#'   status = status,
#'   id = vin
#' )
#'
#' # Example 1 - MCS for delay in registration:
#' mcs_regist <- mcs_delay(
#'   x = mcs_tbl_1,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for delay in report:
#' mcs_report <- mcs_delay(
#'   x = mcs_tbl_2,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - Reproducibility of random numbers:
#' set.seed(1234)
#' mcs_report_reproduce <- mcs_delay(
#'   x = mcs_tbl_2,
#'   distribution = "exponential"
#' )
#'
#' # Example 4 - MCS for delays in registration and report with same distribution:
#' mcs_delays <- mcs_delay(
#'   x = mcs_tbl_both,
#'   distribution = "lognormal"
#' )
#'
#' # Example 5 - MCS for delays in registration and report with different distributions:
#' ## Assuming lognormal registration and exponential reporting delays.
#' mcs_delays_2 <- mcs_delay(
#'   x = mcs_tbl_both,
#'   distribution = c("lognormal", "exponential")
#' )
#'
#' @md
#'
#' @export
mcs_delay <- function(...) {
  UseMethod("mcs_delay")
}



#' @rdname mcs_delay
#'
#' @export
mcs_delay.wt_mcs_delay_data <- function(
                                    ...,
                                    x,
                                    distribution = c("lognormal", "exponential")
) {

  # Check that '...' argument is not used:
  check_dots(...)

  # Extract 'mcs_start_dates', 'mcs_end_dates' and 'time 'columns as list:
  date_1_names <- attr(x, "mcs_start_dates")
  date_2_names <- attr(x, "mcs_end_dates")

  date_1 <- dplyr::select(x, {{date_1_names}})
  date_2 <- dplyr::select(x, {{date_2_names}})
  time <- x$time


  mcs_delay_(
    data = x,
    date_1 = date_1,
    date_2 = date_2,
    time = time,
    distribution = distribution
  )
}



#' Adjustment of Operating Times by Delays using a Monte Carlo Approach
#'
#' @inherit mcs_delay description details return references seealso
#'
#' @inheritParams dist_delay.default
#' @inheritParams mcs_delay_data
#'
#' @examples
#' # Example 1 - MCS for delay in registration:
#' mcs_regist <- mcs_delay(
#'   date_1 = field_data$production_date,
#'   date_2 = field_data$registration_date,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for delay in report:
#' mcs_report <- mcs_delay(
#'   date_1 = field_data$repair_date,
#'   date_2 = field_data$report_date,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - Reproducibility of random numbers:
#' set.seed(1234)
#' mcs_report_reproduce <- mcs_delay(
#'   date_1 = field_data$repair_date,
#'   date_2 = field_data$report_date,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   distribution = "exponential"
#' )
#'
#' # Example 4 - MCS for delays in registration and report with same distribution:
#' mcs_delays <- mcs_delay(
#'   date_1 = list(field_data$production_date, field_data$repair_date),
#'   date_2 = list(field_data$registration_date, field_data$report_date),
#'   time = field_data$dis,
#'   status = field_data$status,
#'   distribution = "lognormal"
#' )
#'
#' # Example 5 - MCS for delays in registration and report with different distributions:
#' ## Assuming lognormal registration and exponential reporting delays.
#' mcs_delays_2 <- mcs_delay(
#'   date_1 = list(field_data$production_date, field_data$repair_date),
#'   date_2 = list(field_data$registration_date, field_data$report_date),
#'   time = field_data$dis,
#'   status = field_data$status,
#'   distribution = c("lognormal", "exponential")
#' )
#'
#' @md
#'
#' @export
mcs_delay.default <- function(...,
                              date_1,
                              date_2,
                              time,
                              status = NULL,
                              id = paste0("ID", seq_len(length(time))),
                              distribution = c("lognormal", "exponential")
) {

  # Checks:
  ## Check that '...' argument is not used:
  check_dots(...)

  ## Convert date_1 and date_2 to lists if they are vectors:
  if (!is.list(date_1)) date_1 <- list(date_1)
  if (!is.list(date_2)) date_2 <- list(date_2)

  mcs_delay_(
    date_1 = date_1,
    date_2 = date_2,
    time = time,
    status = status,
    id = id,
    distribution = distribution
  )
}



# Helper function that performs MCS for delays:
mcs_delay_ <- function(data = NULL,
                       date_1, # list
                       date_2, # list
                       time, # vector
                       status = NULL,
                       id = NULL,
                       distribution
) {

  # Step 1: Parameter estimation using complete cases:
  ## Several checks (distributional and length) are made in dist_delay.default:
  par_list <- dist_delay.default(
    date_1 = date_1,
    date_2 = date_2,
    distribution = distribution
  )

  ## New check is needed since dist_delay.default is used in favour of dist_delay_:
  if (!inherits(par_list, "wt_delay_estimation_list")) {
    par_list <- list(par_list)
  }

  # Step 2: Simulation of random numbers:
  sim_list <- purrr::map2(
    date_2,
    par_list, # list with class
    mcs_helper
  )

  ## Adjustment of operating times:
  times <- time - purrr::reduce(sim_list, `+`)

  # Step 3: Create output:
  ## Create MCS_Delay_Data and renaming 'time' to 'x':
  ## vector-based:
  if (purrr::is_null(data)) {
    data_tbl <- mcs_delay_data(
      date_1 = date_1,
      date_2 = date_2,
      time = times,
      status = status,
      id = id
    )
  } else {
    ## data-based: only 'time' must be updated!
    data_tbl <- dplyr::mutate(data, time = times)
  }

  data_tbl <- dplyr::rename(data_tbl, x = "time")

  ## Set class and attribute w.r.t status; remove class "wt_mcs_delay_data":
  if ("status" %in% names(data_tbl)) {
    class(data_tbl) <- c("wt_reliability_data", "wt_mcs_data", class(data_tbl)[-1])
    attr(data_tbl, "characteristic") <- "time"

  } else {
    class(data_tbl) <- c("wt_mcs_data", class(data_tbl)[-1])
  }

  # Remove attribute "mcs_start_dates" and "mcs_end_dates":
  attr(data_tbl, "mcs_start_dates") <- NULL
  attr(data_tbl, "mcs_end_dates") <- NULL

  ## Set names of sim_list w.r.t number of considered delays:
  if (length(sim_list) > 1) {
    names(sim_list) <- paste0("sim_delay_", seq_along(sim_list))
  } else {
    names(sim_list) <- "sim_delay"
    par_list <- par_list[[1]]
  }

  mcs_output <- list(
    data = data_tbl,
    sim_data = tibble::as_tibble(sim_list),
    model_estimation = list(
      delay_distribution = par_list
    )
  )

  class(mcs_output) <- c("wt_mcs_delay", class(mcs_output))

  mcs_output
}



# Helper function to generate MCS random numbers:
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

  x_sim
}



#' Adjustment of Operating Times by Delays in Registration using a Monte Carlo
#' Approach
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `mcs_delay_register()` is no longer under active development, switching
#' to [mcs_delay] is recommended.
#'
#' @details
#' In general the amount of information about units in the field, that have not
#' failed yet, are rare. For example it is common that a supplier, who provides
#' parts to the automotive industry does not know when a vehicle was put in
#' service and therefore does not know the exact operating time of the supplied
#' parts. This function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of registering
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data (see [dist_delay_register]).
#'
#' @inheritParams dist_delay_register
#' @param time A numeric vector of operating times.
#' @param status A vector of binary data (0 or 1) indicating whether unit *i* is
#' a right censored observation (= 0) or a failure (= 1).
#' @param distribution Supposed distribution of the random variable. Only
#'   `"lognormal"` is implemented.
#' @param details A logical. If `FALSE` the output consists of a vector with
#'   corrected operating times for the censored units and the input operating
#'   times for the failed units. If `TRUE` the output consists of a detailed
#'   list, i.e the same vector as described before, simulated random numbers and
#'   estimated distribution parameters.
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if `details = FALSE`.
#'   If `details = TRUE` the output is a list which consists of the following elements:
#'
#' * `time` : Numeric vector of corrected operating times for the censored
#'   observations and input operating times for failed units.
#' * `x_sim` : Simulated random numbers of specified distribution with estimated
#'   parameters. The length of `x_sim` is equal to the number of censored observations.
#' * `coefficients` : Estimated coefficients of supposed distribution.
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
    stop("No valid distribution!", call. = FALSE)
  }

  time[is.na(date_register)] <- time[is.na(date_register)] - x_sim

  if (details == FALSE) {
    output <- time
  } else {
    output <- list(time = time, x_sim = x_sim, coefficients = params)
  }
  return(output)
}



#' Adjustment of Operating Times by Delays in Report using a Monte Carlo Approach
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `mcs_delay_report()` is no longer under active development, switching to
#' [mcs_delay] is recommended.
#'
#' @details
#' The delay in report describes the time between the occurrence of a damage and
#' the registration in the warranty database. For a given date where the analysis
#' is made there could be units which had a failure but are not registered in the
#' database and therefore treated as censored units. To overcome this problem
#' this function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of reporting
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data, i.e. failed items (see [dist_delay_report]).
#'
#' @inheritParams dist_delay_report
#' @inheritParams mcs_delay_register
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if `details = FALSE`.
#'   If `details = TRUE` the output is a list which consists of the following
#'   elements:
#'
#' * `time` : Numeric vector of corrected operating times for the censored
#'   observations and input operating times for failed units.
#' * `x_sim` : Simulated random numbers of specified distribution with
#'     estimated parameters. The length of `x_sim` is equal to the number of
#'     censored observations.
#' * `coefficients` : Estimated coefficients of supposed distribution.
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
    stop("No valid distribution!", call. = FALSE)
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
#' `mcs_delays()` is no longer under active development, switching to [mcs_delay]
#' is recommended.
#'
#' @details
#' This function is a wrapper that combines both, [mcs_delay_register] and
#' [mcs_delay_report] functions for the adjustment of operating times of censored units.
#'
#' @inheritParams mcs_delay_register
#' @inheritParams dist_delay_report
#'
#' @return A numerical vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   `details = FALSE`. If `details = TRUE` the output is a list which
#'   consists of the following elements:
#'
#' * `time` : A numeric vector of corrected operating times for the censored
#'   observations and input operating times for failed units.
#' * `x_sim_regist` : Simulated random numbers of specified distribution with
#'   estimated parameters for delay in registration. The length of `x_sim_regist`
#'   is equal to the number of censored observations.
#' * `x_sim_report` : Simulated random numbers of specified distribution with
#'   estimated parameters for delay in report. The length of `x_sim_report` is
#'   equal to the number of censored observations.
#' * `coefficients_regist` : Estimated coefficients of supposed distribution for
#'   delay in registration.
#' * `coefficients_report` : Estimated coefficients of supposed distribution for
#'   delay in report
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
    stop("No valid distribution!", call. = FALSE)
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
