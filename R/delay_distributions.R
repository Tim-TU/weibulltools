#' Parameter estimation of the delay in registration distribution
#'
#' This function introduces a delay random variable by calculating the time
#' difference between the registration and production date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using MLE.
#'
#' @param date_prod a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   If no date is available use \code{NA}.
#' @param date_register a vector of class \code{"character"} or \code{"Date"}, in
#'   the format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   If no date is available use \code{NA}.
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#'
#' @return A named vector of estimated parameters for the specified
#'   distribution.
#' @export
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
#'                                     date_prod = date_of_production,
#'                                     date_register = date_of_registration,
#'                                     distribution = "lognormal")

dist_delay_register <- function(date_prod, date_register,
                                distribution = "lognormal") {

  if (class(date_prod) != "Date" || class(date_register) != "Date") {
    prod_date <- as.Date(date_prod, format = "%Y-%m-%d")
    register_date <- as.Date(date_register, format = "%Y-%m-%d")
    t_regist <- as.numeric(difftime(register_date, prod_date,
                                    units = "days"))
  } else {
    t_regist <- as.numeric(difftime(date_register, date_prod,
                                    units = "days"))
  }

  if (distribution == "lognormal") {
    logmu_regist <- mean(log(t_regist[t_regist > 0]), na.rm = TRUE)
    logsd_regist <- stats::sd(log(t_regist[t_regist > 0]), na.rm = TRUE)

    estimates <- c(logmu_regist, logsd_regist)
    names(estimates) <- c("meanlog_register", "sdlog_register")
  } else {
    stop("No valid distribution!")
  }

  return(estimates)
}


#' Adjustment of operating times by delays in registration using a Monte Carlo
#' approach
#'
#' In general the amount of information about units in the field, that have not
#' failed yet, are rare. For example it is common that a supplier, who provides
#' parts to the automotive industry does not know when a vehicle was put in
#' service and therefore does not know the exact operating time of the supplied
#' parts. This function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of registering
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data (see \code{\link{dist_delay_register}}).
#'
#' @param date_prod a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   If no date is available use \code{NA}.
#' @param date_register a vector of class \code{"character"} or \code{"Date"}, in
#'   the format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   If no date is available use \code{NA}.
#' @param x a numeric vector of operating times.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#' @param seed if \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details a logical variable, where the default value is \code{FALSE}.
#'   If \code{FALSE} the output consists of a vector with corrected operating
#'   times for the censored units and the input operating times for the
#'   failed units. If \code{TRUE} the output consists of a detailed list, i.e
#'   the same vector as described before, simulated random numbers, estimated
#'   distribution parameters and a seed for reproducibility.
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following entries:
#'   \itemize{
#'   \item \code{time} : Numeric vector of corrected operating times for the
#'     censored observations and input operating times for failed units.
#'   \item \code{x_sim} : Simulated random numbers of specified distribution with
#'     estimated parameters. The length of \code{x_sim} is equal to the number of
#'     censored observations.
#'   \item \code{coefficients} : Estimated coefficients of supposed
#'     distribution.
#'   \item \code{int_seed} : Integer seed number for reproducibility.}
#'
#' @export
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
#' state <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delay_register(date_prod = date_of_production,
#'                                   date_register = date_of_registration,
#'                                   x = op_time,
#'                                   event = state,
#'                                   distribution = "lognormal",
#'                                   seed = NULL,
#'                                   details = FALSE)
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_register(date_prod = date_of_production,
#'                                   date_register = date_of_registration,
#'                                   x = op_time,
#'                                   event = state,
#'                                   distribution = "lognormal",
#'                                   seed = NULL,
#'                                   details = TRUE)

mcs_delay_register <- function(date_prod, date_register, x, event,
                               distribution = "lognormal", seed = NULL,
                               details = FALSE) {

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(is.na(date_register))

  if (!stats::complete.cases(date_prod) || !stats::complete.cases(date_register)) {
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

  x[is.na(date_register)] <- x[is.na(date_register)] - x_sim

  if (details == FALSE) {
    output <- x
  } else {
    output <- list(time = x, x_sim = x_sim, coefficients = params,
      int_seed = int_seed)
  }
  return(output)
}


#' Parameter estimation of the delay in report distribution
#'
#' This function introduces a delay random variable by calculating the time
#' difference between the report and repair date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using MLE.
#'
#' @param date_repair a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of repair of a failed unit.
#'   If no date is available use \code{NA}.
#' @param date_report a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of report of a failed unit.
#'   If no date is available use \code{NA}.
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#'
#' @return A named vector of estimated parameters for the specified
#'   distribution.
#' @export
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
#' params_delay_report  <- dist_delay_report(date_repair = date_of_repair,
#'                                             date_report = date_of_report,
#'                                             distribution = "lognormal")

dist_delay_report <- function(date_repair, date_report,
                              distribution = "lognormal") {

  if (class(date_repair) != "Date" || class(date_report) != "Date") {
    repair_date <- as.Date(date_repair, format = "%Y-%m-%d")
    report_date <- as.Date(date_report, format = "%Y-%m-%d")
    t_report <- as.numeric(difftime(report_date, repair_date,
                                    units = "days"))
  } else {
    t_report <- as.numeric(difftime(date_report, date_repair,
                                    units = "days"))
  }

  if (distribution == "lognormal") {
    logmu_report <- mean(log(t_report[t_report > 0]), na.rm = TRUE)
    logsd_report <- stats::sd(log(t_report[t_report > 0]), na.rm = TRUE)

    estimates <- c(logmu_report, logsd_report)
    names(estimates) <- c("meanlog_report", "sdlog_report")
  } else {
    stop("No valid distribution!")
  }

  return(estimates)
}


#' Adjustment of operating times by delays in report using a Monte Carlo approach
#'
#' The delay in report describes the time between the occurence of a damage and
#' the registration in the warranty database. For a given date where the analysis
#' is made there could be units which had a failure but are not registered in the
#' database and therefore treated as censored units. To overcome this problem
#' this function uses a Monte Carlo approach for simulating the operating
#' times of (multiple) right censored observations, taking account of reporting
#' delays. The simulation is based on the distribution of operating times that were
#' calculated from complete data, i.e. failed items (see \code{\link{dist_delay_report}}).
#'
#' @param date_repair a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of repair of a failed unit.
#'   If no date is available use \code{NA}.
#' @param date_report a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of report of a failed unit.
#'   If no date is available use \code{NA}.
#' @param x a numeric vector of operating times.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#' @param seed if \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details a logical variable, where the default value is \code{FALSE}.
#'   If \code{FALSE} the output consists of a vector with corrected operating
#'   times for the censored units and the input operating times for the
#'   failed units. If \code{TRUE} the output consists of a detailed list, i.e
#'   the same vector as described before, simulated random numbers, estimated
#'   distribution parameters and a seed for reproducibility.
#'
#' @return A numeric vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following entries:
#'   \itemize{
#'   \item \code{time} : Numeric vector of corrected operating times for the
#'     censored observations and input operating times for failed units.
#'   \item \code{x_sim} : Simulated random numbers of specified distribution with
#'     estimated parameters. The length of \code{x_sim} is equal to the number of
#'     censored observations.
#'   \item \code{coefficients} : Estimated coefficients of supposed
#'     distribution.
#'   \item \code{int_seed} : Integer seed number for reproducibility.}
#'
#' @export
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
#' state <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delay_report(date_repair = date_of_repair,
#'                                 date_report = date_of_report,
#'                                 x = op_time,
#'                                 event = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = FALSE)
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_report(date_repair = date_of_repair,
#'                                 date_report = date_of_report,
#'                                 x = op_time,
#'                                 event = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = TRUE)

mcs_delay_report <- function(date_repair, date_report, x, event,
                             distribution = "lognormal", details = FALSE,
                             seed = NULL) {

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(event == 0)

  if (!stats::complete.cases(date_repair) || !stats::complete.cases(date_report)) {
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

  x[event == 0] <- x[event == 0] - x_sim

  if (details == FALSE) {
    output <- x
  } else {
    output <- list(time = x, x_sim = x_sim, coefficients = params,
      int_seed = int_seed)
  }
  return(output)
}


#' Adjustment of operating times by delays using Monte Carlo method
#'
#' This function is a wrapper that combines both, the
#' \code{\link{mcs_delay_register}} and \code{\link{mcs_delay_report}} function
#' for adjusting the operation times of censored units.
#'
#' @param date_prod a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   If no date is available use \code{NA}.
#' @param date_register a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   If no date is available use \code{NA}.
#' @param date_repair a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of repair of a failed unit.
#'   If no date is available use \code{NA}.
#' @param date_report a vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of report of a failed unit.
#'   If no date is available use \code{NA}.
#' @param x a numeric vector of operating times.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#' @param seed if \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details a logical variable, where the default value is \code{FALSE}.
#'   If \code{FALSE} the output consists of a vector with corrected operating
#'   times for the censored units and the input operating times for the
#'   failed units. If \code{TRUE} the output consists of a detailed list, i.e
#'   the same vector as described before, simulated random numbers, estimated
#'   distribution parameters and a seed for reproducibility.
#'
#' @return A numerical vector of corrected operating times for the censored units
#'   and the input operating times for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following entries:
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
#'     distribution for delay in report.
#'   \item \code{int_seed} : Integer seed number for reproducibility.}
#'
#' @export
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
#' state <- sample(c(0, 1), size = length(date_of_repair), replace = TRUE)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delays(date_prod = date_of_production,
#'                           date_register = date_of_registration,
#'                           date_repair = date_of_repair,
#'                           date_report = date_of_report,
#'                           x = op_time,
#'                           event = state,
#'                           distribution = "lognormal",
#'                           seed = NULL,
#'                           details = FALSE)
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delays(date_prod = date_of_production,
#'                                 date_register = date_of_registration,
#'                                 date_repair = date_of_repair,
#'                                 date_report = date_of_report,
#'                                 x = op_time,
#'                                 event = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = TRUE)

mcs_delays <- function(date_prod, date_register, date_repair, date_report, x,
                       event, distribution = "lognormal", details = FALSE,
                       seed = NULL) {

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

  # Number of Monte Carlo simulated random numbers, i.e. number of censored
  # data.

  n_rand_regist <- sum(is.na(date_register))
  n_rand_report <- sum(event == 0)

  if (!stats::complete.cases(date_prod) || !stats::complete.cases(date_register)) {
    prod_date <- date_prod[(stats::complete.cases(date_prod) &
                           stats::complete.cases(date_register))]
    register_date <- date_register[(stats::complete.cases(date_prod) &
                                   stats::complete.cases(date_register))]
  } else {
    prod_date <- date_prod
    register_date <- date_register
  }

  if (!stats::complete.cases(date_repair) || !stats::complete.cases(date_report)) {
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

  x[is.na(date_register)] <- x[is.na(date_register)] - x_sim_regist
  x[event == 0] <- x[event == 0] - x_sim_report

  if (details == FALSE) {
    output <- x
  } else {
    output <- list(time = x, x_sim_regist = x_sim_regist,
                   x_sim_report = x_sim_report,
                   coefficients_regist = params_regist,
                   coefficients_report = params_report,
                   int_seed = int_seed)
  }
  return(output)
}
