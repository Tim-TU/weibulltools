#' Parameter Estimation of a Supposed Statistical Delay Distribution
#'
#' @description
#' This function models a delay random variable (e.g. in logistic, registration, report)
#' using a supposed continuous distribution. First, the element-wise differences
#' in days of both vectors \code{date_1} and \code{date_2} are calculated and then
#' the parameter(s) of the assumed distribution are estimated using MLE. See
#' 'Details' for more information.
#'
#' @details
#' The distribution parameter(s) are determined on the basis of complete cases,
#' i.e. there is no \code{NA} in one of the related vector elements
#' \code{c(date_1[i], date_2[i])}. Time differences less than or equal to 0 are
#' not considered as well.
#'
#' @param date_1 A vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the earlier of the two dates. If no date is
#'   available use \code{NA}.
#' @param date_2 A vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the later of the two dates. If no date is
#'   available use \code{NA}.
#' @param distribution Supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}.
#'
#' @return A named vector of estimated parameter(s) for the specified
#'   distribution.
#' @export
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

dist_delay <- function(
  date_1,
  date_2,
  distribution = c("lognormal", "exponential")
) {

  distribution <- match.arg(distribution)

  # delay variable:
  t_delay <- as.numeric(
    difftime(
      time1 = as.Date(date_2, format = "%Y-%m-%d"),
      time2 = as.Date(date_1, format = "%Y-%m-%d"),
      units = "days"
    )
  )

  # test for delays: all NA and smaller or equal to 0.
  # all NA:
  if (all(is.na(t_delay))) {
    stop("All differences are NA; No parameters can be estimated!")
  }
  # all smaller or equal to zero:
  if (all(t_delay <= 0, na.rm = TRUE)) {
    stop("All differences are smaller or equal to 0; No parameters can be
    estimated!")
  }
  # any smaller or equal to zero:
  if (any(t_delay <= 0, na.rm = TRUE)) {
    warning("At least one of the time differences is smaller or equal to 0 and is
    ignored in the estimation step!")

    t_delay <- t_delay[t_delay > 0]
  }

  if (distribution == "lognormal") {
    # sample size used for the computation of the population standard deviation.
    n <- length(!is.na(t_delay))
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
    distribution = distribution
  )

  class(dist_output) <- "delay_model"

  return(dist_output)
}


#' Correction of Operating Times by Delays using a Monte Carlo Approach
#'
#' @description
#' In general, the amount of available data about units in the field is very
#' different. While there are many information on failures during the warranty
#' period, little is known about the units without a failure (\emph{censored data}).
#' As a result, the operating times of these units are often inaccurate and must
#' be adjusted by delays. But usually delays are only known for defective units.
#' See 'Details' for more information.
#'
#' This function reduces the operating times of (multiple) right censored observations
#' by simulated delays which were drawn from the distribution determined by
#' complete cases (described in 'Details' of \code{\link{dist_delay}}).
#'
#' @details
#' In field data analysis time-dependent characteristics (e.g. \emph{time in service})
#' are often imprecisely recorded. These inaccuracies are caused by unconsidered delays.
#'
#' For a better understanding of the MCS application in the context of field data,
#' two cases are described below.
#' \itemize{
#'   \item \strong{Delay in registration}: It is common that a supplier, who provides
#'     parts to the manufacturing industry does not know when the unit, in which
#'     its parts are installed, were put in service (due to unknown registration
#'     or sales date). Without taking the described delay into account, the time
#'     in service of intact units would be the difference between the present
#'     date and the production date. But the actual operating time is (much) shorter,
#'     since the stress on the component will not start until the complete system
#'     is put in service. Therefore the intact units must be reduced by delays.
#'   \item \strong{Delay in report}: Authorized repairers often do not immediately
#'     notify the manufacturer or OEM of repairs that occur during the warranty
#'     period, but instead pass the information about these repairs in form of
#'     collected applications e.g. monthly or quarterly. The resulting time
#'     difference between the reporting of the repair in the guarantee database
#'     and the actual repair date is called the reporting delay. For a given date
#'     where the analysis is made there could be units which had a failure but are
#'     not registered therefore treated as censored units. In order to take this
#'     case into account and according to the principle of equal
#'     opportunities, the lifetime of intact units is reduced by simulated
#'     reporting delays, so that the case described before is taken into account
#'     due to lifetime reduction.
#' }
#'
#' @references Verband der Automobilindustrie e.V. (VDA); Qualitätsmanagement in
#'   der Automobilindustrie. Zuverlässigkeitssicherung bei Automobilherstellern
#'   und Lieferanten. Zuverlässigkeits-Methoden und -Hilfsmittel.; 4th Edition, 2016,
#'   <ISSN:0943-9412>
#'
#' @inheritParams dist_delay
#' @param x A numeric vector of operating times.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param seed If \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
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
#' # Example 1 - MCS for delay in registration:
#' list_detail <- mcs_delay(
#'   date_1 = date_of_production,
#'   date_2 = date_of_registration,
#'   x = op_time,
#'   status = state,
#'   distribution = "lognormal",
#'   seed = NULL
#' )

mcs_delay <- function(
  date_1,
  date_2,
  x,
  status,
  distribution = c("lognormal", "exponential"),
  seed = NULL
) {

  distribution <- match.arg(distribution)

  # convert date_1 and date_2 to lists if they are vectors:
  if (!is.list(date_1)) date_1 <- list(date_1)
  if (!is.list(date_2)) date_2 <- list(date_2)

  # Step 1: Parameter estimation using complete cases:
  par_list <- purrr::map2(
    date_1,
    date_2,
    dist_helper,
    distribution = distribution
  )

  # Step 2: Simulation of random numbers:
  ## Generate integer that sets the seed (if NULL) in set.seed() function.
  if (purrr::is_null(seed)) {
    seed <- as.integer(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(seed = seed)

  sim_list <- purrr::map2(
    date_2,
    par_list,
    mcs_helper,
    status = status
  )

  print(sim_list)
  ## Adjustment of operating times:
  x <- x - Reduce('+', sim_list)


  # # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  # n_rand <- sum(is.na(date_register))
  # if (any(!stats::complete.cases(date_prod) | !stats::complete.cases(date_register))) {
  #   prod_date <- date_prod[(stats::complete.cases(date_prod) &
  #                             stats::complete.cases(date_register))]
  #   register_date <- date_register[(stats::complete.cases(date_prod) &
  #                                     stats::complete.cases(date_register))]
  # } else {
  #   prod_date <- date_prod
  #   register_date <- date_register
  # }
  #
  # if (distribution == "lognormal") {
  #   params <- dist_delay_register(date_prod = prod_date,
  #                                 date_register = register_date,
  #                                 distribution = "lognormal")
  #
  #   x_sim <- stats::rlnorm(n = n_rand, meanlog = params[[1]], sdlog = params[[2]])
  # } else {
  #   stop("No valid distribution!")
  # }
  #
  # x[is.na(date_register)] <- x[is.na(date_register)] - x_sim
  #
  # if (details == FALSE) {
  #   output <- x
  # } else {
  #   output <- list(time = x, x_sim = x_sim, coefficients = params,
  #                  int_seed = int_seed)
  # }
  # return(output)
  return(x)
}

# helper function to estimate the parameters:
dist_helper <- function(date_1, date_2, distribution) {
  # complete cases, i.e. date pairs that can be used to estimate delays
  ind <- !is.na(date_1) & !is.na(date_2)

  dist_delay(date_1 = date_1, date_2 = date_2, distribution = distribution)
}

# helper function to generate MCS random numbers:
mcs_helper <- function(date_2, status, par_list) {

  # adjustment can only be done for units that have status = 0 and a date_2 entry
  # of NA, otherwise operation time could be exactly determined by taking differences!
  replacable <- is.na(date_2) & status == 0

  # generate random numbers:
  if (par_list$distribution == "lognormal") {
    x_sim <- stats::rlnorm(length(date_2), par_list$coefficients[1], par_list$coefficients[2])
  }

  if (par_list$distribution == "exponential") {
    x_sim <- stats::rexp(length(date_2), 1 / par_list$coefficients[1])
  }

  x_sim[!replacable] <- 0

  return(x_sim)
}


#' Parameter Estimation of the Delay in Registration Distribution
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' \code{dist_delay_register()} is no longer under active development, switching
#' to \code{dist_delay()} is recommended.
#'
#' This function introduces a delay random variable by calculating the time
#' difference between the registration and production date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using MLE.
#'
#' @param date_prod A vector of class \code{"character"} or \code{"Date"}, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   If no date is available use \code{NA}.
#' @param date_register A vector of class \code{"character"} or \code{"Date"}, in
#'   the format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   If no date is available use \code{NA}.
#' @param distribution Supposed distribution of the random variable. The default
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
#'   date_prod = date_of_production,
#'   date_register = date_of_registration,
#'   distribution = "lognormal"
#' )

dist_delay_register <- function(
  date_prod,
  date_register,
  distribution = "lognormal"
) {
  deprecate_soft("2.0.0", "dist_delay_register()")

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
    stop("All differences are NA; No parameters can be estimated!")
  }
  # all smaller or equal to zero:
  if (all(t_regist <= 0, na.rm = TRUE)) {
    stop("All differences are smaller or equal to 0; No parameters can be
    estimated!")
  }
  # any smaller or equal to zero:
  if (any(t_regist <= 0, na.rm = TRUE)) {
    warning("At least one of the time differences is smaller or equal to 0 and is
    ignored in the estimation step.")

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
#' \lifecycle{soft-deprecated}
#'
#' \code{mcs_delay_register()} is no longer under active development, switching
#' to \code{mcs_delay()} is recommended.
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
#' @inheritParams dist_delay_register
#' @param x A numeric vector of operating times.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param distribution Supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#' @param seed If \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details A logical. If \code{FALSE} the output consists of a vector with
#'   corrected operating times for the censored units and the input operating
#'   times for the failed units. If \code{TRUE} the output consists of a detailed
#'   list, i.e the same vector as described before, simulated random numbers,
#'   estimated distribution parameters and a seed for reproducibility.
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
#'                                   status = state,
#'                                   distribution = "lognormal",
#'                                   seed = NULL,
#'                                   details = FALSE)
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_register(date_prod = date_of_production,
#'                                   date_register = date_of_registration,
#'                                   x = op_time,
#'                                   status = state,
#'                                   distribution = "lognormal",
#'                                   seed = NULL,
#'                                   details = TRUE)

mcs_delay_register <- function(
  date_prod,
  date_register,
  x,
  status,
  distribution = "lognormal",
  seed = NULL,
  details = FALSE
) {
  deprecate_soft("2.0.0", "mcs_delay_register()")

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

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

  x[is.na(date_register)] <- x[is.na(date_register)] - x_sim

  if (details == FALSE) {
    output <- x
  } else {
    output <- list(time = x, x_sim = x_sim, coefficients = params,
      int_seed = int_seed)
  }
  return(output)
}


#' Parameter Estimation of the Delay in Report Distribution
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' \code{dist_delay_report()} is no longer under active development, switching
#' to \code{dist_delay()} is recommended.
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
#' @inheritParams dist_delay_register
#'
#' @return A named vector of estimated parameters for the specified
#'   distribution.
#' @export
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

dist_delay_report <- function(
  date_repair,
  date_report,
  distribution = "lognormal"
) {
  deprecate_soft("2.0.0", "dist_delay_report()")

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
    stop("All differences are NA; No parameters can be estimated!")
  }
  # all smaller or equal to zero:
  if (all(t_report <= 0, na.rm = TRUE)) {
    stop("All differences are smaller or equal to 0; No parameters can be
    estimated!")
  }
  # any smaller or equal to zero:
  if (any(t_report <= 0, na.rm = TRUE)) {
    warning("At least one of the time differences is smaller or equal to 0 and is
    ignored in the estimation step!")

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
#' \lifecycle{soft-deprecated}
#'
#' \code{mcs_delay_report()} is no longer under active development, switching
#' to \code{mcs_delay()} is recommended.
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
#'                                 status = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = FALSE)
#'
#' # Example 2 - Detailed list output:
#' list_detail <- mcs_delay_report(date_repair = date_of_repair,
#'                                 date_report = date_of_report,
#'                                 x = op_time,
#'                                 status = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = TRUE)

mcs_delay_report <- function(
  date_repair,
  date_report,
  x,
  status,
  distribution = "lognormal",
  details = FALSE,
  seed = NULL
) {
  deprecate_soft("2.0.0", "mcs_delay_report()")

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

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

  x[status == 0] <- x[status == 0] - x_sim

  if (details == FALSE) {
    output <- x
  } else {
    output <- list(time = x, x_sim = x_sim, coefficients = params,
      int_seed = int_seed)
  }
  return(output)
}


#' Adjustment of Operating Times by Delays using a Monte Carlo Approach
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' \code{mcs_delays()} is no longer under active development, switching
#' to \code{mcs_delay()} is recommended.
#'
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
#' state <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' # Example 1 - Simplified vector output:
#' x_corrected <- mcs_delays(date_prod = date_of_production,
#'                           date_register = date_of_registration,
#'                           date_repair = date_of_repair,
#'                           date_report = date_of_report,
#'                           x = op_time,
#'                           status = state,
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
#'                                 status = state,
#'                                 distribution = "lognormal",
#'                                 seed = NULL,
#'                                 details = TRUE)

mcs_delays <- function(
  date_prod,
  date_register,
  date_repair,
  date_report,
  x,
  status,
  distribution = "lognormal",
  details = FALSE,
  seed = NULL
) {
  deprecate_soft("2.0.0", "mcs_delays()")

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

  x[is.na(date_register)] <- x[is.na(date_register)] - x_sim_regist
  x[status == 0] <- x[status == 0] - x_sim_report

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
