#' Parameter estimation of the mileage distribution
#'
#' This function introduces a random variable of annual mileage using the units in
#' the sample that had a failure and afterwards estimates the parameter(s) of a
#' supposed distribution, using MLE.
#'
#' @param x a numeric vector of operating times. If not available use \code{NA}.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param mileage a numeric vector of driven distances. If not available use \code{NA}.
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#'
#' @return A named vector of estimated parameters for the specified mileage
#'   distribution.
#' @export
#'
#' @examples
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09", NA,
#'                           NA, "2014-06-16", NA, "2014-05-23", "2014-05-09",
#'                           "2014-05-31", NA, "2014-04-13", NA, NA, "2014-03-12",
#'                           NA, "2014-06-02", NA, "2014-03-21", "2014-06-19",
#'                           NA, NA)
#' date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                     NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                     "2015-06-12", NA, "2015-05-04", NA, NA, "2015-05-22",
#'                     NA, "2015-09-17", NA, "2015-08-15", "2015-11-26", NA, NA)
#'
#' op_time <- as.numeric(difftime(as.Date(date_of_repair),
#'                                as.Date(date_of_registration),
#'                                units = "days"))
#' mileage <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
#'              29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
#'              NA, 122842, 20349, NA, NA)
#' state <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
#'
#' params_mileage_annual <- dist_mileage(x = op_time, event = state,
#'                                       mileage = mileage,
#'                                       distribution = "lognormal")

dist_mileage <- function(x, event, mileage, distribution = "lognormal") {

  x_event <- x[event == 1]
  miles_event <- mileage[event == 1]
  miles_annual <- (miles_event / x_event) * 365

  if (distribution == "lognormal") {
    logmu_mileage <- mean(log(miles_annual[miles_annual > 0]), na.rm = TRUE)
    logsd_mileage <- sd(log(miles_annual[miles_annual > 0]), na.rm = TRUE)

    estimates <- c(logmu_mileage, logsd_mileage)
    names(estimates) <- c("meanlog_mileage", "sdlog_mileage")
  } else {
    stop("No valid distribution!")
  }

  return(estimates)
}


#' Estimation of driving distances for censored observations using a Monte Carlo
#' approach
#'
#' This function simulates driving distances for censored observations under the
#' condition that the operating time of these items is known up to a certain date
#' where analysis is made. Operating times for these units can be estimated with
#' functions like \code{\link{mcs_delay_register}}, \code{\link{mcs_delay_report}}
#' and \code{\link{mcs_delays}}.
#' The failed observations (where the driving distances are known) are used to
#' estimate an annual mileage distribution. If the mileage distribution is fully
#' specified annual random driving distances are drawn from this distribution and
#' afterwards adjusted to the operating times of the censored observations.
#'
#' @param x a numeric vector of operating times. If not available use \code{NA}.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param mileage a numeric vector of driven distances. If not available use \code{NA}.
#' @param distribution supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}. So far no other distribution is implemented.
#' @param seed if \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details a logical variable, where the default value is \code{FALSE}.
#'   If \code{FALSE} the output consists of a vector with simulated driving
#'   distances for the censored units regarding to their current operating time
#'   and the input driving distances for the failed units. If \code{TRUE} the
#'   output consists of a detailed list, i.e the same vector as described before,
#'   simulated annual driving distances, estimated distribution parameters and a
#'   seed for reproducibility.
#'
#' @return A numerical vector of simulated driving distances for the censored
#'   units and the input driving distances for the failed units if
#'   \code{details = FALSE}. If \code{details = TRUE} the output is a list which
#'   consists of the following entries:
#'   \itemize{
#'   \item \code{mileage} : simulated driving distances for the censored
#'     units and the input driving distances for the failed units.
#'   \item \code{mileage_sim_annual} : Simulated annual driving distances of
#'     specified distribution with estimated parameters. The length of
#'     \code{x_sim} is equal to the number of censored observations.
#'   \item \code{coefficients} : Estimated coefficients of supposed
#'     distribution.
#'   \item \code{int_seed} : Integer seed number for reproducibility.}
#' @export
#'
#' @examples
#' # Example 1 - Simplified vector output (complete data):
#' date_of_registration <- c("2014-08-17", "2014-03-29", "2014-12-06",
#'                           "2014-09-09", "2014-05-14", "2014-07-01",
#'                           "2014-06-16", "2014-04-03", "2014-05-23",
#'                           "2014-05-09", "2014-05-31", "2014-08-12",
#'                           "2014-04-13", "2014-02-15", "2014-07-07",
#'                           "2014-03-12", "2014-05-27", "2014-06-02",
#'                           "2014-05-20", "2014-03-21", "2014-06-19",
#'                           "2014-02-12", "2014-03-27")
#' date_of_repair <- c("2014-10-21", "2014-09-15", "2015-07-04", "2015-04-10",
#'                     "2015-02-15", "2015-04-14", "2015-04-24", "2015-02-27",
#'                     "2015-04-25", "2015-04-24", "2015-06-12", "2015-08-26",
#'                     "2015-05-04", "2015-04-04", "2015-09-06", "2015-05-22",
#'                     "2015-08-21", "2015-09-17", "2015-09-15", "2015-08-15",
#'                     "2015-11-26", "2015-08-22", "2015-10-05")
#'
#' op_time <- as.numeric(difftime(as.Date(date_of_repair),
#'                                as.Date(date_of_registration),
#'                                units = "days"))
#' mileage <- c(5227, 15655, 13629, 18292, 24291, 34455, 33555, 21659, 21737,
#'              29870, 21068, 22986, 122283, 31592, 49050, 36088, 10918, 11153,
#'              122437, 122842, 20349, 65656, 40777)
#' state <- sample(c(0, 1), size = length(op_time), replace = TRUE)
#'
#' mileage_corrected <- mcs_mileage(x = op_time, event = state,
#'                                  mileage = mileage,
#'                                  distribution = "lognormal", seed = NULL,
#'                                  details = FALSE)
#'
#' # Example 2 - Detailed list output (complete data):
#' list_detail <- mcs_mileage(x = op_time, event = state, mileage = mileage,
#'                                distribution = "lognormal", seed = NULL,
#'                                details = TRUE)
#'
#' # Example 3 - Detailed list output (realistic example):
#' op_time <- c(65, 170, 210, 213, 277, 287, 312, 330, 337, 350, 377, 379, 386,
#'              413, 426, 436, 451, 472, 483, 512, 525, 556, 557)
#' mileage <- c(NA, 15655, 13629, NA, 24291, 34455, NA, 21659, 21737,
#'              NA, 21068, 22986, NA, 31592, 49050, NA, 10918, 11153,
#'              NA, 122842, 20349, NA, 40777)
#' state <- c(0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,
#'            1, 1, 0, 1)
#'
#' list_detail <- mcs_mileage(x = op_time, event = state, mileage = mileage,
#'                                distribution = "lognormal", seed = NULL,
#'                                details = TRUE)

mcs_mileage <- function(x, event, mileage, distribution = "lognormal",
                        seed = NULL, details = FALSE) {

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(event == 0)

  if (distribution == "lognormal") {
    params <- dist_mileage(x = x, event = event, mileage = mileage,
                           distribution = "lognormal")

    mileage_sim <- rlnorm(n = n_rand, meanlog = params[[1]],
                          sdlog = params[[2]])
  } else {
    stop("No valid distribution!")
  }

  mileage[event == 0] <- mileage_sim * (x[event == 0]) / 365

  if (details == FALSE) {
    output <- mileage
  } else {
    output <- list(mileage = mileage, mileage_sim_annual = mileage_sim,
                   coefficients = params, int_seed = int_seed)
  }
  return(output)
}
