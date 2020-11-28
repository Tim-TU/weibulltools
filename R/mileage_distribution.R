#' Parameter Estimation of an Annual Mileage Distribution
#'
#' @description
#' This function models a mileage random variable on an annual basis with respect
#' to a supposed continuous distribution. First, the distances are normalized to
#' one year (365 days) using a linear relationship between the distances and
#' operating times. Second, the parameter(s) of the assumed distribution are
#' estimated using MLE. See 'Details' for more information.
#'
#' @details
#' The distribution parameter(s) are determined on the basis of complete cases,
#' i.e. there is no \code{NA} in one of the related vector elements
#' \code{c(mileage[i], x[i])}. Distances and operating times less than or equal
#' to 0 are not considered as well.
#'
#' \strong{Assumption of linear interpolation}: Imagine a component in a vehicle
#'   has endured a distance of 25000 kilometers (km) in 500 days (d), the annual
#'   distance of this unit is \deqn{25000 km \cdot (\frac{365 d} {500 d}) = 18250 km}{%
#'                 25000 km * (365 d / 500 d) = 18250 km}
#'
#' @param mileage A numeric vector of distances covered. If not available use \code{NA}.
#' @param x A numeric vector of operating times. If not available use \code{NA}.
#' @param distribution Supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}.
#'
#' @return A list of class \code{"mileage_estimation"} which contains:
#'   \itemize{
#'     \item \code{coefficients} A named vector of estimated parameter(s).
#'     \item \code{miles_annual} A numeric vector of element-wise computed annual
#'       distances using linear interpolation described in 'Details'.
#'     \item \code{distribution} Specified distribution.
#'   }
#' @export
#'
#' @examples
#' date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09", NA,
#'                           NA, "2014-06-16", NA, "2014-05-23", "2014-05-09",
#'                           "2014-05-31", NA, "2014-04-13", NA, NA, "2014-03-12",
#'                           NA, "2014-06-02", NA, "2014-03-21", "2014-06-19",
#'                           NA, NA)
#' date_of_repair       <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                           NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                           "2015-06-12", NA, "2015-05-04", NA, NA, "2015-05-22",
#'                           NA, "2015-09-17", NA, "2015-08-15", "2015-11-26",
#'                           NA, NA)
#' mileage              <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
#'                           29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
#'                           NA, 122842, 20349, NA, NA)
#'
#' op_time <- as.numeric(
#'   difftime(
#'     as.Date(date_of_repair, format = "%Y-%m-%d"),
#'     as.Date(date_of_registration, format = "%Y-%m-%d"),
#'     units = "days"
#'   )
#' )
#'
#' Example 1 - Assuming a lognormal annual mileage distribution:
#' params_mileage_annual <- dist_mileage(
#'   mileage = mileage,
#'   x = op_time,
#'   distribution = "lognormal"
#' )

dist_mileage <- function(
  mileage,
  x,
  distribution = c("lognormal", "exponential")
) {

  distribution <- match.arg(distribution)

  # computing annual mileage:
  ## Case 1: Elements in x and mileage are both 0 -> NaN (OK, since is.na(NaN) == T)
  ## Case 2: Element in x is 0 and mileage is  < 0 -> -Inf (Ok, since (-Inf <= 0) == T)
  ## Case 3: Element in x is 0 and mileage is > 0 -> Inf (OK, since is.infinite is used)
  ## Case 4: Element in x > 0 and in mileage < 0 and viceversa -> negative (OK, will be removed)
  ## Case 5: Element in x is NA or in mileage is NA or both are NA -> NA (OK)
  ## Case 6: Elements in x and mileage are both <0 -> positive (one element should be set to NA)
  ## RESULT: Return all computed annual miles but the above cases are excluded for
  ##         paramter estimation!

  ## Case 6: Set x values where both x and mileage are negative to 'NA'.
  ##  Filters also NAs and hence setting to NA is ok.
  if (any(x <= 0 & mileage <= 0, na.rm = TRUE)) {
    warning("corresponding element(s) of both, 'mileage' and 'x' are negative!
            the computed annual distance is set to NA")

    x[x <= 0 & mileage <= 0] <- NA
  }

  miles_annual <- miles_annual_origin <- (mileage / x) * 365

  # Checks:
  ## case of Inf, i.e. numeric divided by 0:
  ## miles_annual should not contain a Inf value -> mean(c(1, 2, Inf)) -> Inf
  ## is.finite can not be used since is.finite(NA) == FALSE -> is.infinite work
  if (any(is.infinite(miles_annual))) {
    warning("at least one computed annual distance is infinite and is ignored
            for the estimation step!")

    miles_annual <- miles_annual[!is.infinite(miles_annual)]
  }

  ## all NA:
  if (all(is.na(miles_annual))) {
    stop("all computed annual distances are NA; no parameters can be estimated!")
  }
  ## any or all annual distances are smaller or equal to zero:
  if (any(miles_annual <= 0, na.rm = TRUE)) {
    if (all(miles_annual <= 0, na.rm = TRUE)) {
      ### all:
      stop("all computed annual distances are smaller or equal to 0; no",
           " parameters can be estimated!")
    } else {
      ### any:
      warning("at least one computed annual distance is smaller or equal to 0 and is
              ignored for the estimation step!")
      miles_annual <- miles_annual[miles_annual > 0]
    }
  }

  if (distribution == "lognormal") {
    # sample size used for the computation of the population standard deviation.
    n <- sum(!is.na(miles_annual))
    ml_loc <- mean(log(miles_annual), na.rm = TRUE)
    ml_sc <- stats::sd(log(miles_annual), na.rm = TRUE) * (n - 1) / n

    estimates <- c(loc = ml_loc, sc = ml_sc)
  }

  if (distribution == "exponential") {
    ml_sc <- mean(miles_annual, na.rm = TRUE)

    estimates <- c(sc = ml_sc)
  }

  dist_output <- list(
    coefficients = estimates,
    miles_annual = miles_annual_origin,
    distribution = distribution
  )

  class(dist_output) <- c("mileage_estimation", "model_estimation", class(dist_output))

  return(dist_output)
}


#' Estimation of Driving Distances for Censored Observations using a Monte Carlo
#' Approach
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
#' @inheritParams dist_mileage
#' @param seed If \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#' @param details A logical variable, where the default value is \code{FALSE}.
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
#'   consists of the following elements:
#'   \itemize{
#'   \item \code{mileage} : Simulated driving distances for the censored
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
#' mileage_corrected <- mcs_mileage(x = op_time, status = state,
#'                                  mileage = mileage,
#'                                  distribution = "lognormal", seed = NULL,
#'                                  details = FALSE)
#'
#' # Example 2 - Detailed list output (complete data):
#' list_detail <- mcs_mileage(x = op_time, status = state, mileage = mileage,
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
#' list_detail <- mcs_mileage(x = op_time, status = state, mileage = mileage,
#'                                distribution = "lognormal", seed = NULL,
#'                                details = TRUE)

mcs_mileage <- function(x, status, mileage, distribution = "lognormal",
                        seed = NULL, details = FALSE) {

  # Generate integer that sets the seed (if NULL) in set.seed() function.
  if (!is.null(seed)) {
    int_seed <- seed
  } else {
    int_seed <- ceiling(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(int_seed)

  # Number of Monte Carlo simulated random numbers, i.e. number of censored data.
  n_rand <- sum(status == 0)

  if (distribution == "lognormal") {
    params <- dist_mileage(x = x, status = status, mileage = mileage,
                           distribution = "lognormal")

    mileage_sim <- stats::rlnorm(n = n_rand, meanlog = params[[1]],
                          sdlog = params[[2]])
  } else {
    stop("No valid distribution!")
  }

  mileage[status == 0] <- mileage_sim * (x[status == 0]) / 365

  if (details == FALSE) {
    output <- mileage
  } else {
    output <- list(mileage = mileage, mileage_sim_annual = mileage_sim,
                   coefficients = params, int_seed = int_seed)
  }

  return(output)
}
