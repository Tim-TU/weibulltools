#' Parameter Estimation of an Annual Mileage Distribution
#'
#' @description
#' This function models a mileage random variable on an annual basis with respect
#' to a supposed continuous distribution. First, the distances are calculated for
#' one year (365 days) using a linear relationship between the distance and
#' operating time. Second, the parameter(s) of the assumed distribution are
#' estimated using MLE. See 'Details' for more information.
#'
#' @details
#' The distribution parameter(s) are determined on the basis of complete cases,
#' i.e. there is no \code{NA} in one of the related vector elements
#' \code{c(mileage[i], time[i])}. Distances and operating times less than or equal
#' to zero are not considered as well.
#'
#' \strong{Assumption of linear relationship}: Imagine a component in a vehicle
#'   has endured a distance of 25000 kilometers (km) in 500 days (d), the annual
#'   distance of this unit is \deqn{25000 km \cdot (\frac{365 d} {500 d}) = 18250 km}{%
#'                 25000 km * (365 d / 500 d) = 18250 km}
#'
#' @param mileage A numeric vector of distances covered. Use \code{NA} for missing
#'   elements.
#' @param time A numeric vector of operating times. Use \code{NA} for missing
#'   elements.
#' @param distribution Supposed distribution of the random variable.
#'
#' @return A list of class \code{"mileage_estimation"} which contains:
#'   \itemize{
#'     \item \code{coefficients} A named vector of estimated parameter(s).
#'     \item \code{miles_annual} A numeric vector of element-wise computed annual
#'       distances using the linear relationship described in 'Details'.
#'     \item \code{distribution} Specified distribution.
#'   }
#'
#' @examples
#' # Data for examples:
#' date_of_registration <- c("2014-08-17", "2014-03-29", "2014-12-06",
#'                           "2014-09-09", "2014-05-14", "2014-07-01",
#'                           "2014-06-16", "2014-04-03", "2014-05-23",
#'                           "2014-05-09", "2014-05-31", "2014-08-12",
#'                           "2014-04-13", "2014-02-15", "2014-07-07",
#'                           "2014-03-12", "2014-05-27", "2014-06-02",
#'                           "2014-05-20", "2014-03-21", "2014-06-19",
#'                           "2014-02-12", "2014-03-27")
#' date_of_repair       <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                           NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                           "2015-06-12", NA, "2015-05-04", NA, NA, "2015-05-22",
#'                           NA, "2015-09-17", NA, "2015-08-15", "2015-11-26",
#'                           NA, NA)
#' date_of_analysis     <- "2015-12-31"
#'
#' ## Assume that mileage is only known for units that have failed (date_of_repair != NA).
#' mileage              <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
#'                           29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
#'                           NA, 122842, 20349, NA, NA)
#'
#' ## time in service is the difference between repair and registration for failed
#' ## items and the difference between date of analysis and date of registration
#' ## for intact units.
#' time_in_service <- difftime(
#'   as.Date(date_of_repair, format = "%Y-%m-%d"),
#'   as.Date(date_of_registration, format = "%Y-%m-%d"),
#'   units = "days"
#' )
#' time_in_service[is.na(time_in_service)] <- difftime(
#'   as.Date(date_of_analysis, format = "%Y-%m-%d"),
#'   as.Date(date_of_registration[is.na(time_in_service)], format = "%Y-%m-%d"),
#'   units = "days"
#' )
#' time_in_service <- as.numeric(time_in_service)
#'
#' # Example 1 - Assuming lognormal annual mileage distribution:
#' params_mileage_annual <- dist_mileage(
#'   mileage = mileage,
#'   time = time_in_service,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Assuming exponential annual mileage distribution:
#' params_mileage_annual_2 <- dist_mileage(
#'   mileage = mileage,
#'   time = time_in_service,
#'   distribution = "exponential"
#' )
#'
#' @export
dist_mileage <- function(
  mileage,
  time,
  distribution = c("lognormal", "exponential")
) {

  distribution <- match.arg(distribution)

  # Check for negative mileage, stop if TRUE:
  if (any(mileage < 0, na.rm = TRUE)) {
    stop("There is at least one negative element in argument 'mileage'!")
  }

  # Defining annual distance variable (for estimation) and origin variable (output):
  miles_annual <- miles_annual_origin <- (mileage / time) * 365

  # Checks:
  ## case of Inf, i.e. x is 0: could be handled with `is.infinite()`
  if (any(is.infinite(miles_annual))) {
    warning("At least one computed annual distance is infinite and is ignored",
            " for the estimation step!")

    miles_annual <- miles_annual[!is.infinite(miles_annual)]
  }

  ## all NA:
  if (all(is.na(miles_annual))) {
    stop("All computed annual distances are NA. No parameters can be estimated!")
  }
  ## any or all annual distances are smaller or equal to zero:
  if (any(miles_annual <= 0, na.rm = TRUE)) {
    if (all(miles_annual <= 0, na.rm = TRUE)) {
      ### all:
      stop("All computed annual distances are smaller or equal to 0.  No",
           " parameters can be estimated!")
    } else {
      ### any:
      warning("At least one computed annual distance is smaller or equal to 0",
              " and is ignored for the estimation step!")
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

  class(dist_output) <- c("mileage_estimation", class(dist_output))

  return(dist_output)
}



#' Simulation of Unknown Covered Distances using a Monte Carlo Approach
#'
#' @description
#' This function simulates distances for units where these are unknown, i.e.
#' \code{mileage = NA}.
#'
#' First, random numbers of the annual mileage distribution, estimated by
#' \code{\link{dist_mileage}}, are drawn. Second, the drawn annual distances are
#' converted with respect to the actual operating times (in days) using a linear
#' relationship. See 'Details'.
#'
#' @details
#' \strong{Assumption of linear relationship}: Imagine the distance of the vehicle
#' is unknown. A distance of 3500.25 kilometers (km) was drawn from the annual
#' distribution and the known operating time is 200 days (d). So the resulting
#' distance of this vehicle is \deqn{3500.25 km \cdot (\frac{200 d} {365 d}) = 1917.945 km}{%
#'                 3500.25 km * (200 d / 365 d) = 1917.945 km}
#'
#' @inheritParams dist_mileage
#'
#' @param status Optional argument. If used it has to be a vector of binary data
#'   (0 or 1) indicating whether unit i is a right censored observation (= 0) or
#'   a failure (= 1). The effect of status on the return is described in 'Value'.
#' @param id A vector for the identification of every unit.
#'
#' @return A list containing the following elements:
#'   \itemize{
#'     \item \code{data} A tibble with class attributes \code{"mcs_data"} and
#'       \code{"reliability_data"} if \code{status} is provided. Since the
#'       attribute \code{"reliability_data"} enables the direct usage of \code{data}
#'       inside \code{estimate_cdf} (\code{\link{estimate_cdf.reliability_data}}),
#'       the required lifetime characteristic is automatically set to the distance
#'       \code{mileage}.
#'
#'       If \code{status = NULL} class attribute is \code{"mcs_data"}, which is not
#'       supported by \code{estimate_cdf} due to missing \code{status}.
#'
#'       The tibble contains the following columns:
#'       \itemize{
#'         \item \code{mileage} Simulated distances for unknown \code{mileage} and
#'           input distances for known \code{mileage}.
#'         \item \code{time} Input operating times.
#'         \item \code{status} (\strong{optional})
#'           \itemize{
#'             \item If argument \code{status = NULL} column \code{status} does
#'               not exist.
#'             \item If argument \code{status} is provided the column contains
#'               the entered binary data (0 or 1).
#'           }
#'         \item \code{id} Identification of every unit.
#'       }
#'     \item \code{sim_data} A tibble with column \code{sim_mileage} that holds the
#'       simulated distances for unknown \code{mileage} and \code{0} otherwise.
#'     \item \code{model_estimation} A list containing a named list
#'       (\code{"mileage_distribution"}) with output of \code{\link{dist_mileage}}.
#'   }
#'
#' @examples
#' # Data for examples:
#' date_of_registration <- c("2014-08-17", "2014-03-29", "2014-12-06",
#'                           "2014-09-09", "2014-05-14", "2014-07-01",
#'                           "2014-06-16", "2014-04-03", "2014-05-23",
#'                           "2014-05-09", "2014-05-31", "2014-08-12",
#'                           "2014-04-13", "2014-02-15", "2014-07-07",
#'                           "2014-03-12", "2014-05-27", "2014-06-02",
#'                           "2014-05-20", "2014-03-21", "2014-06-19",
#'                           "2014-02-12", "2014-03-27")
#' date_of_repair       <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
#'                           NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
#'                           "2015-06-12", NA, "2015-05-04", NA, NA, "2015-05-22",
#'                           NA, "2015-09-17", NA, "2015-08-15", "2015-11-26",
#'                           NA, NA)
#' date_of_analysis     <- "2015-12-31"
#'
#' ## Assume that mileage is only known for units that have failed (date_of_repair != NA).
#' mileage              <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
#'                           29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
#'                           NA, 122842, 20349, NA, NA)
#'
#' ## time in service is the difference between repair and registration for failed
#' ## items and the difference between date of analysis and date of registration
#' ## for intact units.
#' time_in_service <- difftime(
#'   as.Date(date_of_repair, format = "%Y-%m-%d"),
#'   as.Date(date_of_registration, format = "%Y-%m-%d"),
#'   units = "days"
#' )
#' time_in_service[is.na(time_in_service)] <- difftime(
#'   as.Date(date_of_analysis, format = "%Y-%m-%d"),
#'   as.Date(date_of_registration[is.na(time_in_service)], format = "%Y-%m-%d"),
#'   units = "days"
#' )
#' time_in_service <- as.numeric(time_in_service)
#'
#' # Example 1 - Reproducibility of drawn random numbers:
#' set.seed(1234)
#' mcs_distances <- mcs_mileage(
#'   mileage = mileage,
#'   time = time_in_service,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for distances assuming a exponential annual mileage distribution:
#' mcs_distances_2 <- mcs_mileage(
#'   mileage = mileage,
#'   time = time_in_service,
#'   distribution = "exponential"
#' )
#'
#' status <- ifelse(!is.na(date_of_repair), 1, 0)
#'
#' # Example 3 - MCS for distances using status:
#' mcs_distances_3 <- mcs_mileage(
#'   mileage = mileage,
#'   time = time_in_service,
#'   status = status,
#'   distribution = "lognormal"
#' )
#'
#' ## Using result of *$data in estimate_cdf() -> not working at the moment!
#' #prob_estimation <- estimate_cdf(
#' #  x = mcs_distances_3$data,
#' # methods = "kaplan"
#' #)
#'
#' # plot_prob_estimation <- plot_prob(prob_estimation)
#'
#' @export
mcs_mileage <- function(
  mileage,
  time,
  status = NULL,
  id = paste0("ID", seq_len(length(time))),
  distribution = c("lognormal", "exponential")
) {

  # Checks:
  ## Check for distributions:
  distribution <- match.arg(distribution)

  ## Check for different length in time and mileage:
  if (length(mileage) != length(time)) {
    stop("Elements of 'mileage' and 'time' differ in lengths!")
  }

  # Step 1: Parameter estimation using complete cases:
  par_list <- dist_mileage(
    mileage = mileage,
    time = time,
    distribution = distribution
  )

  # Step 2: Simulation of random numbers:
  sim_nums <- mcs_helper(
    x = mileage,
    par_list = par_list
  )

  ## Imputation of missing mileages:
  mileage[is.na(mileage)] <- (sim_nums[is.na(mileage)] * time[is.na(mileage)]) / 365

  # Defining data_tbl with class "mcs_data" and/or "reliability_data":
  if (purrr::is_null(status)) {
    data_tbl <- tibble::tibble(
      mileage = mileage,
      time = time,
      id = id
    )
    class(data_tbl) <- c("mcs_data", class(data_tbl))

  } else {
    # check for status:
    if (!is_status(status)) {
      stop("'status' must be numeric with elements 0 or 1!")
    }

    data_tbl <- tibble::tibble(
      mileage = mileage,
      time = time,
      status = status,
      id = id
    )
    class(data_tbl) <- c("mcs_data", "reliability_data", class(data_tbl)) # is not working since 'mileage' != x
  }

  mcs_output <- list(
    data = data_tbl,
    sim_data = tibble::tibble(sim_mileage = sim_nums),
    model_estimation = list(
      mileage_distribution = par_list
    )
  )

  return(mcs_output)
}
