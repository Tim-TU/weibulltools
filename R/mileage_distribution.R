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
#' \code{c(mileage[i], x[i])}. Distances and operating times less than or equal
#' to zero are not considered as well.
#'
#' \strong{Assumption of linear relationship}: Imagine a component in a vehicle
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
#'       distances using the linear relationship described in 'Details'.
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

  # Check for negative mileage, stop if TRUE:
  if (any(mileage < 0, na.rm = TRUE)) {
    stop("There is at least one negative element in argument 'mileage'!")
  }

  # Defining annual distance variable (for estimation) and origin variable (output):
  miles_annual <- miles_annual_origin <- (mileage / x) * 365

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
#' First, a random number of an annual mileage distribution, which was estimated
#' by \code{\link{dist_mileage}}, is drawn. Second, the drawn number (annual basis) is
#' converted on the basis of a linear relationship using actual operating time
#' (in days). See 'Details'.
#'
#' @details
#' \strong{Assumption of linear relationship}: Imagine the distance of the vehicle
#' is unknown. A distance of 3500.25 kilometers (km) was drawn from the annual
#' distribution and the known operating time is 200 days (d). So the resulting
#' distance of this vehicle is \deqn{3500.25 km \cdot (\frac{200 d} {365 d}) = 1917.945 km}{%
#'                 3500.25 km * (200 d / 365 d) = 1917.945 km}
#'
#' @param mileage A numeric vector of distances covered. If unknown use \code{NA}.
#' @param x A numeric vector of operating times. If unknown use \code{NA}.
#' @param status Optional argument that determines the class of the returned list
#'   element \code{data}. If \code{NULL} (default) \code{data} has class
#'   \code{"mcs_data"}. If provided, it has to be a vector of binary data (0 or 1)
#'   indicating whether unit \emph{i} is a right censored observation (= 0) or a
#'   failure (= 1). In the later case \code{data} has classes \code{"mcs_data"} and
#'   \code{"reliability_data"}.
#' @param id Identification for every unit.
#' @param distribution Supposed distribution of the random variable. The default
#'   value is \code{"lognormal"}.
#' @param seed If \code{seed = NULL} a random seed is used. Otherwise the user
#'   can specify an integer for the seed.
#'
#' @return A list containing the following elements:
#'   \itemize{
#'     \item \code{data} A tibble with class \code{"mcs_data"} (if \code{status = NULL})
#'       or with classes \code{"mcs_data"} and \code{"reliability_data"} (if \code{status}
#'       is provided) containing the following columns:
#'       \itemize{
#'         \item \code{mileage} Simulated distances for units where \code{mileage}
#'           was unknown and input distances where where \code{mileage} was known.
#'         \item \code{x} Input operating times.
#'         \item \code{status} (\strong{optional}) If argument \code{status = NULL}
#'           column \code{status} does not exist. If argument \code{status} is provided
#'           the column contains the entered binary data (0 or 1) indicating whether
#'           a unit is a right censored observation (= 0) or a failure (= 1).
#'         \item \code{id} Identification for every unit.
#'       }
#'     \item \code{sim_data} A tibble with column \code{sim_mileage} that holds the
#'       simulated distance numbers for units where these were unknown and \code{0}
#'       for for units where these were known.
#'     \item \code{model_estimation} A list containing a named list
#'       (\code{"mileage_distribution"}) with output of \code{\link{dist_mileage}}.
#'     \item \code{seed} : Integer seed number for reproducibility.
#'   }
#'
#' @export
#'
#' @examples
#' # Example 1 - MCS for Complete Data, nothing to impute:
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
#' mcs_mileages <- mcs_mileage(
#'   mileage = mileage,
#'   x = op_time,
#'   status = state,
#'   distribution = "lognormal"
#'   )
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

mcs_mileage <- function(
  mileage,
  x,
  status = NULL,
  id = paste0("ID", seq_len(length(x))),
  distribution = c("lognormal", "exponential"),
  seed = NULL
) {

  # Checks:
  ## Check for distributions:
  distribution <- match.arg(distribution)

  ## Check for different length in x and mileage:
  if (length(mileage) != length(x)) {
    stop("elements of 'mileage' and 'x' differ in lengths!")
  }

  # Step 1: Parameter estimation using complete cases:
  par_list <- dist_mileage(
    mileage = mileage,
    x = x,
    distribution = distribution
  )

  # Step 2: Simulation of random numbers:
  ## Generate integer that sets the seed (if NULL) in set.seed() function.
  if (purrr::is_null(seed)) {
    seed <- as.integer(stats::runif(n = 1, min = 0, max = 1e6))
  }
  set.seed(seed = seed)

  sim_nums <- mcs_helper(
    x = mileage,
    par_list = par_list
  )

  ## Imputation of missing mileages:
  mileage[is.na(mileage)] <- (sim_nums[is.na(mileage)] * x) / 365

  # Defining data_tbl with class "mcs_data" and/or "reliability_data":
  if (purrr::is_null(status)) {
    data_tbl <- tibble::tibble(
      mileage = mileage,
      x = x,
      id = id
    )
    class(data_tbl) <- c("mcs_data", class(data_tbl))
  } else {
    data_tbl <- tibble::tibble(
      mileage = mileage,
      x = x,
      status = status,
      id = id
    )
    class(data_tbl) <- c("mcs_data", "reliability_data", class(data_tbl))
  }

  mcs_output <- list(
    data = data_tbl,
    sim_data = tibble::tibble(sim_mileage = sim_nums),
    model_estimation = list(
      mileage_distribution = par_list
    ),
    seed = seed
  )

  return(mcs_output)
}
