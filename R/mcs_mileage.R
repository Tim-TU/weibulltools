#' Simulation of Unknown Covered Distances using a Monte Carlo Approach
#'
#' @description
#' This function simulates distances for units where these are unknown.
#'
#' First, random numbers of the annual mileage distribution, estimated by
#' [dist_mileage], are drawn. Second, the drawn annual distances are
#' converted with respect to the actual operating times (in days) using a linear
#' relationship. See 'Details'.
#'
#' @details
#' **Assumption of linear relationship**: Imagine the distance of the vehicle
#' is unknown. A distance of 3500.25 kilometers (km) was drawn from the annual
#' distribution and the known operating time is 200 days (d). So the resulting
#' distance of this vehicle is
#' \deqn{3500.25 km \cdot (\frac{200 d} {365 d}) = 1917.945 km}{%
#' 3500.25 km * (200 d / 365 d) = 1917.945 km}
#'
#' @inheritParams dist_mileage
#'
#' @return A list with class `wt_mcs_mileage` containing the following elements:
#'
#' * `data` : A `tibble` returned by [mcs_mileage_data] where two modifications
#'   has been made:
#'
#'   * If the column `status` exists, the `tibble` has additional classes
#'     `wt_mcs_data` and `wt_reliability_data`. Otherwise, the `tibble` only has
#'     the additional class `wt_mcs_data` (which is not supported by [estimate_cdf]).
#'   * The column `mileage` is renamed to `x` (to be in accordance with
#'     [reliability_data]) and contains simulated distances for incomplete
#'     observations and input distances for the complete observations.
#' * `sim_data` : A `tibble` with column `sim_mileage` that holds the simulated
#'   distances for incomplete cases and `0` for complete cases.
#' * `model_estimation` : A list returned by [dist_mileage].
#'
#' @seealso [dist_mileage] for the determination of a parametric annual mileage
#' distribution and [estimate_cdf] for the estimation of failure probabilities.
#'
#' @examples
#' # MCS data preparation:
#' mcs_tbl <- mcs_mileage_data(
#'   field_data,
#'   mileage = mileage,
#'   time = dis,
#'   status = status,
#'   id = vin
#' )
#'
#' # Example 1 - Reproducibility of drawn random numbers:
#' set.seed(1234)
#' mcs_distances <- mcs_mileage(
#'   x = mcs_tbl,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for distances with exponential annual mileage distribution:
#' mcs_distances_2 <- mcs_mileage(
#'   x = mcs_tbl,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - MCS for distances with downstream probability estimation:
#' ## Apply 'estimate_cdf()' to *$data:
#' prob_estimation <- estimate_cdf(
#'   x = mcs_distances$data,
#'   methods = "kaplan"
#' )
#'
#' ## Apply 'plot_prob()':
#' plot_prob_estimation <- plot_prob(prob_estimation)
#'
#' @md
#'
#' @export
mcs_mileage <- function(x, ...) {
  UseMethod("mcs_mileage")
}



#' @rdname mcs_mileage
#'
#' @export
mcs_mileage.wt_mcs_mileage_data <- function(
                                   x,
                                   distribution = c("lognormal", "exponential"),
                                   ...
) {

  # Checks:
  ## Check for distributions:
  distribution <- match.arg(distribution)

  mileage <- x$mileage
  time <- x$time

  mcs_mileage_(
    data = x,
    x = mileage,
    time = time,
    distribution = distribution
  )
}



#' Simulation of Unknown Covered Distances using a Monte Carlo Approach
#'
#' @inherit mcs_mileage description details return seealso
#'
#' @inheritParams dist_mileage.default
#' @inheritParams mcs_mileage_data
#'
#' @examples
#' # Example 1 - Reproducibility of drawn random numbers:
#' set.seed(1234)
#' mcs_distances <- mcs_mileage(
#'   x = field_data$mileage,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - MCS for distances with exponential annual mileage distribution:
#' mcs_distances_2 <- mcs_mileage(
#'   x = field_data$mileage,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - MCS for distances with downstream probability estimation:
#' ## Apply 'estimate_cdf()' to *$data:
#' prob_estimation <- estimate_cdf(
#'   x = mcs_distances$data,
#'   methods = "kaplan"
#' )
#'
#' ## Apply 'plot_prob()':
#' plot_prob_estimation <- plot_prob(prob_estimation)
#'
#' @md
#'
#' @export
mcs_mileage.default <- function(x,
                                time,
                                status = NULL,
                                id = paste0("ID", seq_len(length(time))),
                                distribution = c("lognormal", "exponential"),
                                ...
) {

  # Checks:
  ## Check for distributions:
  distribution <- match.arg(distribution)

  ## Check for different length in time and x:
  if (length(x) != length(time)) {
    stop("Elements of 'x' and 'time' differ in length!", call. = FALSE)
  }

  mcs_mileage_(
    x = x,
    time = time,
    status = status,
    id = id,
    distribution = distribution
  )
}



# Helper function that performs MCS for distances:
mcs_mileage_ <- function(data = NULL,
                         x, # vector
                         time, # vector,
                         status = NULL,
                         id = NULL,
                         distribution
) {

  # Step 1: Parameter estimation using complete cases:
  par_list <- dist_mileage.default(
    x = x,
    time = time,
    distribution = distribution
  )

  # Step 2: Simulation of random numbers:
  sim_nums <- mcs_helper(
    x = x,
    par_list = par_list
  )

  ## Imputation of missing mileages:
  x[is.na(x)] <- (sim_nums[is.na(x)] * time[is.na(x)]) / 365

  # Step 3: Create output:
  ## Create MCS_Mileage_Data and renaming 'mileage' to 'x':
  ## vector-based:
  if (purrr::is_null(data)) {
    data_tbl <- mcs_mileage_data(
      mileage = x,
      time = time,
      status = status,
      id = id
    )
  } else {
    ## data-based: only 'mileage' must be updated!
    data_tbl <- dplyr::mutate(data, mileage = x)
  }

  data_tbl <- dplyr::rename(data_tbl, x = .data$mileage)

  ## Set class and attribute w.r.t status; remove class "wt_mcs_mileage_data":
  if ("status" %in% names(data_tbl)) {
    class(data_tbl) <- c("wt_reliability_data", "wt_mcs_data", class(data_tbl)[-1])
    attr(data_tbl, "characteristic") <- "mileage"
  } else {
    class(data_tbl) <- c("wt_mcs_data", class(data_tbl)[-1])
  }

  # Remove attribute "mcs_characteristic":
  attr(data_tbl, "mcs_characteristic") <- NULL

  mcs_output <- list(
    data = data_tbl,
    sim_data = tibble::tibble(sim_mileage = sim_nums),
    model_estimation = list(
      mileage_distribution = par_list
    )
  )

  class(mcs_output) <- c("wt_mcs_mileage", class(mcs_output))

  mcs_output
}
