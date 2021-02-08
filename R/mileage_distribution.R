#' Parameter Estimation of an Annual Mileage Distribution
#'
#' @description
#' This function models a mileage random variable on an annual basis with respect
#' to a supposed continuous distribution. First, the distances are calculated for
#' one year (365 days) using a linear relationship between the distance and
#' operating time. Second, the parameter(s) of the assumed distribution are
#' estimated with maximum likelihood. See 'Details' for more information.
#'
#' @details
#' The distribution parameter(s) is (are) determined on the basis of complete
#' cases, i.e. there is no `NA` (row-wise) in one of the related columns `mileage`
#' and `time`. Distances and operating times less than or equal to zero are not
#' considered as well.
#'
#' **Assumption of linear relationship**: Imagine a component in a vehicle
#'   has endured a distance of 25000 kilometers (km) in 500 days (d), the annual
#'   distance of this unit is \deqn{25000 km \cdot (\frac{365 d} {500 d}) = 18250 km}{%
#'                 25000 km * (365 d / 500 d) = 18250 km}
#'
#' @param x A `tibble` of class `wt_mcs_mileage_data` returned by [mcs_mileage_data].
#' @param distribution Supposed distribution of the annual mileage.
#' @template dots
#'
#' @return A list with class `wt_mileage_estimation` which contains:
#'
#' * `coefficients` : A named vector of estimated parameter(s).
#' * `miles_annual` : A numeric vector of element-wise computed annual distances
#'   using the linear relationship described in 'Details'.
#' * `distribution` : Specified distribution.
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
#' # Example 1 - Assuming lognormal annual mileage distribution:
#' params_mileage_annual <- dist_mileage(
#'   x = mcs_tbl,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Assuming exponential annual mileage distribution:
#' params_mileage_annual_2 <- dist_mileage(
#'   x = mcs_tbl,
#'   distribution = "exponential"
#' )
#'
#' @md
#'
#' @export
dist_mileage <- function(x, distribution, ...) {
  UseMethod("dist_mileage")
}



#' @rdname dist_mileage
#'
#' @export
dist_mileage.wt_mcs_mileage_data <- function(
                                   x,
                                   distribution = c("lognormal", "exponential"),
                                   ...
) {

  mileage <- x$mileage
  time <- x$time

  # Use default method:
  dist_mileage.default(
    x = mileage,
    time = time,
    distribution = distribution
  )
}



#' Parameter Estimation of an Annual Mileage Distribution
#'
#' @inherit dist_mileage description return
#'
#' @details
#' The distribution parameter(s) is (are) determined on the basis of complete cases,
#' i.e. there is no `NA` in one of the related vector elements
#' `c(mileage[i], time[i])`. Distances and operating times less than or equal
#' to zero are not considered as well.
#'
#' **Assumption of linear relationship**: Imagine a component in a vehicle
#'   has endured a distance of 25000 kilometers (km) in 500 days (d), the annual
#'   distance of this unit is \deqn{25000 km \cdot (\frac{365 d} {500 d}) = 18250 km}{%
#'                 25000 km * (365 d / 500 d) = 18250 km}
#'
#' @inheritParams dist_mileage
#' @param x A numeric vector of distances covered. Use `NA` for missing elements.
#' @param time A numeric vector of operating times. Use `NA` for missing elements.
#'
#' @seealso [dist_mileage]
#'
#' @examples
#' # Example 1 - Assuming lognormal annual mileage distribution:
#' params_mileage_annual <- dist_mileage(
#'   x = field_data$mileage,
#'   time = field_data$dis,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Assuming exponential annual mileage distribution:
#' params_mileage_annual_2 <- dist_mileage(
#'   x = field_data$mileage,
#'   time = field_data$dis,
#'   distribution = "exponential"
#' )
#'
#' @md
#'
#' @export
dist_mileage.default <- function(x,
                                 time,
                                 distribution = c("lognormal", "exponential"),
                                 ...
) {

  # Checks:
  ## Distribution check:
  distribution <- match.arg(distribution)

  ## Check for negative mileage, stop if TRUE:
  if (any(x < 0, na.rm = TRUE)) {
    stop(
      "Elements with negative distances are not meaningful and must be removed!"
    )
  }

  # Do dist_mileage_():
  dist_mileage_(
    x = x,
    time = time,
    distribution = distribution
  )
}



# Helper function that performs the estimation of an annual mileage distribution:
dist_mileage_ <- function(x,
                          time,
                          distribution
) {

  # Defining annual distance variable (for estimation) and origin variable (output):
  miles_annual <- miles_annual_origin <- (x / time) * 365

  # Checks:

  ## case of Inf, i.e. time is 0: could be handled with `is.infinite()`
  if (any(is.infinite(miles_annual))) {
    warning(
      "At least one computed annual distance is infinite and is ignored ",
      "for the estimation step!",
    )

    miles_annual <- miles_annual[!is.infinite(miles_annual)]
  }

  ## all NA:
  if (all(is.na(miles_annual))) {
    stop(
      "All computed annual distances are 'NA'. No parameters can be estimated!"
    )
  }

  ## any or all annual distances are smaller or equal to zero:
  if (any(miles_annual <= 0, na.rm = TRUE)) {

    if (all(miles_annual <= 0, na.rm = TRUE)) {
      ### all:
      stop(
        "All computed annual distances are smaller or equal to 0. ",
        "No parameters can be estimated!"
      )
    } else {
      ### any:
      warning(
        "At least one computed annual distance is smaller or equal to 0 ",
        "and is ignored for the estimation step!"
      )

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

  class(dist_output) <- c("wt_mileage_estimation", class(dist_output))

  return(dist_output)
}



#' @export
print.wt_mileage_estimation <- function(x,
                                        digits = max(
                                          3L,
                                          getOption("digits") - 3L
                                        ),
                                        ...
) {
  cat("Coefficients:\n")
  print(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  invisible(x)
}
