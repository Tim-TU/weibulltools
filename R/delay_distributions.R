#' Parameter Estimation of a Delay Distribution
#'
#' @description
#' This function models a delay (in days) random variable (e.g. in logistic,
#' registration, report) using a supposed continuous distribution. First, the
#' row-wise differences in days of the related date columns are calculated and then
#' the parameter(s) of the assumed distribution is (are) estimated with maximum
#' likelihood. See 'Details' for more information.
#'
#' @details
#' The distribution parameter(s) is (are) determined on the basis of complete
#' cases, i.e. there is no `NA` (row-wise) in one of the related date columns.
#' Time differences less than or equal to zero are not considered as well.
#'
#' @param x A `tibble` of class `wt_mcs_delay_data` returned by [mcs_delay_data].
#' @param distribution Supposed distribution of the respective delay.
#' @template dots
#'
#' @return A list with class `wt_delay_estimation` which contains:
#'
#' * `coefficients` : A named vector of estimated parameter(s).
#' * `delay` : A numeric vector of element-wise computed differences in days.
#' * `distribution` : Specified distribution.
#'
#' If more than one delay was considered in [mcs_delay_data], the resulting output
#' is a list with class `wt_delay_estimation_list`. In this case each list element
#' has class `wt_delay_estimation` and the items listed above, are included.
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
#' # Example 1 - Delay in registration:
#' params_delay_regist  <- dist_delay(
#'   x = mcs_tbl_1,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Delay in report:
#' params_delay_report  <- dist_delay(
#'   x = mcs_tbl_2,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - Delays in registration and report with same distribution:
#' params_delays  <- dist_delay(
#'   x = mcs_tbl_both,
#'   distribution = "lognormal"
#' )
#'
#' # Example 4 - Delays in registration and report with different distributions:
#' params_delays_2  <- dist_delay(
#'   x = mcs_tbl_both,
#'   distribution = c("lognormal", "exponential")
#' )
#'
#' @md
#'
#' @export
dist_delay <- function(...) {
  UseMethod("dist_delay")
}



#' @rdname dist_delay
#'
#' @export
dist_delay.wt_mcs_delay_data <- function(
                                    ...,
                                    x,
                                    distribution = c("lognormal", "exponential")
) {

  # Check that '...' argument is not used:
  check_dots(...)

  # Extract 'mcs_start_dates' and 'mcs_end_dates' columns as list:
  date_1_names <- attr(x, "mcs_start_dates")
  date_2_names <- attr(x, "mcs_end_dates")

  date_1 <- dplyr::select(x, {{date_1_names}})
  date_2 <- dplyr::select(x, {{date_2_names}})

  # Use default method:
  dist_delay.default(
    date_1 = date_1,
    date_2 = date_2,
    distribution = distribution
  )
}



#' Parameter Estimation of a Delay Distribution
#'
#' @description
#' This function models a delay (in days) random variable (e.g. in logistic,
#' registration, report) using a supposed continuous distribution. First, the
#' element-wise differences in days of both vectors `date_1` and `date_2` are
#' calculated and then the parameter(s) of the assumed
#' distribution is (are) estimated with maximum likelihood. See 'Details' for
#' more information.
#'
#' @details
#' The distribution parameter(s) is (are) determined on the basis of complete
#' cases, i.e. there is no `NA` in one of the related vector elements
#' `c(date_1[i], date_2[i])`. Time differences less than or equal to zero are
#' not considered as well.
#'
#' @param date_1 A vector of class `character` or `Date`, in the format "yyyy-mm-dd",
#' representing the earlier of the two dates belonging to a particular delay.
#' Use `NA` for missing elements.
#'
#' If more than one delay is to be considered, use a list where the first element
#' is the earlier date of the first delay, the second element is the earlier date
#' of the second delay, and so forth (see 'Examples').
#' @param date_2 A vector of class `character` or `Date` in the format "yyyy-mm-dd".
#' `date_2` is the counterpart of `date_1` and is used the same as `date_1`, just with
#' the later date(s) of the particular delay(s). Use `NA` for missing elements.
#' @inheritParams dist_delay
#'
#' @return A list with class `wt_delay_estimation` which contains:
#'
#' * `coefficients` : A named vector of estimated parameter(s).
#' * `delay` : A numeric vector of element-wise computed differences in days.
#' * `distribution` : Specified distribution.
#'
#' If more than one delay was considered, the resulting output is a list with class
#' `wt_delay_estimation_list`. In this case each list element has class
#' `wt_delay_estimation` and the items listed above, are included.
#'
#' @seealso [dist_delay]
#'
#' @examples
#' # Example 1 - Delay in registration:
#' params_delay_regist  <- dist_delay(
#'   date_1 = field_data$production_date,
#'   date_2 = field_data$registration_date,
#'   distribution = "lognormal"
#' )
#'
#' # Example 2 - Delay in report:
#' params_delay_report  <- dist_delay(
#'   date_1 = field_data$repair_date,
#'   date_2 = field_data$report_date,
#'   distribution = "exponential"
#' )
#'
#' # Example 3 - Delays in registration and report with same distribution:
#' params_delays  <- dist_delay(
#'   date_1 = list(field_data$production_date, field_data$repair_date),
#'   date_2 = list(field_data$registration_date, field_data$report_date),
#'   distribution = "lognormal"
#' )
#'
#' # Example 4 - Delays in registration and report with different distributions:
#' params_delays_2  <- dist_delay(
#'   date_1 = list(field_data$production_date, field_data$repair_date),
#'   date_2 = list(field_data$registration_date, field_data$report_date),
#'   distribution = c("lognormal", "exponential")
#' )
#'
#' @md
#'
#' @export
dist_delay.default <- function(...,
                               date_1,
                               date_2,
                               distribution = c("lognormal", "exponential")
) {

  # Check that '...' argument is not used:
  check_dots(...)

  # Convert date_1 and date_2 to lists if they are vectors:
  if (!is.list(date_1)) date_1 <- list(date_1)
  if (!is.list(date_2)) date_2 <- list(date_2)

  # Default for distribution if not specified:
  if (missing(distribution)) {
    distribution <- "lognormal"
  }

  # Checks:
  ## Check for (multiple) distributions:
  distribution <- match.arg(distribution, several.ok = TRUE)

  ## Compare length of date_1 and distribution:
  if (!(length(distribution) == length(date_1) || length(distribution) == 1L)) {
    stop(
      "'distribution' must be either length one or 'length(date_1)'",
      call. = FALSE
    )
  }

  ## Check for different length in date_1 and date_2:
  if (length(unique(lengths(c(date_1, date_2)))) != 1L) {
    stop(
      "All elements of 'date_1' and 'date_2' must have the same length!",
      call. = FALSE
    )
  }

  # Do dist_delay_() with respect to the number of delays:
  if (length(date_1) == 1L) {
    ## One delay is to be considered:
    ### unlist kills class "Date" -> purrr::reduce(., "c")
    dist_estimation_list <- dist_delay_(
      purrr::reduce(date_1, "c"),
      purrr::reduce(date_2, "c"),
      distribution = distribution
    )
  } else {
    ## Multiple delays are to be considered:
    ### map over dates and distribution(s):
    dist_estimation_list <- purrr::pmap(
      list(
        date_1,
        date_2,
        distribution
      ),
      dist_delay_
    )

    # Prepare output with respect to multiple delays:
    names(dist_estimation_list) <- paste0("delay_", seq_along(dist_estimation_list))
    class(dist_estimation_list) <- c("wt_delay_estimation_list",
                                     class(dist_estimation_list))
  }

  dist_estimation_list
}



# Helper function that performs the estimation of a parametric delay distribution:
dist_delay_ <- function(date_1,
                        date_2,
                        distribution = c("lognormal", "exponential")
) {

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
    stop(
      "All date differences are 'NA'. No parameters can be estimated!",
      call. = FALSE
    )
  }
  ## any or all delays are smaller or equal to zero:
  if (any(t_delay <= 0, na.rm = TRUE)) {
    if (all(t_delay <= 0, na.rm = TRUE)) {
      ### all:
      stop(
        "All date differences are smaller or equal to 0. ",
        "No parameters can be estimated!",
        call. = FALSE
      )
    } else {
      ### any:
      warning(
        "At least one of the date differences is smaller or equal to 0 and is ",
        "ignored for the estimation step!",
        call. = FALSE
      )
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

  class(dist_output) <- c("wt_delay_estimation", class(dist_output))
  return(dist_output)
}



#' @export
print.wt_delay_estimation <- function(x,
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



#' @export
print.wt_delay_estimation_list <- function(x,
                                           digits = max(
                                             3L,
                                             getOption("digits") - 3L
                                           ),
                                           ...
) {
  cat(paste("List of", length(x), "delay estimations:\n"))
  cat("\n")
  purrr::walk2(x, seq_along(x), function(x, y) {
    cat(paste0("Delay distribution ", y, ": ", "'", x$distribution, "'", "\n"))
    print(x)
    cat("\n")
  })
  invisible(x)
}



#' Parameter Estimation of the Delay in Registration Distribution
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `dist_delay_register()` is no longer under active development, switching to
#' [dist_delay] is recommended.
#'
#' @details
#' This function introduces a delay random variable by calculating the time
#' difference between the registration and production date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using maximum likelihood.
#'
#' @param date_prod A vector of class `character` or `Date`, in the
#'   format "yyyy-mm-dd", indicating the date of production of a unit.
#'   Use `NA` for missing elements.
#' @param date_register A vector of class `character` or `Date`, in
#'   the format "yyyy-mm-dd", indicating the date of registration of a unit.
#'   Use `NA` for missing elements.
#' @param distribution Supposed distribution of the random variable. Only
#'   `"lognormal"`is implemented.
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
    stop(
      "All differences are NA. No parameters can be estimated!",
      call. = FALSE
    )
  }
  # all smaller or equal to zero:
  if (all(t_regist <= 0, na.rm = TRUE)) {
    stop(
      "All differences are smaller or equal to 0. No parameters can be estimated!",
      call. = FALSE
    )
  }
  # any smaller or equal to zero:
  if (!all(t_regist <= 0, na.rm = TRUE) && any(t_regist <= 0, na.rm = TRUE)) {
    warning(
      "At least one of the time differences is smaller or equal to 0 and is",
      " ignored for the estimation step!",
      call. = FALSE
    )

    t_regist <- t_regist[t_regist > 0]
  }

  # sample size used for the computation of the population standard deviation.
  n <- length(!is.na(t_regist))
  ml_loc <- mean(log(t_regist), na.rm = TRUE)
  ml_sc <- stats::sd(log(t_regist), na.rm = TRUE) * (n - 1) / n

  estimates <- c(loc = ml_loc, sc = ml_sc)

  return(estimates)
}



#' Parameter Estimation of the Delay in Report Distribution
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `dist_delay_report()`is no longer under active development, switching to
#' [dist_delay] is recommended.
#'
#' @details
#' This function introduces a delay random variable by calculating the time
#' difference between the report and repair date for the sample units
#' and afterwards estimates the parameter(s) of a supposed distribution,
#' using maximum likelihood.
#'
#' @inheritParams dist_delay_register
#'
#' @param date_repair a vector of class `character` or `Date`, in the
#'   format "yyyy-mm-dd", indicating the date of repair of a failed unit.
#'   Use `NA` for missing elements.
#' @param date_report a vector of class `character` or `Date`, in the
#'   format "yyyy-mm-dd", indicating the date of report of a failed unit.
#'   Use `NA` for missing elements.
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
    stop(
      "All differences are NA. No parameters can be estimated!",
      call. = FALSE
    )
  }
  # all smaller or equal to zero:
  if (all(t_report <= 0, na.rm = TRUE)) {
    stop(
      "All differences are smaller or equal to 0. No parameters can be estimated!",
      call. = FALSE
    )
  }
  # any smaller or equal to zero:
  if (!all(t_report <= 0, na.rm = TRUE) && any(t_report <= 0, na.rm = TRUE)) {
    warning(
      "At least one of the time differences is smaller or equal to 0 and is",
      " ignored for the estimation step!",
      call. = FALSE
    )

    t_report <- t_report[t_report > 0]
  }

  # sample size used for the computation of the population standard deviation.
  n <- length(!is.na(t_report))
  ml_loc <- mean(log(t_report), na.rm = TRUE)
  ml_sc <- stats::sd(log(t_report), na.rm = TRUE) * (n - 1) / n

  estimates <- c(loc = ml_loc, sc = ml_sc)

  return(estimates)
}
