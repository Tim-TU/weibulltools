#' MCS Delay Data
#'
#' @description
#' Create consistent `mcs_delay_data` based on an existing `data.frame` (preferred)
#' or on multiple equal length vectors.
#'
#' @param data Either `NULL` or a `data.frame`. If data is `NULL`, `date_1`, `date_2`,
#' `time`, `status` and `id` must be vectors containing the data. Otherwise `date_1`,
#' `date_2`, `time`, `status` and `id` can be either column names or column positions.
#' @param date_1 A date of class `character` or `Date` in the format "yyyy-mm-dd",
#' representing the earlier of the two dates belonging to a particular delay.
#' Use `NA` for missing elements.
#'
#' If more than one delay is to be considered, use a list for the vector-based
#' approach and a vector of column names or positions for the data-based approach.
#' The first element is the earlier date of the first delay, the second element is the
#' earlier date of the second delay, and so forth (see 'Examples').
#' @param date_2 A date of class `character` or `Date` in the format "yyyy-mm-dd".
#' `date_2` is the counterpart of `date_1` and is used the same as `date_1`, just with
#' the later date(s) of the particular delay(s). Use `NA` for missing elements.
#' @param time Operating times. Use `NA` for missing elements.
#' @param status Optional argument. If used, it must contain binary data
#' (0 or 1) indicating whether a unit is a right censored observation (= 0) or a
#' failure (= 1).
#'
#' If `status` is provided, class `wt_reliability_data` is assigned to the
#' output of [mcs_delay], which enables the direct application of [estimate_cdf]
#' on operating times.
#' @param id Identification of every unit.
#' @param .keep_all If `TRUE` keep remaining variables in `data`.
#'
#' @return A `tibble` with class `wt_mcs_delay_data` that is formed for the downstream
#' Monte Carlo method [mcs_delay].
#' It contains the following columns (if `.keep_all = FALSE`):
#'
#' * Column(s) preserving the input of `date_1`. For the vector-based approach
#'     with unnamed input, column name(s) is (are) `date_1`
#'     (`date_1.1`, `date_1.2`, `...`, `date_1.i`).
#' * Column(s) preserving the input of `date_2`. For the vector-based approach
#'     with unnamed input, column name(s) is (are) `date_2`
#'     (`date_2.1`, `date_2.2`, `...`, `date_2.i`).
#' * `time` : Input operating times.
#' * `status` (**optional**) :
#'   * If `is.null(status)` column `status` does not exist.
#'   * If `status` is provided the column contains the entered binary
#'     data (0 or 1).
#' * `id` : Identification for every unit.
#'
#' If `.keep_all = TRUE`, the remaining columns of `data` are also preserved.
#'
#' The attributes `mcs_start_dates` and `mcs_end_dates` hold the name(s) of the
#' column(s) that preserve the input of `date_1` and `date_2`.
#'
#' @seealso [dist_delay] for the determination of a parametric delay distribution
#' and [mcs_delay] for the Monte Carlo method with respect to delays.
#'
#' @examples
#' # Example 1 -  Based on an existing data.frame/tibble and column names:
#' mcs_tbl <- mcs_delay_data(
#'   data = field_data,
#'   date_1 = production_date,
#'   date_2 = registration_date,
#'   time = dis,
#'   status = status
#' )
#'
#' # Example 2 - Based on an existing data.frame/tibble and column positions:
#' mcs_tbl_2 <- mcs_delay_data(
#'   data = field_data,
#'   date_1 = 7,
#'   date_2 = 8,
#'   time = 2,
#'   id = 1
#' )
#'
#' # Example 3 - Keep all variables of the tibble/data.frame entered to argument data:
#' mcs_tbl_3 <- mcs_delay_data(
#'   data = field_data,
#'   date_1 = production_date,
#'   date_2 = registration_date,
#'   time = dis,
#'   status = status,
#'   id = vin,
#'   .keep_all = TRUE
#' )
#'
#' # Example 4 - For multiple delays (data-based):
#' mcs_tbl_4 <- mcs_delay_data(
#'   data = field_data,
#'   date_1 = c(production_date, repair_date),
#'   date_2 = c(registration_date, report_date),
#'   time = dis,
#'   status = status
#' )
#'
#' # Example 5 - Based on vectors:
#' mcs_tbl_5 <- mcs_delay_data(
#'   date_1 = field_data$production_date,
#'   date_2 = field_data$registration_date,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin
#' )
#'
#' # Example 6 - For multiple delays (vector-based):
#' mcs_tbl_6 <- mcs_delay_data(
#'   date_1 = list(field_data$production_date, field_data$repair_date),
#'   date_2 = list(field_data$registration_date, field_data$report_date),
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin
#' )
#'
#' # Example 7 - For multiple delays (vector-based with named dates):
#' mcs_tbl_7 <- mcs_delay_data(
#'   date_1 = list(d11 = field_data$production_date, d12 = field_data$repair_date),
#'   date_2 = list(d21 = field_data$registration_date, d22 = field_data$report_date),
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin
#' )
#'
#' @md
#'
#' @export
mcs_delay_data <- function(data = NULL,
                           date_1,
                           date_2,
                           time,
                           status = NULL,
                           id = NULL,
                           .keep_all = FALSE
) {

  if (purrr::is_null(data)) {
    # Vector based approach ----------------------------------------------------
    if (!is_characteristic(time)) {
      stop("'time' must be numeric!", call. = FALSE)
    }

    if (purrr::is_null(id)) {
      id <- paste0("ID", seq_along(time))
    }

    ## Convert date_1 and date_2 to lists if they are vectors:
    if (!is.list(date_1)) date_1 <- list(date_1)
    if (!is.list(date_2)) date_2 <- list(date_2)

    ## Checks:
    purrr::walk2(
      .x = date_1,
      .y = date_2,
      function(e1, e2) {
        ### Check for different length in date_1 and date_2:
        if (length(e1) != length(e2)) {
          stop(
            "Elements of 'date_1' and 'date_2' differ in length!",
            call. = FALSE
          )
        }
        ### Check for class of date_1:
        if (!(class(e1) %in% c("Date", "character"))) {
          stop(
            "'date_1' must be of class 'Date' or 'character'!",
            call. = FALSE
          )
        }
        ### Check for class date_2:
        if (!(class(e2) %in% c("Date", "character"))) {
          stop(
            "'date_2' must be of class 'Date' or 'character'!",
            call. = FALSE
          )
        }
      }
    )

    ## One list for the dates:
    dates_list <- c(date_1, date_2)
    names_dates_list <- names(dates_list)

    if (any(trimws(names_dates_list) == "")) {
      stop(
        "Either all or no elements of 'date_1' and 'date_2' must have names!",
        call. = FALSE
      )
    }

    if (purrr::is_null(names_dates_list)) {
      if (length(dates_list) == 2L) {
        names_dates_list <- c("date_1", "date_2")
      } else {
        index <- seq_along(date_1)
        names_dates_list <- c(
          paste0("date_1.", index),
          paste0("date_2.", index)
        )
      }
    }

    if (!purrr::is_null(status)) {
      if (!is_status(status)) {
        stop("'status' must be numeric with elements 0 or 1!", call. = FALSE)
      }
      tbl_list <- c(dates_list, list(time, status, id))
      names(tbl_list) <- c(names_dates_list, "time", "status", "id")
      tbl <- tibble::as_tibble(tbl_list)
    } else {
      tbl_list <- c(dates_list, list(time, id))
      names(tbl_list) <- c(names_dates_list, "time", "id")
      tbl <- tibble::as_tibble(tbl_list)
    }

    ## Preparation of 'mcs_start_dates' and 'mcs_end_dates':
    n <- length(names_dates_list)
    characteristic_1 <- names_dates_list[1:(n / 2)]
    characteristic_2 <- names_dates_list[((n / 2) + 1):n]

  } else {
    # Data based approach ------------------------------------------------------
    ## Checks:
    if (!is_characteristic(dplyr::select(data, {{time}})[[1]])) {
      stop("'time' must be numeric!", call. = FALSE)
    }

    purrr::walk(
      dplyr::select(data, {{date_1}}, {{date_2}}),
      ~ if (!(class(.) %in% c("Date", "character"))) {
        stop(
          "Columns specified in 'date_1' and 'date_2' must be of class",
          " 'Date' or 'character'!",
          call. = FALSE
        )
      }
    )

    ## Preparation of 'mcs_start_dates' and 'mcs_end_dates':
    characteristic_1 <- dplyr::select(data, {{date_1}}) %>% names()
    characteristic_2 <- dplyr::select(data, {{date_2}}) %>% names()

    ## Check for same length in 'date_1' and 'date_2':
    if (length(characteristic_1) != length(characteristic_2)) {
      stop(
        "The same number of columns must be specified in 'date_1' and 'date_2'!",
        call. = FALSE
      )
    }

    data <- tibble::as_tibble(data)

    if (.keep_all) {
      ## If date_1 and date_2 are col positions rename() fails since cols must be named:
      tbl <- dplyr::rename(
        data,
        time = {{time}},
        status = {{status}},
        id = {{id}}
      )
    } else {
      tbl <- dplyr::select(
        data,
        {{characteristic_1}},
        {{characteristic_2}},
        time = {{time}},
        status = {{status}},
        id = {{id}}
      )
    }

    if (!("id" %in% names(tbl))) {
      tbl$id <- paste0("ID", seq_len(nrow(data)))
    }

    if ("status" %in% names(tbl)) {
      if (!is_status(tbl$status)) {
        stop("'status' must be numeric with elements 0 or 1!", call. = FALSE)
      }
    }

    ## For col positions relocate() should use names, since locations may no longer exist:
    names_tbl <- names(tbl)[!(names(tbl) %in% c(characteristic_1, characteristic_2))]
    ## MCS characteristics should have the first column positions:
    tbl <- dplyr::relocate(tbl, {{characteristic_1}}, {{characteristic_2}}, {{names_tbl}})
  }

  class(tbl) <- c("wt_mcs_delay_data", class(tbl))
  # Mark date columns as characteristic
  attr(tbl, "mcs_start_dates") <- characteristic_1
  attr(tbl, "mcs_end_dates") <- characteristic_2

  tbl
}



#' MCS Mileage Data
#'
#' @description
#' Create consistent `mcs_mileage_data` based on an existing `data.frame` (preferred)
#' or on multiple equal length vectors
#'
#' @param data Either `NULL` or a `data.frame`. If data is `NULL`, `mileage`, `time`,
#' `status` and `id` must be vectors containing the data. Otherwise `mileage`, `time`,
#' `status` and `id` can be either column names or column positions.
#' @param mileage Covered distances. Use `NA` for missing elements.
#' @param time Operating times. Use `NA` for missing elements.
#' @param status Optional argument. If used, it must contain binary data
#' (0 or 1) indicating whether a unit is a right censored observation (= 0) or a
#' failure (= 1).
#'
#' If `status` is provided, class `wt_reliability_data` is assigned to the
#' output of [mcs_mileage], which enables the direct application of [estimate_cdf]
#' on distances.
#' @param id Identification of every unit.
#' @param .keep_all If `TRUE` keep remaining variables in `data`.
#'
#' @return A `tibble` with class `wt_mcs_mileage_data` that is formed for the downstream
#' Monte Carlo method [mcs_mileage].
#' It contains the following columns (if `.keep_all = FALSE`):
#'
#' * `mileage` : Input mileages.
#' * `time` : Input operating times.
#' * `status` (**optional**) :
#'   * If `is.null(status)` column `status` does not exist.
#'   * If `status` is provided the column contains the entered binary
#'     data (0 or 1).
#' * `id` : Identification for every unit.
#'
#' If `.keep_all = TRUE`, the remaining columns of `data` are also preserved.
#'
#' The attribute `mcs_characteristic` is set to `"mileage"`.
#'
#' @seealso [dist_mileage] for the determination of a parametric annual mileage
#' distribution and [mcs_mileage] for the Monte Carlo method with respect to
#' unknown distances.
#'
#' @examples
#' # Example 1 -  Based on an existing data.frame/tibble and column names:
#' mcs_tbl <- mcs_mileage_data(
#'   data = field_data,
#'   mileage = mileage,
#'   time = dis,
#'   status = status
#' )
#'
#' # Example 2 - Based on an existing data.frame/tibble and column positions:
#' mcs_tbl_2 <- mcs_mileage_data(
#'   data = field_data,
#'   mileage = 3,
#'   time = 2,
#'   id = 1
#' )
#'
#' # Example 3 - Keep all variables of the tibble/data.frame entered to argument data:
#' mcs_tbl_3 <- mcs_mileage_data(
#'   data = field_data,
#'   mileage = mileage,
#'   time = dis,
#'   status = status,
#'   id = vin,
#'   .keep_all = TRUE
#' )
#'
#' # Example 4 - Based on vectors:
#' mcs_tbl_4 <- mcs_mileage_data(
#'   mileage = field_data$mileage,
#'   time = field_data$dis,
#'   status = field_data$status,
#'   id = field_data$vin
#' )
#'
#' @md
#'
#' @export
mcs_mileage_data <- function(data = NULL,
                             mileage,
                             time,
                             status = NULL,
                             id = NULL,
                             .keep_all = FALSE
) {

  if (purrr::is_null(data)) {
    # Vector based approach ----------------------------------------------------
    if (!is_characteristic(mileage)) {
      stop("'mileage' must be numeric!", call. = FALSE)
    }

    if (!is_characteristic(time)) {
      stop("'time' must be numeric!", call. = FALSE)
    }

    if (purrr::is_null(id)) {
      id <- paste0("ID", seq_along(mileage))
    }

    if (!purrr::is_null(status)) {
      if (!is_status(status)) {
        stop("'status' must be numeric with elements 0 or 1!", call. = FALSE)
      }
      tbl <- tibble::tibble(
        mileage = mileage,
        time = time,
        status = status,
        id = id
      )
    } else {
      tbl <- tibble::tibble(
        mileage = mileage,
        time = time,
        id = id
      )
    }

  } else {
    # Data based approach ------------------------------------------------------
    if (!is_characteristic(dplyr::select(data, {{mileage}})[[1]])) {
      stop("'mileage' must be numeric!", call. = FALSE)
    }

    if (!is_characteristic(dplyr::select(data, {{time}})[[1]])) {
      stop("'time' must be numeric!", call. = FALSE)
    }

    data <- tibble::as_tibble(data)

    if (.keep_all) {
      tbl <- dplyr::rename(
        data,
        mileage = {{mileage}},
        time = {{time}},
        status = {{status}},
        id = {{id}}
      )
    } else {
      tbl <- dplyr::select(
        data,
        mileage = {{mileage}},
        time = {{time}},
        status = {{status}},
        id = {{id}}
      )
    }

    if (!("id" %in% names(tbl))) {
      tbl$id <- paste0("ID", seq_len(nrow(data)))
    }

    if ("status" %in% names(tbl)) {
      if (!is_status(tbl$status)) {
        stop("'status' must be numeric with elements 0 or 1!", call. = FALSE)
      }

      tbl <- dplyr::relocate(tbl, "mileage", "time", "status", "id")
    } else {
      tbl <- dplyr::relocate(tbl, "mileage", "time", "id")
    }
  }

  class(tbl) <- c("wt_mcs_mileage_data", class(tbl))
  # Mark column mileage as characteristic
  attr(tbl, "mcs_characteristic") <- "mileage"

  tbl
}



#' @export
print.wt_mcs_mileage_data <- function(x, ...) {
  cat(
    "MCS Mileage Data with characteristic '",
    attr(x, "mcs_characteristic"),
    "':\n",
    sep = ""
  )
  NextMethod()
}



#' @export
print.wt_mcs_delay_data <- function(x, ...) {
  if (length(attr(x, "mcs_start_dates")) == 1L) {
    start <- "start date: "
    end <- "end date: "
  } else {
    start <- "start dates: "
    end <- "end dates: "
  }
  cat(
    "MCS Delay Data with characteristics\n",
    start, paste0("'", attr(x, "mcs_start_dates"), "'", collapse = ", "),
    "\tand\n", # 1 tab!
    end, paste0("'", attr(x, "mcs_end_dates"), "'", collapse = ", "),
    "\n",
    sep = ""
  )
  NextMethod()
}
