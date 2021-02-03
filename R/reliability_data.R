#' Reliability Data
#'
#' @description
#' Create consistent reliability data based on an existing `data.frame`
#' (preferred) or on multiple equal length vectors.
#'
#' @param data Either `NULL` or a `data.frame`. If data is `NULL`,
#' `x`, `status` and `id` must be vectors containing
#' the data. Otherwise `x`, `status` and `id` can be either column
#' names or column positions.
#' @param x Lifetime data, that means any characteristic influencing the reliability
#' of a product, e.g. operating time (days/months in service), mileage (km, miles),
#' load cycles.
#' @param status Binary data (0 or 1) indicating whether a unit is a right
#' censored observation (= 0) or a failure (= 1).
#' @param id Identification of every unit.
#' @param .keep_all If `TRUE` keep remaining variables in `data`.
#'
#' @return A tibble with class `wt_reliability_data` containing the following
#' columns (if `.keep_all = FALSE`):
#'
#' * `x` : Lifetime characteristic.
#' * `status` : Binary data (0 or 1) indicating whether a unit is a right
#'     censored observation (= 0) or a failure (= 1).
#' * `id` : Identification for every unit.
#'
#' If `.keep_all = TRUE`, the remaining columns of `data` are also preserved.
#'
#' If `!is.null(data)` the attribute `characteristic` holds the name of the
#' characteristic described by `x`. Otherwise it is set to `"x"`.
#'
#' @examples
#' # Example 1 -  Based on an existing data.frame/tibble and column names:
#' data <- reliability_data(
#'   data = shock,
#'   x = distance,
#'   status = status
#' )
#'
#' # Example 2 - Based on an existing data.frame/tibble and column positions:
#' data_2 <- reliability_data(
#'   data = shock,
#'   x = 1,
#'   status = 3
#' )
#'
#' # Example 3 - Keep all variables of the tibble/data.frame entered to argument data:
#' data_3 <- reliability_data(
#'   data = shock,
#'   x = distance,
#'   status = status,
#'   .keep_all = TRUE
#' )
#'
#' # Example 4 - Based on vectors:
#' cycles <- alloy$cycles
#' state <- alloy$status
#' id <- "XXXXXX"
#'
#' data_4 <- reliability_data(
#'   x = cycles,
#'   status = state,
#'   id = id
#' )
#'
#' @md
#' @export
reliability_data <- function(data = NULL,
                             x,
                             status,
                             id = NULL,
                             .keep_all = FALSE
) {

  if (purrr::is_null(data)) {
    # Vector based approach ----------------------------------------------------
    if (!is_characteristic(x)) {
      stop("'x' must be numeric!")
    }

    if (!is_status(status)) {
      stop("'status' must be numeric with elements 0 or 1!")
    }

    if (purrr::is_null(id)) {
      id <- paste0("ID", seq_along(x))
    }

    tbl <- tibble::tibble(x = x, status = status, id = id)

    characteristic <- "x"
  } else {
    # Data based approach -----------------------------------------------------
    if (!is_characteristic(dplyr::select(data, x = {{x}})[[1]])) {
      stop("'x' must be numeric!")
    }

    if (!is_status(dplyr::select(data, status = {{status}})[[1]])) {
      stop("'status' must be numeric with elements 0 or 1!")
    }

    x_def <- dplyr::enexpr(x)
    characteristic <- if (is.numeric(x_def)) {
      # Column index
      names(data)[x_def]
    } else {
      # Column name
      as.character(x_def)
    }

    data <- tibble::as_tibble(data)

    if (.keep_all) {
      tbl <- dplyr::rename(data, x = {{x}}, status = {{status}}, id = {{id}})
    } else {
      tbl <- dplyr::select(data, x = {{x}}, status = {{status}}, id = {{id}})
    }

    if (!"id" %in% names(tbl)) {
      tbl$id <- paste0("ID", seq_len(nrow(data)))
    }

    tbl <- dplyr::relocate(tbl, x, status, id)
  }

  class(tbl) <- c("wt_reliability_data", class(tbl))
  # Mark column x as characteristic
  attr(tbl, "characteristic") <- characteristic

  tbl
}



#' @export
print.wt_reliability_data <- function(x, ...) {
  if (attr(x, "characteristic") == "x") {
    cat("Reliability Data:\n")
  } else {
    cat(
      "Reliability Data with characteristic x: '",
      attr(x, "characteristic"),
      "':\n",
      sep = ""
    )
  }
  NextMethod()
}



is_characteristic <- function(x) {
  is.numeric(x)
}



is_status <- function(x) {
  is.numeric(x) && all(x %in% c(0, 1))
}
