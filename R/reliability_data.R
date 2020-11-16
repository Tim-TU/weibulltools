#' Reliability data
#'
#' @description
#' Create consistent reliability data based on an existing data.frame/tibble
#' (preferred) or on multiple equal length vectors.
#'
#' @param data Either NULL or a data frame/tibble. If data is NULL, x, status and
#' id must be vectors containing the data. Otherwise x, status and id can be either
#' column names or column positions.
#' @param x Lifetime data, that means any characteristic influencing the reliability
#' of a product, e.g. operating time (days/months in service), mileage (km, miles),
#' load cycles.
#' @param status Binary data (0 or 1) indicating whether a unit is a right
#' censored observation (= 0) or a failure (= 1).
#' @param id Identification for every unit.
#'
#' @return A tibble with class \code{"reliability_data"} containing the following
#' columns:
#' \itemize{
#'   \item \code{x} Lifetime characteristic.
#'   \item \code{status} Binary data (0 or 1) indicating whether a unit is a right
#'     censored observation (= 0) or a failure (= 1).
#'   \item \code{id} Identification for every unit.
#' }
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar (1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#' id <- "XXXXXX"
#'
#' # Example 1: Based on existing data.frame/tibble
#' tbl <- tibble::tibble(x = cycles, status = state, id = id)
#' data <- reliability_data(tbl, x = x, status = status, id = id)
#'
#' # Example 2: Based on vectors
#' data_2 <- reliability_data(x = cycles, status = state, id = id)
#'
#' @export
reliability_data <- function(data = NULL, x, status, id = NULL) {
  if (purrr::is_null(data)) {
    if (!is_characteristic(x)) {
      stop("x must be numeric!")
    }

    if (!is_status(status)) {
      stop("status must be numeric! all elements must be either 0 or 1!")
    }

    if (length(x) < length(status) || length(x) < length(id)) {
      stop("length of x must be equal or greater than length of status and id!")
    }

    if (purrr::is_null(id)) {
      id <- paste0("ID", seq_along(x))
    }

    tbl <- tibble::tibble(x = x, status = status, id = id)
  } else {
    if (!is_characteristic(data[[substitute(x)]])) {
      stop("x must be numeric!")
    }

    if (!is_status(data[[substitute(status)]])) {
      stop("status must be numeric! all elements must be either 0 or 1!")
    }

    data <- tibble::as_tibble(data)

    tbl <- dplyr::select(data, x = {{x}}, status = {{status}}, id = {{id}})

    # Check if id is null without breaking if id is just a promise
    if (!"id" %in% names(tbl)) {
      tbl$id <- paste0("ID", seq_len(nrow(data)))
    }
  }

  class(tbl) <- c("reliability_data", class(tbl))

  tbl
}

is_characteristic <- function(x) {
  is.numeric(x)
}

is_status <- function(x) {
  is.numeric(x) && all(x %in% c(0, 1))
}
