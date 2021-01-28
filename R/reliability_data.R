#' Reliability Data
#'
#' @description
#' Create consistent reliability data based on an existing \code{data.frame}
#' (preferred) or on multiple equal length vectors.
#'
#' @param data Either \code{NULL} or a \code{data.frame}. If data is \code{NULL},
#' \code{x}, \code{status} and \code{id} must be vectors containing
#' the data. Otherwise \code{x}, \code{status} and \code{id} can be either column
#' names or column positions.
#' @param x Lifetime data, that means any characteristic influencing the reliability
#' of a product, e.g. operating time (days/months in service), mileage (km, miles),
#' load cycles.
#' @param status Binary data (0 or 1) indicating whether a unit is a right
#' censored observation (= 0) or a failure (= 1).
#' @param id Identification of every unit.
#' @param .keep_all If \code{TRUE} keep remaining variables in \code{data}.
#'
#' @return A tibble with class \code{wt_reliability_data} containing the following
#' columns (if \code{.keep_all = FALSE}):
#' \itemize{
#'   \item \code{x} : Lifetime characteristic.
#'   \item \code{status} : Binary data (0 or 1) indicating whether a unit is a right
#'     censored observation (= 0) or a failure (= 1).
#'   \item \code{id} : Identification for every unit.
#' }
#' If \code{.keep_all = TRUE}, the remaining columns of \code{data} are also preserved.
#'
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
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#' id <- "XXXXXX"
#'
#' data_4 <- reliability_data(
#'   x = cycles,
#'   status = state,
#'   id = id
#' )
#'
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
  } else {
    # Data based approach -----------------------------------------------------
    if (!is_characteristic(dplyr::select(data, x = {{x}})[[1]])) {
      stop("'x' must be numeric!")
    }

    if (!is_status(dplyr::select(data, status = {{status}})[[1]])) {
      stop("'status' must be numeric with elements 0 or 1!")
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
  attr(tbl, "characteristic") <- "x"

  tbl
}



#' @export
print.wt_reliability_data <- function(x, ...) {
  if (attr(x, "characteristic") == "x") {
    cat("Reliability Data:\n")
  } else {
    cat(
      "Reliability data with characteristic '",
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



get_characteristic <- function(x) {
  # x is reliability_data
  x[[attr(x, "characteristic")]]
}
