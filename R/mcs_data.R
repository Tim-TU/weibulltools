#' MCS Mileage Data
#'
#' @description
#' Create consistent \emph{mcs_mileage_data} based on an existing \code{data.frame}
#' (preferred) or on multiple equal length vectors.
#'
#' @param data Either \code{NULL} or a \code{data.frame}. If data is \code{NULL},
#'   \code{mileage} and \code{time} (and if available, \code{status} and \code{id})
#'   must be vectors containing the data. Otherwise \code{mileage} and \code{time}
#'   (and \code{status} and \code{id}) can be either column names or column positions.
#' @param mileage Covered distances. For missing elements \code{NA} has to be used.
#' @param time Operating times. For missing elements \code{NA} has to be used.
#' @param status Optional argument. If used it has to be a vector of binary data
#'   (0 or 1) indicating whether unit i is a right censored observation (= 0) or a
#'   failure (= 1).
#' @param id Identification of every unit.
#' @param .keep_all If \code{TRUE} keep remaining variables in \code{data}.
#'
#' @return A tibble with class \code{wt_mcs_mileage_data} that is formed for the
#' downstream Monte Carlo Simulation of unknown covered distances
#' (see \code{\link{mcs_mileage}}. It contains the following columns (if
#' \code{.keep_all = FALSE}):
#' \itemize{
#'   \item \code{mileage} : Input mileages labeled as \emph{MCS characteristic}.
#'    \item \code{time} : Input operating times.
#'         \item \code{status} (\strong{optional}) :
#'           \itemize{
#'             \item If argument \code{status = NULL} column \code{status} does
#'               not exist.
#'             \item If argument \code{status} is provided the column contains
#'               the entered binary data (0 or 1).
#'           }
#'   \item \code{id} : Identification for every unit.
#' }
#' If \code{.keep_all = TRUE}, the remaining columns of \code{data} are also preserved.
#'
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
#' @export
mcs_mileage_data <- function(data = NULL,
                             mileage,
                             time,
                             status = NULL,
                             id = NULL,
                             .keep_all = FALSE
) {

  if (purrr::is_null(data)) {

    if (!is_characteristic(mileage)) {
      stop("'mileage' must be numeric!")
    }

    if (!is_characteristic(time)) {
      stop("'time' must be numeric!")
    }

    if (purrr::is_null(id)) {
      id <- paste0("ID", seq_along(mileage))
    }

    if (!purrr::is_null(status)) {
      if (!is_status(status)) {
        stop("'status' must be numeric with elements 0 or 1!")
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

    if (!is_characteristic(dplyr::select(data, {{mileage}})[[1]])) {
      stop("'mileage' must be numeric!")
    }

    if (!is_characteristic(dplyr::select(data, {{time}})[[1]])) {
      stop("'time' must be numeric!")
    }

    data <- tibble::as_tibble(data)

    if (.keep_all) {
      tbl <- dplyr::rename(data,
                           mileage = {{mileage}},
                           status = {{status}},
                           time = {{time}},
                           id = {{id}}
      )
    } else {
      tbl <- dplyr::select(data,
                           mileage = {{mileage}},
                           status = {{status}},
                           time = {{time}},
                           id = {{id}}
      )
    }

    if (!("id" %in% names(tbl))) {
      tbl$id <- paste0("ID", seq_len(nrow(data)))
    }

    if ("status" %in% names(tbl)) {
      if (!is_status(tbl$status)) {
        stop("'status' must be numeric with elements 0 or 1!")
      }
    }

    tbl <- dplyr::relocate(tbl, mileage, time, status, id)
  }

  class(tbl) <- c("wt_mcs_mileage_data", "wt_mcs_data", class(tbl))
  # Mark column mileage as characteristic
  attr(tbl, "mcs_characteristic") <- "mileage"

  tbl
}



#' @export
print.wt_mcs_mileage_data <- function(x, ...) {
  if (attr(x, "mcs_characteristic") %in% c("mileage", "dates")) {
    cat(
      "MCS Data with characteristic '",
      attr(x, "mcs_characteristic"),
      "':\n",
      sep = ""
    )
  } else {
    cat("MCS Data:\n")
  }
  NextMethod()
}



get_mcs_characteristic <- function(x) {
  # x is wt.mcs_data
  x[[attr(x, "mcs_characteristic")]]
}
