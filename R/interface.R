#' Reliability data
#'
#' This function prepares a dataset for the reliability pipeline.
#'
#' @param data A data frame.
#' @param x The name of a column holding lifetime data. Lifetime data could
#' be every characteristic influencing the reliability of a product, e.g.
#' operating time (days/months in service), mileage (km, miles), load cycles.
#' @param event The name of a column holding binary data (0 or 1) indicating
#' whether unit \emph{i} is a right censored observation (= 0) or a failure (= 1).
#' @param id The name of a column holding the identification for every unit.
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#' id <- "XXXXXX"
#'
#' df <- data.frame(cycles = cycles, state = state, id = id)
#'
#' data <- reliability_data(df, x = cycles, event = state, id = id)
#'
#' @export
reliability_data <- function(data, x, event, id) {
  data <- dplyr::select(data, x = {{x}}, event = {{event}}, id = {{id}})

  class(data) <- c("reliability_data", class(data))

  data
}

#' Estimation of Failure Probabilities
#'
#' Estimate failure probabilities for uncensored or (multiple) right-censored
#' data.
#'
#' Depending on the \code{method} argument one of the following functions is
#' called:
#' \itemize{
#'   \item \code{"mr"}: \code{\link{mr_method}}. Works only for uncensored data.
#'   \item \code{"johnson"}: \code{\link{johnson_method}}.
#'   \item \code{"kaplan"}: \code{\link{kaplan_method}}.
#'   \item \code{"nelson"}: \code{\link{nelson_method}}.
#' }
#'
#' @param data A standardised data frame returned by \link{reliability_data}.
#' @param method Method used for estimating the failure probablities. See 'Details'.
#' @param options A list of named options passed to \code{<method>_method}. For
#' now there is just the option "method" with \code{method = "mr"}. See
#' \code{\link{mr_method}}.
#' @export
estimate_cdf <- function(data, ...) {
  UseMethod("estimate_cdf")
}

#' @export
estimate_cdf.default <- function(
  data, method = c("mr", "johnson", "kaplan", "nelson"), options = list()
) {
  # Use of generic saves test for class of data

  method <- match.arg(method)

  method_fun <- switch(
    method,
    "mr" = mr_method,
    "johnson" = johnson_method,
    "kaplan" = kaplan_method,
    "nelson" = nelson_method
  )

  args <- list(
    x = data$x,
    event = data$event,
    id = data$id
  )

  if (method == "mr") {
    args$method <- if (is.null(options$method)) "benard" else options$method
  }

  do.call(method_fun, args)
}

