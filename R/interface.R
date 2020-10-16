#' Reliability data
#'
#' This function prepares a dataset for the reliability pipeline.
#'
#' @param data A data frame.
#' @param x,event,id Column names of columns containing reliability information.
#' @export
reliability_data <- function(data, x, event, id) {
  data <- dplyr::select(data, x = {{x}}, event = {{event}}, id = {{id}})

  class(data) <- c("reliability_data", class(data))

  data
}

#' Estimation of Failure Probabilities
#' @param data A standardised data frame returned by \link{reliability_data}.
#' @param method Method used for estimating the failure probablities. A detailed
#' explanation can be found with \code{?<method>_method}.
#' @param options A list of named options passed to \code{<method>_method}. For
#' now there is the option "method" with \code{method = "mr"}.
#' @export
estimate_cdf <- function(
  data, method = c("mr", "johnson", "kaplan", "nelson"), options = list()
) {
  stopifnot(inherits(data, "reliability_data"))

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
    mr_method <- if (is.null(options$method)) "benard" else options$method

    args$method <- mr_method
  }

  do.call(method_fun, args)
}

