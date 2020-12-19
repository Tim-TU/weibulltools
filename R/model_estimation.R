#' @export
print.wt_model <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  NextMethod("print")
}

#' @export
print.wt_model_estimation <- function(x,
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
print.wt_model_estimation_list <- function(x,
                                        digits = max(
                                          3L,
                                          getOption("digits") - 3L
                                        ),
                                        ...
) {
  cat(paste("List of", length(x), "model estimations:\n"))
  purrr::walk2(x, names(x), function(model_estimation, method) {
    print(model_estimation)
    cat(paste("Method of CDF Estimation:", method, "\n"))
    cat("\n")
  })
  invisible(x)
}

#' @export
vcov.wt_model_estimation <- function(object, ...) {
  if (hasName(object, "varcov")) {
    object$varcov
  } else {
    stop("Variance-covariance matrix of location-scale parameters does not exist!")
  }

}
