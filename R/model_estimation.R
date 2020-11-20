#' @export
print.model_estimation <- function(x,
                                   digits = max(3L, getOption("digits") - 3L)
) {
  cat("Coefficients:\n")
  print(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  invisible(x)
}



#' @export
print.model_estimation_list <- function(x,
                                        digits = max(
                                          3L, getOption("digits") - 3L
                                        )
) {
  cat(paste("List of", length(x), "model estimations:\n"))
  purrr::walk2(x, names(x), function(model_estimation, method) {
    print(model_estimation)
    cat(paste("Method of CDF Estimation:", method, "\n"))
    cat("\n")
  })
  invisible(x)
}