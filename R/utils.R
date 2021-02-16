two_parametric <- function(distribution) {
  sub("3", "", distribution)
}



check_compatible_distributions <- function(p_obj_dist, model_dist) {
  if (p_obj_dist != two_parametric(model_dist)) {
    msg <- paste0(
      "Incompatible distributions! Probability plot has distribution '",
      p_obj_dist,
      "' whereas model has distribution '",
      model_dist,
      "'."
    )

    stop(
      errorCondition(
        message = msg,
        class = "incompatible_distributions"
      )
    )
  }
}



check_dots <- function(...) {
  dots <- list(...)

  if (length(dots) > 0) {
    stop(
      "'...' is not used. Arguments must be matched by name!",
      call. = FALSE
    )
  }
}


check_dates <- function(x,
                        date.format = "%Y-%m-%d"
){
  tryCatch(
    !is.na(as.Date(x, date.format)),
    error = function(e) {FALSE}
  )
}



# Distributional helper functions throughout the package:
## qsev
qsev <- function(p) {
  p <- ifelse(p >= 0.9999999999999999, 0.9999999999999999, p)
  p <- ifelse(p <= 1 - 0.9999999999999999, 1 - 0.9999999999999999, p)

  log(-log(1 - p))
}



## psev
psev <- function(q) {
  1 - exp(-exp(q))
}



## dsev
dsev <- function(x) {
  exp(x - exp(x))
}
