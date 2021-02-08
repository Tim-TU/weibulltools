two_parametric <- function(distribution) {
  sub("3", "", distribution)
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
