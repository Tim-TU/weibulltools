two_parametric <- function(distribution) {
  sub("3", "", distribution)
}



check_dots <- function(...) {
  dots <- list(...)

  if (length(dots) > 1) {
    stop(
      "'...' is not used. Arguments must be matched by name!",
      call. = FALSE
    )
  }
}

check_dates <- function(mydate,
                        date.format = "%Y-%m-%d"
){
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}
