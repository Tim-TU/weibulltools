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



`%||%` <- function(x, y) if (!is.null(x)) x else y

`%NA%` <- function(x, y) if (!is.na(x)) x else y
