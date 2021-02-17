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



`%||%` <- function(x, y) if (!is.null(x)) x else y
