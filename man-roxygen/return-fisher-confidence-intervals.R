#' @return
#' A tibble with class `wt_confint` containing the following columns:
#'
#' * `x` : An ordered sequence of the lifetime characteristic regarding the
#'   failed units, starting at `min(x)` and ending up at `max(x)`. With
#'   `b_lives = c(0.01, 0.1, 0.5)` the 1%, 10% and 50% quantiles are additionally
#'   included in `x`, but only if the specified probabilities are in the range of
#'   the estimated probabilities.
#' * `prob` : An ordered sequence of probabilities with specified `b_lives`
#'   included.
#' * `std_err` : Estimated standard errors with respect to `direction`.
#' * `lower_bound` : Provided, if `bounds` is one of `"two_sided"` or `"lower"`.
#'   Lower confidence limits with respect to `direction`, i.e. limits for
#'   quantiles or probabilities.
#' * `upper_bound` : Provided, if `bounds` is one of `"two_sided"` or `"upper"`.
#'   Upper confidence limits with respect to `direction`, i.e. limits for
#'   quantiles or probabilities.
#' * `cdf_estimation_method` : A character that is always `NA_character`. Only
#'   needed for internal use.
#'
#' @md
