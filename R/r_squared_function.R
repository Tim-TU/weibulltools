#' R-Squared-Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @description
#' This function evaluates the coefficient of determination with respect to a
#' given threshold parameter of a parametric lifetime distribution. In terms of
#' *Rank Regression* this function can be optimized ([optim][stats::optim]) to
#' estimate the threshold parameter.
#'
#' @inheritParams rank_regression
#' @param thres A numeric value for the threshold parameter.
#' @param distribution Supposed parametric distribution of the random variable.
#'
#' @return
#' Returns the coefficient of determination with respect to the threshold
#' parameter `thres`.
#'
#' @encoding UTF-8
#'
#' @examples
#' # Data:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   data,
#'   methods = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(
#'     dplyr::pull(
#'       dplyr::filter(
#'         prob_tbl,
#'         status == 1,
#'         x == min(x)
#'       ),
#'       x
#'     ) - 0.1
#'   ),
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = dplyr::filter(
#'     prob_tbl,
#'     status == 1
#'   ),
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
r_squared_profiling <- function(x, ...) {
  UseMethod("r_squared_profiling")
}



#' @rdname r_squared_profiling
#'
#' @export
r_squared_profiling.wt_cdf_estimation <- function(x,
                                                  thres,
                                                  distribution = c(
                                                    "weibull3",
                                                    "lognormal3",
                                                    "loglogistic3",
                                                    "exponential2"
                                                  ),
                                                  direction = c(
                                                    "x_on_y", "y_on_x"
                                                  ),
                                                  ...
) {

  r_squared_profiling.default(
    x = x$x,
    y = x$prob,
    thres = thres,
    distribution = distribution,
    direction = direction
  )
}



#' R-Squared-Profile Function for Parametric Lifetime Distributions with Threshold
#'
#' @inherit r_squared_profiling description details return
#'
#' @inheritParams r_squared_profiling
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#'   could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in x.
#'
#' @seealso [r_squared_profiling]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' # Determining the optimal coefficient of determination:
#' ## Range of threshold parameter must be smaller than the first failure:
#' threshold <- seq(
#'   0,
#'   min(cycles[status == 1]) - 0.1,
#'   length.out = 100
#' )
#'
#' ## Coefficient of determination with respect to threshold values:
#' profile_r2 <- r_squared_profiling(
#'   x = prob_tbl$x[prob_tbl$status == 1],
#'   y = prob_tbl$prob[prob_tbl$status == 1],
#'   thres = threshold,
#'   distribution = "weibull3"
#' )
#'
#' ## Threshold value (among the candidates) that maximizes the
#' ## coefficient of determination:
#' threshold[which.max(profile_r2)]
#'
#' ## plot:
#' plot(
#'   threshold,
#'   profile_r2,
#'   type = "l"
#' )
#' abline(
#'   v = threshold[which.max(profile_r2)],
#'   h = max(profile_r2),
#'   col = "red"
#' )
#'
#' @md
#'
#' @export
r_squared_profiling.default <- function(x,
                                        y,
                                        thres,
                                        distribution = c(
                                          "weibull3",
                                          "lognormal3",
                                          "loglogistic3",
                                          "exponential2"
                                        ),
                                        direction = c("x_on_y", "y_on_x"),
                                        ...
) {

  if (any(is.na(y))) {
    stop(
      "At least one of the failure probabilities ('y') is NA!",
      .call = FALSE
    )
  }

  distribution <- match.arg(distribution)
  direction <- match.arg(direction)

  r_sq_prof_vectorized <- Vectorize(
    FUN = r_squared_profiling_,
    vectorize.args = "thres"
  )

  r_sq_prof_vectorized(
    x = x,
    y = y,
    thres = thres,
    distribution = distribution,
    direction = direction
  )
}



r_squared_profiling_ <- function(x,
                                 y,
                                 thres,
                                 distribution,
                                 direction

) {

  # Subtracting value of threshold, i.e. influence of threshold is eliminated:
  x_thres <- x - thres

  # Rank Regression:
  rr <- lm_(
    x = x_thres,
    y = y,
    distribution = distribution,
    direction = direction
  )

  summary(rr)$r.squared
}
