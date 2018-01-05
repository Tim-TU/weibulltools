#' Mixture Model Identification using Segmented Regression
#'
#' This method uses piecewise linear regression to separate the data in
#' subgroups, if appropriate. Since this happens in an automated fashion
#' the function tends to overestimate the number of breakpoints and
#' therefore returns to many subgroups. This problem is already stated in
#' the documentation of the function \link{segmented.lm}, which is part of
#' the \emph{segmented} package.
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a
#'   product, e.g. operating time (days/months in service), mileage (km,
#'   miles), load cycles.
#' @param y a numeric vector which consists of estimated failure
#'   probabilities regarding the lifetime data in \code{x}.
#' @param event a vector of binary data (0 or 1) indicating whether
#'   unit \emph{i} is a right censored observation (= 0) or a
#'   failure (= 1).
#' @param distribution supposed distribution of the random variable.
#'   The default value is \code{"weibull"}.
#' @param conf_level confidence level of the interval. The default value is
#'   \code{conf_level = 0.95}
#'
#' @return Returns a list where the length of the list depends on
#'   the number of identified subgroups. Each list contain the same
#'   information as supplied by \link{rank_regression}.
#' @export
#'
#' @examples
#' hours = c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state = c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' john <- johnson_method(x = hours, event = state)
#'
#' mix_mod <- mixmod_regression(x = john$characteristic,
#'                              y = john$prob,
#'                              event = john$status)
#'
mixmod_regression <- function(x, y, event, distribution = c("weibull", "lognormal", "loglogistic"),
                              conf_level = .95) {
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  if (distribution == "weibull") {
    mrr <- lm(log(x_f) ~ SPREDA::qsev(y_f))
  } else if (distribution == "lognormal") {
    mrr <- lm(log(x_f) ~ stats::qnorm(y_f))
  } else if (distribution == "loglogistic") {
    mrr <- lm(log(x_f) ~ stats::qlogis(y_f))
  } else {
    stop("No valid distribution")
  }
    seg_mrr <- try(segmented::segmented.lm(mrr,
                              control = segmented::seg.control(it.max = 20,
                                        n.boot = 20)),
                   silent = TRUE)

#    if ("try-error" %in% class(seg_mrr) || length(x_f[seg_mrr$id.group != 0]) < 5) {
      mrr_0 <- rank_regression(x = x, y = y, event = event,
                                    distribution = distribution,
                                    conf_level = conf_level)
      r_sq0 <- mrr_0$r_squared
      mrr_0$x_range <- range(x)

      mrr_output <- mrr_0

      if ("try-error" %in% class(seg_mrr)) {


        message("An admissible breakpoint could not be found!
               Simple linear regression model was estimated!")

      } else if (length(x_f[seg_mrr$id.group != 0]) < 5) {


        message("Second segment contains less than 5 elements.
                Simple linear regression model was estimated!")

#      }
    } else {
      groups <- seg_mrr$id.group


      x_1 <- x_f[groups == 0]
      y_1 <- y_f[groups == 0]
      mrr_1 <- rank_regression(x = x_1, y = y_1,
                               event = rep(1, length(x_1)),
                               distribution = distribution,
                               conf_level = conf_level)

      r_sq1 <- mrr_1$r_squared
      mrr_1$x_range <- range(x_1)

      x_rest <- x_f[groups != 0]
      y_rest <- y_f[groups != 0]

      mrr_23 <- rank_regression(x = x_rest, y = y_rest,
                               event = rep(1, length(x_rest)),
                               distribution = distribution,
                               conf_level = conf_level)
      r_sq23 <- mrr_23$r_squared
      mrr_23$x_range <- range(x_rest)

      if (distribution == "weibull") {
        mrr2 <- lm(log(x_rest) ~ SPREDA::qsev(y_rest))
      } else if (distribution == "lognormal") {
        mrr2 <- lm(log(x_rest) ~ stats::qnorm(y_rest))
      } else if (distribution == "loglogistic") {
        mrr2 <- lm(log(x_rest) ~ stats::qlogis(y_rest))
      }
      seg_mrr2 <- try(segmented::segmented.lm(mrr2, control = segmented::seg.control(it.max = 20,
                                                                                     n.boot = 20)),
                      silent = TRUE)

      if ("try-error" %in% class(seg_mrr2) || length(x_rest[seg_mrr2$id.group != 0]) < 5) {

        # mrr_2 <- rank_regression(x = x_rest, y = y_rest,
        #                         event = rep(1, length(x_rest)),
        #                         distribution = distribution,
        #                         conf_level = conf_level)
        # r_sq2 <- mrr_2$r_squared

        if (mean(c(r_sq1, r_sq23)) < r_sq0) {

          print("1 statt 2 Segmente: lineare Regression")

          mrr_output <- mrr_0

        } else {

        mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_23)

        }

        # if (length(x_rest[seg_mrr2$id.group != 0]) < 5) {
        if (is.list(seg_mrr2)) {

          message("Third segment contains less than 5 elements.
                  Simple linear regression model was estimated for all elements after first breakpoint!")

        }
       } else {
        groups2 <- seg_mrr2$id.group


        x_2 <- x_rest[groups2 == 0]
        y_2 <- y_rest[groups2 == 0]

        mrr_2 <- rank_regression(x = x_2, y = y_2, event = rep(1, length(x_2)),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq2 <- mrr_2$r_squared
        mrr_2$x_range <- range(x_2)

        mrr_12 <- rank_regression(x = c(x_1, x_2), y = c(y_1, y_2), event = rep(1, length(c(x_1, x_2))),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq12 <- mrr_12$r_squared
        mrr_12$x_range <- range(c(x_1, x_2))

        x_3 <- x_rest[groups2 == 1]
        y_3 <- y_rest[groups2 == 1]

        mrr_3 <- rank_regression(x = x_3, y = y_3, event = rep(1, length(x_3)),
                                 distribution = distribution,
                                 conf_level = conf_level)
        r_sq3 <- mrr_3$r_squared
        mrr_3$x_range <- range(x_3)

        mean_r_sq <- mean(c(r_sq1, r_sq2, r_sq3))

        print(r_sq0)
        print(r_sq1)
        print(r_sq2)
        print(r_sq3)
        print(r_sq12)
        print(r_sq23)
        print(mean(c(r_sq1, r_sq23)))
        print(mean(c(r_sq12, r_sq3)))
        print(mean_r_sq)


        if (mean_r_sq < r_sq0 | mean_r_sq < mean(c(r_sq1, r_sq23)) | mean_r_sq < mean(c(r_sq12, r_sq3))) {

          print("mean_r_sq nicht am größten")

          if (mean(c(r_sq1, r_sq23)) > r_sq0 && mean(c(r_sq1, r_sq23)) > mean(c(r_sq12, r_sq3))) {

            print("2 statt 3 Segmenten: 1 und 23")

            mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_23)

          } else if (mean(c(r_sq12, r_sq3)) > r_sq0) {

            print("2 statt 3 Segmenten: 12 und 3")

            mrr_output <- list(mod_1 = mrr_12, mod_2 = mrr_3)

          } else {

            print("lineare Regression")

            mrr_output <- mrr_0
            }

        } else{

          mrr_output <- list(mod_1 = mrr_1, mod_2 = mrr_2, mod_3 = mrr_3)
          message("Problem of overestimation may have occured. Further
                  investigations are recommended!")
        }
      }
    }
  return(mrr_output)
}
