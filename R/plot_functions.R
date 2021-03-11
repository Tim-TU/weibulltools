#' Layout of the Probability Plot
#'
#' @description
#' This function is used to create the layout of a probability plot. It is
#' called inside of [plot_prob] to determine the appearance of the grid with
#' respect to the given characteristic `x`.
#'
#' @param x A numeric vector which consists of lifetime data. `x` is used to
#' specify the grid of the plot.
#' @param distribution Supposed distribution of the random variable.
#' @param title_main A character string which is assigned to the main title.
#' @param title_x A character string which is assigned to the title of the x axis.
#' @param title_y A character string which is assigned to the title of the y axis.
#' @param plot_method Package, which is used for generating the plot output.
#'
#' @return A plot object containing the layout of the probability plot.
#'
#' @md
#'
#' @keywords internal
plot_layout <- function(x,
                        distribution = c(
                          "weibull", "lognormal", "loglogistic",
                          "sev", "normal", "logistic"
                        ),
                        title_main = "Probability Plot",
                        title_x = "Characteristic",
                        title_y = "Unreliability",
                        plot_method = c("plotly", "ggplot2")
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  p_obj <- if (plot_method == "plotly") {
    plotly::plotly_empty(type = "scatter", mode = "markers", colors = "Set2")
  } else {
    ggplot2::ggplot()
  }

  plot_layout_vis(
    p_obj = p_obj,
    x = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )
}


#' Probability Plotting Method for Univariate Lifetime Distributions
#'
#' @description
#' This function is used to apply the graphical technique of probability
#' plotting. It is either applied to the output of [estimate_cdf]
#' (`plot_prob.wt_cdf_estimation`) or to the output of a mixture model from
#' [mixmod_regression] / [mixmod_em] (`plot_prob.wt_model`). Note that in the
#' latter case no distribution has to be specified because it is inferred from
#' the model.
#'
#' @details
#' If `x` was split by [mixmod_em], [estimate_cdf] with method `"johnson"` is
#' applied to subgroup-specific data. The calculated plotting positions are
#' shaped according to the determined split in [mixmod_em].
#'
#' In [mixmod_regression] a maximum of three subgroups can be determined and thus
#' being plotted. The intention of this function is to give the user a hint for
#' the existence of a mixture model. An in-depth analysis should be done afterwards.
#'
#' For `plot_method == "plotly"` the marker label for x and y are determined by
#' the first word provided in the argument `title_x` and `title_y` respectively,
#' i.e. if `title_x = "Mileage in km"` the x label of the marker is "Mileage".
#' The name of the legend entry is a combination of the `title_trace` and the
#' number of determined subgroups (if any). If `title_trace = "Group"` and the
#' data has been split in two groups, the legend entries are "Group: 1" and
#' "Group: 2".
#'
#' @param x A tibble with class `wt_cdf_estimation` returned by [estimate_cdf]
#' or a list with class `wt_model` returned by [rank_regression], [ml_estimation],
#' [mixmod_regression] or [mixmod_em].
#' @param distribution Supposed distribution of the random variable.
#' @param title_main A character string which is assigned to the main title.
#' @param title_x A character string which is assigned to the title of the x axis.
#' @param title_y A character string which is assigned to the title of the y axis.
#' @param title_trace A character string which is assigned to the legend trace.
#' @param plot_method Package, which is used for generating the plot output.
#' @template dots
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return A plot object containing the probability plot.
#'
#' @examples
#' # Reliability data:
#' data <- reliability_data(
#'   alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   data,
#'   methods = c("johnson", "kaplan")
#' )
#'
#' # Example 1 - Probability Plot Weibull:
#' plot_weibull <- plot_prob(prob_tbl)
#'
#' # Example 2 - Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   x = prob_tbl,
#'   distribution = "lognormal"
#' )
#'
#' ## Mixture identification
#' # Reliability data:
#' data_mix <- reliability_data(
#'   voltage,
#'   x = hours,
#'   status = status
#' )
#'
#' prob_mix <- estimate_cdf(
#'   data_mix,
#'   methods = c("johnson", "kaplan")
#' )
#'
#' # Example 3 - Mixture identification using mixmod_regression:
#' mix_mod_rr <- mixmod_regression(prob_mix)
#'
#' plot_mix_mod_rr <- plot_prob(x = mix_mod_rr)
#'
#' # Example 4 - Mixture identification using mixmod_em:
#' mix_mod_em <- mixmod_em(data_mix)
#'
#' plot_mix_mod_em <- plot_prob(x = mix_mod_em)
#'
#' @md
#'
#' @export
plot_prob <- function(x, ...) {
  UseMethod("plot_prob")
}



#' @rdname plot_prob
#'
#' @export
plot_prob.wt_cdf_estimation <- function(x,
                                        distribution = c(
                                          "weibull", "lognormal", "loglogistic",
                                          "sev", "normal", "logistic"
                                        ),
                                        title_main = "Probability Plot",
                                        title_x = "Characteristic",
                                        title_y = "Unreliability",
                                        title_trace = "Sample",
                                        plot_method = c("plotly", "ggplot2"),
                                        ...
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  plot_prob_(
    cdf_estimation = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @rdname plot_prob
#'
#' @export
plot_prob.wt_model <- function(x,
                               title_main = "Probability Plot",
                               title_x = "Characteristic",
                               title_y = "Unreliability",
                               title_trace = "Sample",
                               plot_method = c("plotly", "ggplot2"),
                               ...
) {
  NextMethod()
}



#' @export
plot_prob.wt_rank_regression <- function(x,
                                         title_main = "Probability Plot",
                                         title_x = "Characteristic",
                                         title_y = "Unreliability",
                                         title_trace = "Sample",
                                         plot_method = c("plotly", "ggplot2"),
                                         ...
) {

  plot_method <- match.arg(plot_method)

  cdf_estimation <- x$data

  plot_prob.wt_cdf_estimation(
    x = cdf_estimation,
    distribution = two_parametric(x$distribution),
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @export
plot_prob.wt_ml_estimation <- function(x,
                                       title_main = "Probability Plot",
                                       title_x = "Characteristic",
                                       title_y = "Unreliability",
                                       title_trace = "Sample",
                                       plot_method = c("plotly", "ggplot2"),
                                       ...
) {

  plot_method <- match.arg(plot_method)

  cdf_estimation <- estimate_cdf(x$data, methods = "johnson")

  plot_prob.wt_cdf_estimation(
    x = cdf_estimation,
    distribution = two_parametric(x$distribution),
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @export
plot_prob.wt_mixmod_regression <- function(x,
                                           title_main = "Probability Plot",
                                           title_x = "Characteristic",
                                           title_y = "Unreliability",
                                           title_trace = "Sample",
                                           plot_method = c("plotly", "ggplot2"),
                                           ...
) {

  plot_method <- match.arg(plot_method)

  # Add variable 'group' to the data tibbles in each list:
  cdf_estimation <- purrr::map2_dfr(
    x,
    seq_along(x),
    function(model_estimation, i) {
      model_estimation$data %>%
      dplyr::mutate(group = as.character(i))
    }
  )

  # Segmentation inside `mixmod_regression()` is based on one distribution:
  distribution <- x[[1]]$distribution

  plot_prob.wt_cdf_estimation(
    x = cdf_estimation,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @export
plot_prob.wt_mixmod_em <- function(x,
                                   title_main = "Probability Plot",
                                   title_x = "Characteristic",
                                   title_y = "Unreliability",
                                   title_trace = "Sample",
                                   plot_method = c("plotly", "ggplot2"),
                                   ...
) {

  plot_method <- match.arg(plot_method)

  # Remove last list element, i.e. 'em_results':
  model_estimation_list <- x[-length(x)]


  # Apply `estimate_cdf()` with `methods = "johnson"`:
  cdf_estimation <- purrr::map2_dfr(
    model_estimation_list,
    seq_along(model_estimation_list),
    function(model_estimation, index) {
      data <- reliability_data(
        model_estimation$data, x = "x", status = "status"
      )

      cdf_estimation <- estimate_cdf(data, "johnson")
      cdf_estimation$cdf_estimation_method <- as.character(index)

      cdf_estimation
    }
  )

  plot_prob.wt_cdf_estimation(
    x = cdf_estimation,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @export
plot_prob.wt_mixmod_regression_list <- function(
                                           x,
                                           title_main = "Probability Plot",
                                           title_x = "Characteristic",
                                           title_y = "Unreliability",
                                           title_trace = "Sample",
                                           plot_method = c("plotly", "ggplot2"),
                                           ...
) {

  plot_method <- match.arg(plot_method)

  # Add variable 'group' to the data tibbles in each list of each cdf estimation:
  cdf_estimation <- purrr::map_dfr(
    x,
    function(mixmod_regression) {
      purrr::map2_dfr(
        mixmod_regression,
        seq_along(mixmod_regression),
        function(model_estimation, index) {
          model_estimation$data %>%
            dplyr::mutate(group = as.character(index))
        }
      )
    }
  )

  # Segmentation inside `mixmod_regression()` is based on one distribution:
  distribution <- x[[1]][[1]]$distribution

  plot_prob.wt_cdf_estimation(
    x = cdf_estimation,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' Probability Plotting Method for Univariate Lifetime Distributions
#'
#' @inherit plot_prob return references
#'
#' @description
#' This function is used to apply the graphical technique of probability
#' plotting.
#'
#' @details
#' For `plot_method == "plotly"` the marker label for x and y are determined by
#' the first word provided in the argument `title_x` and `title_y` respectively,
#' i.e. if `title_x = "Mileage in km"` the x label of the marker is "Mileage".
#'
#' @inheritParams plot_prob
#' @param x A numeric vector which consists of lifetime data. Lifetime data
#' could be every characteristic influencing the reliability of a product, e.g.
#' operating time (days/months in service), mileage (km, miles), load cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#' regarding the lifetime data in `x`.
#' @param status A vector of binary data (0 or 1) indicating whether a unit is a
#' right censored observation (= 0) or a failure (= 1).
#' @param id Identification for every unit.
#'
#' @seealso [plot_prob]
#'
#' @examples
#' # Vectors:
#' cycles   <- alloy$cycles
#' status <- alloy$status
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' # Example 1: Probability Plot Weibull:
#' plot_weibull <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id
#' )
#'
#' # Example 2: Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id,
#'   distribution = "lognormal"
#' )
#'
#' @md
#'
#' @export
plot_prob.default <- function(x,
                              y,
                              status,
                              id = rep("XXXXXX", length(x)),
                              distribution = c(
                                "weibull", "lognormal", "loglogistic",
                                "sev", "normal", "logistic"
                              ),
                              title_main = "Probability Plot",
                              title_x = "Characteristic",
                              title_y = "Unreliability",
                              title_trace = "Sample",
                              plot_method = c("plotly", "ggplot2"),
                              ...
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  # Fake cdf_estimation:
  cdf_estimation <- tibble::tibble(
    x = x,
    prob = y,
    status = status,
    id = id,
    cdf_estimation_method = NA_character_
  )

  plot_prob_(
    cdf_estimation = cdf_estimation,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



# Function that determines the positions and performs the plot method dispatch:
plot_prob_ <- function(cdf_estimation,
                       distribution,
                       title_main,
                       title_x,
                       title_y,
                       title_trace,
                       plot_method
) {

  # Call to helper function:
  tbl_prob <- plot_prob_helper(cdf_estimation, distribution)

  # Call to `plot_layout()` to determine the distribution-specific grid:
  p_obj <- plot_layout(
    x = tbl_prob$x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    plot_method = plot_method
  )

  # Dispatch based on `plot_method`:
  p_obj <- plot_prob_vis(
    p_obj = p_obj,
    tbl_prob = tbl_prob,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace
  )

  # Store distribution for compatibility checks
  attr(p_obj, "distribution") <- distribution

  p_obj
}



#' Probability Plot for Separated Mixture Models
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `plot_prob_mix()` is no longer under active development, switching to
#' [plot_prob] is recommended.
#'
#' @details
#' This function is used to apply the graphical technique of probability
#' plotting to univariate mixture models that have been separated with functions
#' [mixmod_regression] or [mixmod_em].
#'
#' If data has been split by `mixmod_em` the function `johnson_method` is applied
#' to subgroup-specific data. The calculated plotting positions are shaped
#' regarding the obtained split of the used splitting function.
#'
#' In [mixmod_regression] a maximum of three subgroups can be determined and thus
#' being plotted. The intention of this function is to give the user a hint for
#' the existence of a mixture model. An in-depth analysis should be done
#' afterwards.
#'
#' The marker label for x and y are determined by the first word provided in the
#' argument `title_x` and `title_y` respectively, i.e. if
#' `title_x = "Mileage in km"` the x label of the marker is "Mileage".
#'
#' The name of the legend entry is a combination of the `title_trace` and the
#' number of determined subgroups (if any). If `title_trace = "Group"` and the
#' data has been split in two groups, the legend entries are "Group: 1" and
#' "Group: 2".
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @inheritParams plot_prob.default
#' @param mix_output A list returned by [mixmod_regression] or [mixmod_em], which
#' consists of values necessary to visualize the subgroups.The default value of
#' `mix_output` is `NULL`.
#'
#' @seealso [plot_prob]
#'
#' @examples
#' # Vectors:
#' hours <- voltage$hours
#' status <- voltage$status
#'
#' # Example 1 - Using result of mixmod_em:
#' mix_mod_em <- mixmod_em(
#'   x = hours,
#'   status = status
#' )
#'
#' plot_weibull_em <- plot_prob_mix(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   mix_output = mix_mod_em
#' )
#'
#' # Example 2 - Using result of mixmod_regression:
#' john <- estimate_cdf(
#'   x = hours,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' mix_mod_reg <- mixmod_regression(
#'   x = john$x,
#'   y = john$prob,
#'   status = john$status,
#'   distribution = "weibull"
#' )
#'
#' plot_weibull_reg <- plot_prob_mix(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   mix_output = mix_mod_reg
#' )
#'
#' @md
#'
#' @export
plot_prob_mix <- function(
                        x,
                        status,
                        id = rep("XXXXXX", length(x)),
                        distribution = c("weibull", "lognormal", "loglogistic"),
                        mix_output,
                        title_main = "Probability Plot",
                        title_x = "Characteristic",
                        title_y = "Unreliability",
                        title_trace = "Sample",
                        plot_method = c("plotly", "ggplot2"),
                        ...
) {

  deprecate_soft(
    "2.0.0", "plot_prob_mix()", "plot_prob()"
  )

  plot_method <- match.arg(plot_method)

  plot_prob(
    mix_output,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' Add Estimated Population Line(s) to a Probability Plot
#'
#' @description
#' This function adds one or multiple estimated regression lines to an existing
#' probability plot ([plot_prob]). Depending on the output of the functions
#' [rank_regression], [ml_estimation], [mixmod_regression] or [mixmod_em] one or
#' multiple lines are plotted.
#'
#' @details
#' The name of the legend entry is a combination of the `title_trace` and the
#' number of determined subgroups from [mixmod_regression] or [mixmod_em]. If
#' `title_trace = "Line"` and the data could be split in two groups, the legend
#' entries would be "Line: 1" and "Line: 2".
#'
#' @inheritParams plot_prob.wt_cdf_estimation
#' @param p_obj A plot object returned by [plot_prob].
#' @param x A list with class `wt_model` returned by [rank_regression],
#' [ml_estimation], [mixmod_regression] or [mixmod_em].
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#' reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return A plot object containing the probability plot with plotting positions
#' and the estimated regression line(s).
#'
#' @examples
#' # Reliability data:
#' data <- reliability_data(data = alloy, x = cycles, status = status)
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(data, methods = c("johnson", "kaplan"))
#'
#'
#' ## Rank Regression
#' # Example 1 - Probability Plot and Regression Line Three-Parameter-Weibull:
#' plot_weibull <- plot_prob(prob_tbl, distribution = "weibull")
#' rr_weibull <- rank_regression(prob_tbl, distribution = "weibull3")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull, x = rr_weibull)
#'
#' # Example 2 - Probability Plot and Regression Line Three-Parameter-Lognormal:
#' plot_lognormal <- plot_prob(prob_tbl, distribution = "lognormal")
#' rr_lognormal <- rank_regression(prob_tbl, distribution = "lognormal3")
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal, x = rr_lognormal)
#'
#'
#' ## ML Estimation
#' # Example 3 - Probability Plot and Regression Line Two-Parameter-Weibull:
#' plot_weibull <- plot_prob(prob_tbl, distribution = "weibull")
#' ml_weibull_2 <- ml_estimation(data, distribution = "weibull")
#'
#' plot_reg_weibull_2 <- plot_mod(p_obj = plot_weibull, ml_weibull_2)
#'
#'
#' ## Mixture Identification
#' # Reliability data:
#' data_mix <- reliability_data(voltage, x = hours, status = status)
#'
#' # Probability estimation:
#' prob_mix <- estimate_cdf(
#'   data_mix,
#'   methods = c("johnson", "kaplan", "nelson")
#' )
#'
#' # Example 4 - Probability Plot and Regression Line Mixmod Regression:
#' mix_mod_rr <- mixmod_regression(prob_mix, distribution = "weibull")
#' plot_weibull <- plot_prob(mix_mod_rr)
#'
#' plot_reg_mix_mod_rr <- plot_mod(p_obj = plot_weibull, x = mix_mod_rr)
#'
#' # Example 5 - Probability Plot and Regression Line Mixmod EM:
#' mix_mod_em <- mixmod_em(data_mix)
#' plot_weibull <- plot_prob(mix_mod_em)
#'
#' plot_reg_mix_mod_em <- plot_mod(p_obj = plot_weibull, x = mix_mod_em)
#'
#' @md
#'
#' @export
plot_mod <- function(p_obj, x, ...) {
  UseMethod("plot_mod", x)
}



#' @rdname plot_mod
#'
#' @export
plot_mod.wt_model <- function(p_obj,
                              x,
                              title_trace = "Fit",
                              ...
) {
  NextMethod("plot_mod", x)
}



#' @export
plot_mod.wt_model_estimation <- function(p_obj,
                                         x,
                                         title_trace = "Fit",
                                         ...
) {

  check_compatible_distributions(
    attr(p_obj, "distribution"),
    x$distribution
  )

  failed_data <- dplyr::filter(x$data, .data$status == 1)

  plot_mod.default(
    p_obj = p_obj,
    x = range(failed_data$x),
    dist_params = x$coefficients,
    distribution = x$distribution,
    title_trace = title_trace
  )
}



#' @export
plot_mod.wt_model_estimation_list <- function(p_obj,
                                              x,
                                              title_trace = "Fit",
                                              ...
) {

  cdf_estimation_methods <- names(x)

  tbl_mod <- purrr::map2_dfr(
    x,
    cdf_estimation_methods,
    function(model_estimation, cdf_estimation_method) {
      check_compatible_distributions(
        attr(p_obj, "distribution"),
        model_estimation$distribution
      )

      failed_data <- dplyr::filter(model_estimation$data, .data$status == 1)

      plot_mod_helper(
        x = range(failed_data$x),
        dist_params = model_estimation$coefficients,
        distribution = model_estimation$distribution,
        cdf_estimation_method = cdf_estimation_method
      )
    }
  )

  plot_mod_vis(
    p_obj = p_obj,
    tbl_mod = tbl_mod,
    title_trace = title_trace
  )
}



#' @export
plot_mod.wt_mixmod_regression <- function(p_obj,
                                          x,
                                          title_trace = "Fit",
                                          ...
) {

  tbl_mod <- purrr::map2_dfr(
    x,
    seq_along(x),
    function(model_estimation, index) {
      check_compatible_distributions(
        attr(p_obj, "distribution"),
        model_estimation$distribution
      )

      # Extract cdf_estimation_method from model_estimation
      cdf_estimation_method <- if (
        !tibble::has_name(model_estimation$data, "cdf_estimation_method")
        ) {
        # Case mixmod_em (plot_mod.mixmod_em calls this method internally)
        as.character(index)
      } else {
        # Case mixmod_regression
        model_estimation$data$cdf_estimation_method[1]
      }

      plot_mod_mix_helper(
        model_estimation = model_estimation,
        cdf_estimation_method = cdf_estimation_method,
        group = as.character(index)
      )
    }
  )

  plot_mod_vis(
    p_obj = p_obj,
    tbl_mod = tbl_mod,
    title_trace = title_trace
  )
}



#' @export
plot_mod.wt_mixmod_regression_list <- function(p_obj,
                                               x,
                                               title_trace = "Fit",
                                               ...
) {

  tbl_mod <- purrr::map2_dfr(
    x,
    names(x),
    function(mixmod_regression, cdf_estimation_method) {
      purrr::map2_dfr(
        mixmod_regression,
        seq_along(mixmod_regression),
        function(model_estimation, index) {
          check_compatible_distributions(
            attr(p_obj, "distribution"),
            model_estimation$distribution
          )

          plot_mod_mix_helper(
            model_estimation = model_estimation,
            cdf_estimation_method = cdf_estimation_method,
            group = as.character(index)
          )
        }
      )
    }
  )

  plot_mod_vis(
    p_obj = p_obj,
    tbl_mod = tbl_mod,
    title_trace = title_trace
  )
}



#' @export
plot_mod.wt_mixmod_em <- function(p_obj,
                                  x,
                                  title_trace = "Fit",
                                  ...
) {

  # Remove em results to get model_estimation_list
  model_estimation_list <- x[-length(x)]

  plot_mod.wt_mixmod_regression(
    p_obj = p_obj,
    x = model_estimation_list,
    title_trace = title_trace
  )
}



#' Add Estimated Population Line to a Probability Plot
#'
#' @description
#' This function adds an estimated regression line to an existing probability
#' plot ([plot_prob]).
#'
#' @inherit plot_mod references
#'
#' @inheritParams plot_mod
#' @inheritParams plot_prob.default
#' @param x A numeric vector containing the x-coordinates of the respective
#' regression line.
#' @param dist_params A (named) numeric vector of estimated location and scale
#' parameters for a specified distribution. The order of elements is important.
#' First entry needs to be the location parameter \eqn{\mu} and the second
#' element needs to be the scale parameter \eqn{\sigma}. If a three-parametric
#' model is used the third element is the threshold parameter \eqn{\gamma}.
#'
#' @return A plot object containing the probability plot with plotting positions
#' and the estimated regression line.
#'
#' @seealso [plot_mod]
#'
#' @examples
#' # Vectors:
#' cycles <- alloy$cycles
#' status <- alloy$status
#'
#' # Probability estimation
#' prob_tbl <- estimate_cdf(x = cycles, status = status, method = "johnson")
#'
#' # Example 1: Probability Plot and Regression Line Three-Parameter-Weibull:
#' plot_weibull <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id,
#'   distribution = "weibull"
#' )
#'
#' rr <- rank_regression(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   distribution = "weibull3"
#' )
#'
#' plot_reg_weibull <- plot_mod(
#'   p_obj = plot_weibull,
#'   x = prob_tbl$x,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull3"
#' )
#'
#'
#'
#' # Example 2: Probability Plot and Regression Line Three-Parameter-Lognormal:
#' plot_lognormal <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id,
#'   distribution = "lognormal"
#' )
#'
#' rr_ln <- rank_regression(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   distribution = "lognormal3"
#' )
#'
#' plot_reg_lognormal <- plot_mod(
#'   p_obj = plot_lognormal,
#'   x = prob_tbl$x,
#'   dist_params = rr_ln$coefficients,
#'   distribution = "lognormal3"
#' )
#'
#' ## Mixture Identification
#' # Vectors:
#' hours <- voltage$hours
#' status <- voltage$status
#'
#' # Probability estimation:
#' prob_mix <- estimate_cdf(
#'   x = hours,
#'   status = status,
#'   method = "johnson"
#' )
#'
#' @md
#'
#' @export
plot_mod.default <- function(p_obj,
                             x,
                             dist_params,
                             distribution = c(
                               "weibull", "lognormal", "loglogistic",
                               "sev", "normal", "logistic",
                               "weibull3", "lognormal3", "loglogistic3"
                             ),
                             title_trace = "Fit",
                             ...
) {

  distribution <- match.arg(distribution)

  check_compatible_distributions(
    attr(p_obj, "distribution"),
    distribution
  )

  tbl_mod <- plot_mod_helper(
    x,
    dist_params,
    distribution
  )

  plot_mod_vis(
    p_obj = p_obj,
    tbl_mod = tbl_mod,
    title_trace = title_trace
  )
}


#' Add Estimated Population Lines of a Separated Mixture Model to a
#' Probability Plot
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' `plot_mod_mix()` is no longer under active development, switching to
#' [plot_mod] is recommended.
#'
#' @details
#' This function adds one or multiple estimated regression lines to an existing
#' probability plot [plot_prob]). Depending on the output of the function
#' [mixmod_regression] or [mixmod_em] one or multiple lines are plotted.
#'
#' The name of the legend entry is a combination of the `title_trace` and the
#' number of determined subgroups. If `title_trace = "Line"` and the data has
#' been split in two groups, the legend entries would be `"Line: 1"` and
#' `"Line: 2"`.
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @inheritParams plot_mod.default
#' @inheritParams plot_prob.default
#' @param p_obj A plot object returned by [plot_prob_mix].
#' @param mix_output A list returned by [mixmod_regression] or [mixmod_em],
#' which consists of elements necessary to visualize the regression lines.
#'
#' @return A plot object containing the probability plot with plotting positions
#' and estimated regression line(s).
#'
#' @examples
#' # Vectors:
#' hours <- voltage$hours
#' status <- voltage$status
#'
#' # Example 1 - Using result of mixmod_em in mix_output:
#' mix_mod_em <- mixmod_em(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   conf_level = 0.95,
#'   k = 2,
#'   method = "EM",
#'   n_iter = 150
#' )
#'
#' plot_weibull_em <- plot_prob_mix(
#'   x = hours,
#'   status = status,
#'   id = id,
#'   distribution = "weibull",
#'   mix_output = mix_mod_em
#' )
#'
#' plot_weibull_emlines <- plot_mod_mix(
#'   p_obj = plot_weibull_em,
#'   x = hours,
#'   status = status,
#'   mix_output = mix_mod_em,
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Using result of mixmod_regression in mix_output:
#' john <- johnson_method(x = hours, status = status)
#' mix_mod_reg <- mixmod_regression(
#'   x = john$x,
#'   y = john$prob,
#'   status = john$status,
#'   distribution = "weibull"
#' )
#'
#' plot_weibull_reg <- plot_prob_mix(
#'   x = john$x,
#'   status = john$status,
#'   id = john$id,
#'   distribution = "weibull",
#'   mix_output = mix_mod_reg,
#' )
#'
#' plot_weibull_reglines <- plot_mod_mix(
#'   p_obj = plot_weibull_reg,
#'   x = john$x,
#'   status = john$status,
#'   mix_output = mix_mod_reg,
#'   distribution = "weibull"
#' )
#'
#' @md
#'
#' @export
plot_mod_mix <- function(p_obj,
                         x,
                         status,
                         mix_output,
                         distribution = c(
                           "weibull", "lognormal", "loglogistic"
                         ),
                         title_trace = "Fit",
                         ...
) {

  deprecate_soft(
    "2.0.0", "plot_mod_mix()", "plot_mod()"
  )

  plot_mod(
    p_obj = p_obj,
    x = mix_output,
    title_trace = title_trace
  )
}


#' Add Confidence Region(s) for Quantiles and Probabilities
#'
#' This function is used to add estimated confidence region(s) to an existing
#' probability plot. Since confidence regions are related to the estimated
#' regression line, the latter is provided as well.
#'
#' @param p_obj A plot object returned by [plot_prob].
#' @param x A tibble with class `wt_confint` returned by [confint_betabinom] or
#' [confint_fisher].
#' @param title_trace_mod A character string which is assigned to the model trace
#' in the legend.
#' @param title_trace_conf A character string which is assigned to the confidence
#' trace in the legend.
#' @template dots
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#' reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return A plot object containing the probability plot with plotting positions,
#' the estimated regression line and the estimated confidence region(s).
#'
#' @examples
#' # Reliability data:
#' data <- reliability_data(data = alloy, x = cycles, status = status)
#'
#' # Probability estimation:
#' prob_tbl <- estimate_cdf(data, methods = "johnson")
#'
#' # Example 1 - Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' rr <- rank_regression(prob_tbl, distribution = "weibull3")
#'
#' conf_betabin <- confint_betabinom(rr)
#'
#' plot_weibull <- plot_prob(prob_tbl, distribution = "weibull")
#'
#' plot_conf_beta <- plot_conf(
#'   p_obj = plot_weibull,
#'   x = conf_betabin
#' )
#'
#' # Example 2 - Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' rr_ln <- rank_regression(
#'   prob_tbl,
#'   distribution = "lognormal3",
#'   conf_level = 0.9
#' )
#'
#' conf_betabin_ln <- confint_betabinom(
#'   rr_ln,
#'   bounds = "two_sided",
#'   conf_level = 0.9,
#'   direction = "y"
#' )
#'
#' plot_lognormal <- plot_prob(prob_tbl, distribution = "lognormal")
#'
#' plot_conf_beta_ln <- plot_conf(
#'   p_obj = plot_lognormal,
#'   x = conf_betabin_ln
#' )
#'
#' # Example 3 - Probability Plot, Regression Line and Confidence Bounds for MLE
#' ml <- ml_estimation(data, distribution = "weibull")
#'
#' conf_fisher <- confint_fisher(ml)
#'
#' plot_weibull <- plot_prob(prob_tbl, distribution = "weibull")
#'
#' plot_conf_fisher_weibull <- plot_conf(
#'   p_obj = plot_weibull,
#'   x = conf_fisher
#' )
#'
#' @md
#'
#' @export
plot_conf <- function(p_obj, x, ...) {
  UseMethod("plot_conf", x)
}



#' @rdname plot_conf
#'
#' @export
plot_conf.wt_confint <- function(p_obj,
                                 x,
                                 title_trace_mod = "Fit",
                                 title_trace_conf = "Confidence Limit",
                                 ...
) {

  # Extract models, could be either of class 'wt_model_estimation' or '*_list':
  mod <- attr(x, "model_estimation")

  # If-clause captures `ml_estimation()` or `rank_regression()` models, where
  # the latter was performed with only one 'cdf_estimation_method':
  if (inherits(mod, "wt_model_estimation")) {

    ## Fake a 'model_estimation_list':
    if (hasName(mod$data, "cdf_estimation_method")) {
      ### `rank_regression()` with only one 'cdf_estimation_method':
      cdf_estimation_method <- mod$data$cdf_estimation_method[1]
    } else {
      ### `ml_estimation()` where 'data' has no 'cdf_estimation_method' column:
      cdf_estimation_method <- NA_character_
    }

    mod <- list(mod)
    names(mod) <- cdf_estimation_method
  }

  # Perform customized `plot_mod()` on 'model_estimation_list':
  cdf_estimation_methods <- names(mod)
  tbl_mod <- purrr::map2_dfr(
    mod,
    cdf_estimation_methods,
    function(model_estimation, cdf_estimation_method) {
      check_compatible_distributions(
        attr(p_obj, "distribution"),
        model_estimation$distribution
      )

      plot_mod_helper(
        x = x$x, # x coordinates of confint guarantees consideration of b lives.
        dist_params = model_estimation$coefficients,
        distribution = model_estimation$distribution,
        cdf_estimation_method = cdf_estimation_method
      )
    }
  )

  tbl_conf <- plot_conf_helper_2(confint = x)

  # if (inherits(p_obj, "plotly")) {
  #   tbl_mod$hovertext <- hovertext_conf(
  #     tbl_mod = tbl_mod,
  #     tbl_conf = tbl_conf
  #   )
  # }

  bounds <- attr(x, "bounds", exact = TRUE)

  # Same x coordinates for `plot_mod()` and `plot_conf()`:
  if (bounds == "two_sided") {
    tbl_mod$lower <- x$lower_bound
    tbl_mod$upper <- x$upper_bound
  } else if (bounds == "lower") {
    tbl_mod$lower <- x$lower_bound
  } else {
    tbl_mod$upper <- x$upper_bound
  }

  p_mod <- plot_mod_vis(
    p_obj = p_obj,
    tbl_mod = tbl_mod,
    title_trace = title_trace_mod
  )

  plot_conf_vis(
    p_mod, tbl_conf, title_trace_conf
  )
}



#' Add Confidence Region(s) for Quantiles and Probabilities
#'
#' @inherit plot_conf return references
#'
#' @description
#' This function is used to add estimated confidence region(s) to an existing
#' probability plot which also includes the estimated regression line.
#'
#' @details
#' It is important that the length of the vectors provided as lists in `x` and
#' `y` match with the length of the vectors `x` and `y` in the function [plot_mod].
#' For this reason the following procedure is recommended:
#'
#' * Calculate confidence intervals with the function [confint_betabinom] or
#'   [confint_fisher] and store it in a `data.frame`. For instance call it df.
#' * Inside [plot_mod] use the output `df$x` for `x` and `df$prob` for `y` of
#'   the function(s) named before.
#' * In __Examples__ the described approach is shown with code.
#'
#' @inheritParams plot_prob.default
#' @param p_obj A plot object returned by [plot_mod].
#' @param x A list containing the x-coordinates of the confidence region(s). The
#' list can be of length 1 or 2. For more information see **Details**.
#' @param y A list containing the y-coordinates of the Confidence Region(s).
#'   The list can be of length 1 or 2. For more information see **Details**.
#' @param direction A character string specifying the direction of the plotted
#'   interval(s). `"y"` for failure probabilities or `"x"` for quantiles.
#'
#' @seealso [plot_conf]
#'
#' @examples
#' # Vectors:
#' cycles   <- alloy$cycles
#' status <- alloy$status
#'
#' prob_tbl <- estimate_cdf(x = cycles, status = status, method = "johnson")
#'
#' # Example 1 - Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' rr <- rank_regression(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   distribution = "weibull3"
#' )
#'
#' conf_betabin <- confint_betabinom(
#'   x = prob_tbl$x,
#'   status = prob_tbl$status,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull3"
#' )
#'
#' plot_weibull <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id,
#'   distribution = "weibull"
#' )
#'
#' plot_reg_weibull <- plot_mod(
#'   p_obj = plot_weibull,
#'   x = conf_betabin$x,
#'   y = conf_betabin$prob,
#'   dist_params = rr$coefficients,
#'   distribution = "weibull3"
#' )
#'
#' plot_conf_beta <- plot_conf(
#'   p_obj = plot_reg_weibull,
#'   x = list(conf_betabin$x),
#'   y = list(conf_betabin$lower_bound, conf_betabin$upper_bound),
#'   direction = "y",
#'   distribution = "weibull3"
#' )
#'
#' # Example 2 - Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' rr_ln <- rank_regression(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   distribution = "lognormal3"
#' )
#'
#' conf_betabin_ln <- confint_betabinom(
#'   x = prob_tbl$x,
#'   status = prob_tbl$status,
#'   dist_params = rr_ln$coefficients,
#'   distribution = "lognormal3"
#' )
#'
#' plot_lognormal <- plot_prob(
#'   x = prob_tbl$x,
#'   y = prob_tbl$prob,
#'   status = prob_tbl$status,
#'   id = prob_tbl$id,
#'   distribution = "lognormal"
#' )
#'
#' plot_reg_lognormal <- plot_mod(
#'   p_obj = plot_lognormal,
#'   x = conf_betabin_ln$x,
#'   y = conf_betabin_ln$prob,
#'   dist_params = rr_ln$coefficients,
#'   distribution = "lognormal3"
#' )
#'
#' plot_conf_beta_ln <- plot_conf(
#'   p_obj = plot_reg_lognormal,
#'   x = list(conf_betabin_ln$x),
#'   y = list(conf_betabin_ln$lower_bound, conf_betabin_ln$upper_bound),
#'   direction = "y",
#'   distribution = "lognormal3"
#' )
#'
#' @md
#'
#' @export
plot_conf.default <- function(p_obj,
                              x,
                              y,
                              distribution = c(
                                "weibull", "lognormal", "loglogistic",
                                "sev", "normal", "logistic",
                                "weibull3", "lognormal3", "loglogistic3"
                              ),
                              direction = c("y", "x"),
                              title_trace = "Confidence Limit",
                              ...
) {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  check_compatible_distributions(
    attr(p_obj, "distribution"),
    distribution
  )

  # Extracting tbl_mod:
  plot_method <- if (inherits(p_obj, "plotly")) {
    "plotly"
  } else if (inherits(p_obj, "ggplot")) {
    "ggplot2"
  } else {
    stop(
      "'p_obj' must be either a {plotly} or {ggplot2} object!",
      call. = FALSE
    )
  }

  tbl_mod <- if (plot_method == "plotly") {
    plotly::plotly_data(p_obj)
  } else {
    p_obj$layers[[2]]$data
  }

  tbl_conf <- plot_conf_helper(
    tbl_mod, x, y, direction, distribution
  )

  plot_conf_vis(
    p_obj, tbl_conf, title_trace
  )
}



#' Add Population Line(s) to an Existing Grid
#'
#' @description
#' This function adds one or multiple linearized CDFs to an existing plot grid.
#'
#' @details
#' `dist_params_tbl` is a data.frame with two or three columns. For
#' location-scale distributions, the first column contains the location parameter
#' and the second column contains the scale parameter. For three-parametric
#' distributions the third column contains the threshold parameter.
#'
#' If only one population line should be displayed, a numeric vector of length
#' 2 or 3 is also supported (`c(loc, sc)` or `c(loc, sc, thres)`).
#'
#' @inheritParams plot_prob
#' @param p_obj A plot object to which the population line(s) is (are) added or
#' `NULL`. If `NULL` the population line(s) is (are) plotted in an empty grid.
#' @param x A numeric vector of length two or greater used for the x coordinates
#' of the population line. If `length(x) == 2` a sequence of length 200 between
#' `x[1]` and `x[2]` is created. This sequence is equidistant with respect to the
#' scale of the x axis. If `length(x) > 2` the elements of `x` are the x
#' coordinates of the population line.
#' @param dist_params_tbl A tibble. See 'Details'.
#' @param distribution Supposed distribution of the random variable. In the
#' context of this function `"weibull"`, `"lognormal"` and `"loglogistic"` can
#' be the two- **and** three-parametric versions of the respective distribution.
#' The distinction is made with `dist_params_tbl`.
#' @param tol The failure probability is restricted to the interval
#' \eqn{[tol, 1 - tol]}. The default value is in accordance with the decimal
#' places shown in the hover for `plot_method = "plotly"`.
#' @param plot_method Package, which is used for generating the plot output. Only
#' used when `p_obj = NULL`. If `p_obj != NULL` the plot object is used to
#' determine the plot method.
#'
#' @return A plot object containing the linearized CDF(s).
#'
#' @examples
#' x <- rweibull(n = 100, shape = 1, scale = 20000)
#'
#' # Example 1 - Two-parametric straight line:
#' pop_weibull <- plot_pop(
#'   p_obj = NULL,
#'   x = range(x),
#'   dist_params_tbl = c(log(20000), 1),
#'   distribution = "weibull"
#' )
#'
#' # Example 2 - Three-parametric curved line:
#' x2 <- rweibull(n = 100, shape = 1, scale = 20000) + 5000
#'
#' pop_weibull_2 <- plot_pop(
#'   p_obj = NULL,
#'   x = x2,
#'   dist_params_tbl = c(log(20000 - 5000), 1, 5000),
#'   distribution = "weibull"
#' )
#'
#' # Example 3 - Multiple lines:
#' pop_weibull_3 <- plot_pop(
#'   p_obj = NULL,
#'   x = x,
#'   dist_params_tbl = data.frame(
#'     p_1 = c(log(20000), log(20000), log(20000)),
#'     p_2 = c(1, 1.5, 2)
#'     ),
#'   distribution = "weibull",
#'   plot_method = "ggplot2"
#' )
#'
#' # Example 4 - Compare two- and three-parametric distributions:
#' pop_weibull_4 <- plot_pop(
#'   p_obj = NULL,
#'   x = x,
#'   dist_params_tbl = data.frame(
#'     param_1 = c(log(20000), log(20000)),
#'     param_2 = c(1, 1),
#'     param_3 = c(NA, 2)
#'   ),
#'   distribution = "weibull"
#' )
#'
#' @md
#'
#' @export
plot_pop <- function(p_obj = NULL,
                     x,
                     dist_params_tbl,
                     distribution = c(
                       "weibull", "lognormal", "loglogistic",
                       "sev", "normal", "logistic"
                     ),
                     tol = 1e-6,
                     title_trace = "Population",
                     plot_method = c("plotly", "ggplot2")
) {

  distribution <- match.arg(distribution)

  if (purrr::is_null(p_obj)) {
    plot_method <- match.arg(plot_method)

    p_obj <- plot_layout(
      x = x,
      distribution = distribution,
      plot_method = plot_method
    )
  }

  # Support vector instead of tibble for ease of use in dist_params_tbl:
  if (!inherits(dist_params_tbl, "data.frame")) {
    dist_params_tbl <- tibble::tibble(
      loc = dist_params_tbl[1],
      sc = dist_params_tbl[2],
      # thres is `NA` if `dist_params` is of length 2:
      thres = dist_params_tbl[3]
    )
  }

  # Add thres column if not present:
  if (ncol(dist_params_tbl) == 2) {
    dist_params_tbl$thres <- NA_real_
  }

  # Ensure correct naming:
  names(dist_params_tbl) <- c("loc", "sc", "thres")

  tbl_pop <- plot_pop_helper(x, dist_params_tbl, distribution, tol)

  plot_pop_vis(
    p_obj, tbl_pop, title_trace
  )
}
