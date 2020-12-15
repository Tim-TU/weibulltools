#' Layout of the Probability Plot
#'
#' This function is called to create the layout of a probability plot. It is
#' called inside of \code{\link{plot_prob}} to determine the appearance of the
#' grid with respect to the given characteristic \code{x}.
#'
#' @param x A numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the grid of the plot.
#' @param distribution Supposed distribution of the random variable.
#' @param title_main A character string which is assigned to the main title
#'   of the plot.
#' @param title_x A character string which is assigned to the title of the
#'   x axis.
#' @param title_y A character string which is assigned to the title of the
#'   y axis.
#' @param plot_method Package, which is used for generating the plot output.
#'
#' @return Returns a plot object which contains the layout
#'   that is used for probability plotting.
#'
#' @examples
#' # Example 1: Weibull-Grid:
#' x_layout <- seq(1e-5, 1e+07, length.out = 10)
#' grid_weibull <- plot_layout(x = x_layout,
#'                             distribution = "weibull",
#'                             title_main = "Weibull Analysis",
#'                             title_x = "Time to Failure",
#'                             title_y = "Failure Probability in %")
#'
#' # Example 2: Grid of Normal Distribution:
#' x_layout <- seq(1, 10, length.out = 10)
#' grid_normal <- plot_layout(x = x_layout,
#'                             distribution = "normal",
#'                             title_main = "Normal Grid",
#'                             title_x = "Time to Event",
#'                             title_y = "Failure Probability in %")
plot_layout <- function(
  x,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  plot_method = c("plotly", "ggplot2")
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  plot_layout_fun <- if (plot_method == "plotly") plot_layout_plotly else
    plot_layout_ggplot2

  plot_layout_fun(
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
#' plotting.
#'
#' @details
#'
#' If data was splitted by \code{\link{mixmod_em}} \code{\link{estimate_cdf}} with
#' method \code{"johnson"} is applied to subgroup-specific data. The
#' calculated plotting positions are shaped according to the determined split in
#' \code{\link{mixmod_em}}.
#'
#' In \code{\link{mixmod_regression}} a maximum of three subgroups can be determined
#' and thus being plotted. The intention of this function is to give the
#' user a hint for the existence of a mixture model. An in-depth analysis should
#' be done afterwards.
#'
#' The marker label for x and y are determined by the first word provided in the
#' argument \code{title_x} and \code{title_y} respectively, i.e. if
#' \code{title_x = "Mileage in km"} the x label of the marker is "Mileage".
#'
#' The name of the legend entry is a combination of the \code{title_trace} and
#' the number of determined subgroups (if any). If \code{title_trace = "Group"}
#' and the data is split in two groups, the legend entries are "Group: 1"
#' and "Group: 2".
#'
#' @section S3 methods:
#'
#' | **Class** | **Returned from** | **Condition** |
#' | --- | --- | --- |
#' | \code{cdf_estimation} | \code{\link{estimate_cdf}} | - |
#' | \code{model_estimation} | \code{\link{mixmod_regression}} | One method in \code{\link{estimate_cdf}}. No subgroups determined. |
#' | \code{mixmod_regression} | \code{\link{mixmod_regression}} | One method in \code{\link{estimate_cdf}}. |
#' | \code{mixmod_regression_list} | \code{mixmod_regression} | Multiple methods in \code{\link{estimate_cdf}}.
#' | \code{mixmod_em} | \code{\link{mixmod_em}} | - |
#'
#' @usage
#' ## S3 method for classes 'cdf_estimation', 'model_estimation',
#' ## 'mixmod_regression', 'mixmod_em', 'mixmod_regression_list'
#' plot_prob(
#'   x,
#'   title_main = "Probability Plot",
#'   title_x = "Characteristic",
#'   title_y = "Unreliability",
#'   title_trace = "Sample",
#'   plot_method = c("plotly", "ggplot2"),
#'   ...
#' )
#'
#' @param x An object of aforementioned classes. See 'Details'.
#' @param distribution Supposed distribution of the random variable.
#' @param title_main A character string which is assigned to the main title
#'   of the plot.
#' @param title_x A character string which is assigned to the title of the
#'   x axis.
#' @param title_y A character string which is assigned to the title of the
#'   y axis.
#' @param title_trace A character string whis is assigned to the trace shown in
#'   the legend.
#' @param plot_method Package, which is used for generating the plot output.
#' @template dots
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a plot object containing the probability plot.
#'
#' @examples
#' # Reliability data:
#' data <- reliability_data(
#'   data = alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' prob_tbl <- estimate_cdf(data, methods = c("johnson", "kaplan"))
#'
#' # Example 1 - Probability Plot Weibull:
#' plot_weibull <- plot_prob(
#'   prob_tbl,
#'   distribution = "weibull",
#'   title_main = "Weibull Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' # Example 2 - Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   prob_tbl,
#'   distribution = "lognormal",
#'   title_main = "Lognormal Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' ## Mixture identification
#' # Reliability data:
#' data_mix <- reliability_data(
#'   data = voltage,
#'   x = hours,
#'   status = status
#' )
#'
#' prob_mix <- estimate_cdf(data_mix, methods = c("johnson", "kaplan"))
#'
#' # Example 3 - Mixture identification using mixmod_regression:
#' mix_mod_rr <- mixmod_regression(
#'   x = prob_mix,
#'   distribution = "weibull",
#'   conf_level = 0.99
#' )
#'
#' plot_mix_rr <- plot_prob(
#'   mix_mod_rr,
#'   title_main = "Weibull Mixture Regression",
#'   title_x = "Time in Hours",
#'   title_y = "Probability of Failure",
#'   title_trace = "Subgroup"
#' )
#'
#' # Example 4 - Mixture identification using mixmod_em:
#' mix_mod_em <- mixmod_em(
#'   data_mix,
#'   distribution = "weibull",
#'   conf_level = 0.99
#' )
#'
#' plot_mix_em <- plot_prob(
#'   mix_mod_em,
#'   title_main = "Weibull Mixture EM",
#'   title_x = "Time in Hours",
#'   title_y = "Probability of Failure",
#'   title_trace = "Subgroup"
#' )
#'
#' @md
#' @export
#'
plot_prob <- function(x, ...) {
  UseMethod("plot_prob")
}



#' @rdname plot_prob
#'
#' @usage NULL
#'
#' @export
plot_prob.cdf_estimation <- function(
  x,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
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
#' @usage NULL
#'
#' @export
plot_prob.model_estimation <- function(x,
                                       title_main = "Probability Plot",
                                       title_x = "Characteristic",
                                       title_y = "Unreliability",
                                       title_trace = "Sample",
                                       plot_method = c("plotly", "ggplot2"),
                                       ...
) {
  plot_method <- match.arg(plot_method)

  cdf_estimation <- x$data

  plot_prob.cdf_estimation(
    x = cdf_estimation,
    distribution = x$distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @rdname plot_prob
#'
#' @usage NULL
#'
#' @export
plot_prob.mixmod_regression <- function(x,
                                        title_main = "Probability Plot",
                                        title_x = "Characteristic",
                                        title_y = "Unreliability",
                                        title_trace = "Sample",
                                        plot_method = c("plotly", "ggplot2"),
                                        ...
) {

  plot_method <- match.arg(plot_method)

  # Take all data
  cdf_estimation <- purrr::map2_dfr(x, seq_along(x), function(model_estimation, i) {
    model_estimation$data %>%
      # Mark group
      dplyr::mutate(group = as.character(i))
  })

  distribution <- x[[1]]$distribution
  plot_prob.cdf_estimation(
    x = cdf_estimation,
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
#' @usage NULL
#'
#' @export
plot_prob.mixmod_em <- function(x,
                                title_main = "Probability Plot",
                                title_x = "Characteristic",
                                title_y = "Unreliability",
                                title_trace = "Sample",
                                plot_method = c("plotly", "ggplot2"),
                                ...
) {

  plot_method <- match.arg(plot_method)

  model_estimation_list <- x[-length(x)]

  john <- purrr::map2_dfr(
    model_estimation_list, seq_along(model_estimation_list),
    function(model_estimation, index) {
      data <- reliability_data(
        model_estimation$data, x = x, status = status
      )

      john <- estimate_cdf(data, "johnson")

      john$method <- as.character(index)

      john
    }
  )

  plot_prob.cdf_estimation(
    x = john,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace,
    plot_method = plot_method
  )
}



#' @rdname plot_prob
#'
#' @usage NULL
#'
#' @export
plot_prob.mixmod_regression_list <- function(x,
                                             title_main = "Probability Plot",
                                             title_x = "Characteristic",
                                             title_y = "Unreliability",
                                             title_trace = "Sample",
                                             plot_method = c("plotly", "ggplot2"),
                                             ...
) {

  plot_method <- match.arg(plot_method)

  # Take all data
  cdf_estimation <- purrr::map_dfr(x, function(mixmod_regression) {
    purrr::map2_dfr(
      mixmod_regression,
      seq_along(mixmod_regression),
      function(model_estimation, index) {
        model_estimation$data %>% dplyr::mutate(group = as.character(index))
      }
    )
  })

  distribution <- x[[1]][[1]]$distribution
  plot_prob.cdf_estimation(
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
#' @inherit plot_prob description details return references
#'
#' @param x A numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y A numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param status A vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id A character vector for the identification of every unit.
#' @param distribution Supposed distribution of the random variable.
#' @param title_main A character string which is assigned to the main title
#'   of the plot.
#' @param title_x A character string which is assigned to the title of the
#'   x axis.
#' @param title_y A character string which is assigned to the title of the
#'   y axis.
#' @param title_trace A character string whis is assigned to the trace shown in
#'   the legend.
#' @param plot_method Package, which is used for generating the plot output.
#'
#' @return Returns a plot object containing the probability plot.
#'
#' @seealso \code{\link{plot_prob}}
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#'
#' john <- estimate_cdf(
#'   x = cycles,
#'   status = status,
#'   methods = "johnson"
#' )
#'
#' # Example 1: Probability Plot Weibull:
#' plot_weibull <- plot_prob(
#'   x = john$x,
#'   y = john$prob,
#'   status = john$status,
#'   id = john$id,
#'   distribution = "weibull",
#'   title_main = "Weibull Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' # Example 2: Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   x = john$x,
#'   y = john$prob,
#'   status = john$status,
#'   id = john$id,
#'   distribution = "lognormal",
#'   title_main = "Lognormal Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' @export
plot_prob.default <- function(
  x, y, status,
  id = rep("XXXXXX", length(x)),
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic", title_y = "Unreliability",
  title_trace = "Sample",
  plot_method = c("plotly", "ggplot2"),
  ...
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  cdf_estimation <- tibble::tibble(
    x = x, prob = y, status = status, id = id,
    method = title_trace
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



plot_prob_ <- function(
  cdf_estimation, distribution, title_main, title_x, title_y, title_trace,
  plot_method
) {

  tbl_prob <- plot_prob_helper(
    cdf_estimation, distribution
  )

  p_obj <- plot_layout(
    x = tbl_prob$x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    plot_method = plot_method
  )

  plot_prob_fun <- if (plot_method == "plotly") plot_prob_plotly else
    plot_prob_ggplot2

  plot_prob_fun(
    p_obj = p_obj,
    tbl_prob = tbl_prob,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace
  )
}



#' Probability Plot for Separated Mixture Models
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' This function is used to apply the graphical technique of probability
#' plotting to univariate mixture models that were separated with functions
#' \code{\link{mixmod_regression}} or \code{\link{mixmod_em}}.
#'
#' @details
#' If data was splitted by \code{mixmod_em} the function \code{johnson_method}
#' is applied to subgroup-specific data. The calculated plotting positions are
#' shaped regarding the obtained split of the used splitting function.
#'
#' In \code{\link{mixmod_regression}} a maximum of three subgroups can be determined
#' and thus being plotted. The intention of this function is to give the
#' user a hint for the existence of a mixture model. An in-depth analysis should
#' be done afterwards.
#'
#' The marker label for x and y are determined by the first word provided in the
#' argument \code{title_x} respective \code{title_y}, i.e. if
#' \code{title_x = "Mileage in km"} the x label of the marker is "Mileage".
#'
#' The name of the legend entry is a combination of the \code{title_trace} and the
#' number of determined subgroups. If \code{title_trace = "Group"} and the data
#' could be splitted in two groups, the legend entries would be "Group 1" and "Group 2".
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @inheritParams plot_prob.default
#' @param mix_output A list provided by \code{\link{mixmod_regression}} or
#'   \code{\link{mixmod_em}}, which consists of values necessary to visualize the
#'   subgroups.The default value of \code{mix_output} is \code{NULL}.
#'
#' @seealso \code{\link{plot_prob}}
#'
#' @examples
#' # Data is taken from given reference:
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' status <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' id <- 1:length(hours)
#'
#' # Example 1 - Using result of mixmod_em:
#' mix_mod_em <- mixmod_em(
#'   x = hours,
#'   status = status,
#'   distribution = "weibull",
#'   conf_level = 0.95
#' )
#'
#' plot_weibull_em <- plot_prob_mix(
#'   x = hours,
#'   status = status,
#'   id = id,
#'   distribution = "weibull",
#'   mix_output = mix_mod_em,
#'   title_main = "Weibull Mixture EM",
#'   title_x = "Time in Hours",
#'   title_y = "Probability of Failure",
#'   title_trace = "Subgroup"
#' )
#'
#' # Example 2 - Using result of mixmod_regression:
#' john <- estimate_cdf(
#'   x = hours,
#'   status = status,
#'   id = id,
#'   methods = "johnson"
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
#'   id = id,
#'   distribution = "weibull",
#'   mix_output = mix_mod_reg,
#'   title_main = "Weibull Mixture Regression",
#'   title_x = "Time in Hours",
#'   title_y = "Probability of Failure",
#'   title_trace = "Subgroup"
#' )
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
#' This function adds one or multiple estimated regression lines to an existing
#' probability plot (\code{\link{plot_prob}}). Depending on the output of the
#' functions \code{\link{rank_regression}}, \code{ml_estimation},
#' \code{\link{mixmod_regression}} or \code{\link{mixmod_em}} one or
#' multiple lines are plotted.
#'
#' The name of the legend entry is a combination of the \code{title_trace} and
#' the number of determined subgroups from \code{\link{mixmod_regression}} or
#' \code{\link{mixmod_em}}. If \code{title_trace = "Line"} and the
#' data could be splitted in two groups, the legend entries would be "Line: 1"
#' and "Line: 2".
#'
#' @usage
#' ## S3 method for classes 'model_estimation', 'model_estimation_list',
#' ## 'mixmod_regression', 'mixmod_regression_list', 'mixmod_em'
#' plot_mod(p_obj, x, title_trace = "Fit")
#'
#' @section S3 methods:
#'
#' | **Class** | **Returned from** | **Condition** |
#' | --- | --- | --- |
#' | \code{model_estimation} | \code{\link{rank_regression}} | One method in \code{\link{estimate_cdf}} |
#' | \code{model_estimation} | \code{\link{mixmod_regression}} | One method in \code{\link{estimate_cdf}}. No subgroups determined. |
#' | \code{model_estimation} | \code{\link{ml_estimation}} | - |
#' | \code{model_estimation_list} | \code{\link{rank_regression}} | Multiple methods in \code{\link{estimate_cdf}} |
#' | \code{mixmod_regression} | \code{\link{mixmod_regression}} | One method in \code{\link{estimate_cdf}} |
#' | \code{mixmod_regression_list} | \code{\link{mixmod_regression}} | Multiple methods in \code{\link{estimate_cdf}} |
#' | \code{mixmod_em} | \code{\link{mixmod_em}} | - |
#'
#'
#' @param p_obj A plot object returned from \code{\link{plot_prob}}.
#' @param x An object of aforementioned classes. See 'S3 methods'.
#' @inheritParams plot_prob.cdf_estimation
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a plot object containing the probability plot with
#'   plotting positions and the estimated regression line(s).
#'
#' @examples
#'
#' data <- reliability_data(
#'   data = alloy,
#'   x = cycles,
#'   status = status
#' )
#'
#' prob_tbl <- estimate_cdf(data, methods = c("johnson", "kaplan"))
#'
#' # Example 1 - Probability Plot and Regression Line Three-Parameter-Weibull:
#' plot_weibull <- plot_prob(
#'   prob_tbl,
#'   distribution = "weibull",
#'   title_main = "Three-Parametric Weibull",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' rr_weibull <- rank_regression(
#'   prob_tbl,
#'   distribution = "weibull3",
#'   conf_level = 0.9
#' )
#'
#' plot_reg_weibull <- plot_mod(
#'   p_obj = plot_weibull,
#'   x = rr_weibull,
#'   title_trace = "Estimated Weibull CDF"
#' )
#'
#' # Example 2 - Probability Plot and Regression Line Three-Parameter-Lognormal:
#' plot_lognormal <- plot_prob(
#'   prob_tbl,
#'   distribution = "lognormal",
#'   title_main = "Three-Parametric Lognormal",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' rr_lognormal <- rank_regression(
#'   prob_tbl,
#'   distribution = "lognormal3",
#'   conf_level = 0.9
#' )
#'
#' plot_reg_lognormal <- plot_mod(
#'   p_obj = plot_lognormal,
#'   x = rr_lognormal,
#'   title_trace = "Estimated Lognormal CDF"
#' )
#'
#' @md
#' @export
#'
plot_mod <- function(p_obj, x, ...) {
  UseMethod("plot_mod", x)
}



#' @rdname plot_mod
#'
#' @usage NULL
#'
#' @export
#'
plot_mod.model_estimation <- function(p_obj, x, title_trace = "Fit", ...) {

  failed_data <- dplyr::filter(x$data, status == 1)

  plot_mod.default(
    p_obj = p_obj,
    x = range(failed_data$x),
    loc_sc_params = x$loc_sc_params,
    distribution = x$distribution,
    title_trace = title_trace
  )
}



#' @rdname plot_mod
#'
#' @usage NULL
#'
#' @export
#'
plot_mod.model_estimation_list <- function(p_obj, x, title_trace = "Fit", ...) {
  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  methods <- names(x)

  tbl_pred <- purrr::map2_dfr(x, methods, function(model_estimation, method) {
    failed_data <- dplyr::filter(model_estimation$data, status == 1)

    plot_mod_helper(
      x = range(failed_data$x),
      loc_sc_params = model_estimation$loc_sc_params,
      distribution = model_estimation$distribution,
      method = method
    )
  })

  plot_mod_fun <- if (plot_method == "plotly") plot_mod_plotly else
    plot_mod_ggplot2

  plot_mod_fun(
    p_obj = p_obj,
    tbl_pred = tbl_pred,
    title_trace = title_trace
  )
}



#' @rdname plot_mod
#'
#' @usage NULL
#'
#' @export
plot_mod.mixmod_regression <- function(p_obj, x, title_trace = "Fit", ...) {
  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  tbl_pred <- purrr::map2_dfr(x, seq_along(x), function(model_estimation, index) {
    method <- if (purrr::is_null(model_estimation$data$method)) {
      # Case mixmod_em (plot_mod.mixmod_em calls this method internally)
      as.character(index)
    } else {
      # Case mixmod_regression
      model_estimation$data$method[1]
    }

    plot_mod_mix_helper(
      model_estimation = model_estimation,
      method = method,
      group = as.character(index)
    )
  })

  plot_mod_fun <- if (plot_method == "plotly") plot_mod_plotly else
    plot_mod_ggplot2

  plot_mod_fun(
    p_obj = p_obj,
    tbl_pred = tbl_pred,
    title_trace = title_trace
  )
}



#' @rdname plot_mod
#'
#' @usage NULL
#'
#' @export
plot_mod.mixmod_regression_list <- function(p_obj,
                                            x,
                                            title_trace = "Fit",
                                            ...
) {
  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  tbl_pred <- purrr::map2_dfr(x, names(x), function(mixmod_regression, method) {
    purrr::map2_dfr(
      mixmod_regression,
      seq_along(mixmod_regression),
      function(model_estimation, index) {
        plot_mod_mix_helper(
          model_estimation = model_estimation,
          method = method,
          group = as.character(index)
        )
      }
    )
  })

  plot_mod_fun <- if (plot_method == "plotly") plot_mod_plotly else
    plot_mod_ggplot2

  plot_mod_fun(
    p_obj = p_obj,
    tbl_pred = tbl_pred,
    title_trace = title_trace
  )
}



#' @rdname plot_mod
#'
#' @usage NULL
#'
#' @export
plot_mod.mixmod_em <- function(p_obj, x, title_trace = "Fit", ...) {

  # Remove em results to get model_estimation_list
  model_estimation_list <- x[-length(x)]

  plot_mod.mixmod_regression(
    p_obj = p_obj,
    x = model_estimation_list,
    title_trace = title_trace
  )
}



#' Add Estimated Population Line to a Probability Plot
#'
#' @inherit plot_mod description return references
#'
#' @param x A numeric vector containing the x-coordinates of the regression line.
#' @param loc_sc_params A (named) numeric vector of estimated location
#'   and scale parameters for a specified distribution. The order of
#'   elements is important. First entry needs to be the location
#'   parameter \eqn{\mu} and the second element needs to be the scale
#'   parameter \eqn{\sigma}. If a three-parametric model is used the third element
#'   is the threshold parameter \eqn{\gamma}.
#' @inheritParams plot_mod.model_estimation
#' @inheritParams plot_prob.default
#'
#' @return Returns a plot object containing the probability plot with
#' plotting positions and the estimated regression line(s).
#'
#' @seealso \code{\link{plot_mod}}
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#'
#' data <- reliability_data(x = cycles, status = status)
#'
#' tbl_john <- estimate_cdf(data, methods = c("johnson", "nelson"))
#'
#' # Example 1: Probability Plot and Regression Line Three-Parameter-Weibull:
#' plot_weibull <- plot_prob(
#'   tbl_john,
#'   distribution = "weibull",
#'   title_main = "Three-Parametric Weibull",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' mrr <- rank_regression(
#'   tbl_john,
#'   distribution = "weibull3",
#'   conf_level = .90
#' )
#'
#' plot_reg_weibull <- plot_mod(
#'   p_obj = plot_weibull,
#'   x = mrr,
#'   title_trace = "Estimated Weibull CDF"
#' )
#'
#'
#'
#' # Example 2: Probability Plot and Regression Line Three-Parameter-Lognormal:
#' plot_lognormal <- plot_prob(
#'   tbl_john,
#'   distribution = "lognormal",
#'   title_main = "Three-Parametric Lognormal",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' mrr_ln <- rank_regression(
#'   tbl_john,
#'   distribution = "lognormal3",
#'   conf_level = .90
#' )
#'
#' plot_reg_lognormal <- plot_mod(
#'   p_obj = plot_lognormal,
#'   x = mrr_ln,
#'   title_trace = "Estimated Lognormal CDF"
#' )
#'
#' @export
#'
plot_mod.default <- function(p_obj,
                             x,
                             loc_sc_params,
                             distribution = c(
                              "weibull", "lognormal", "loglogistic", "normal",
                              "logistic", "sev", "weibull3", "lognormal3",
                              "loglogistic3"
                             ),
                             title_trace = "Fit",
                             ...
) {

  distribution <- match.arg(distribution)

  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  tbl_pred <- plot_mod_helper(
    x, loc_sc_params, distribution
  )

  plot_mod_fun <- if (plot_method == "plotly") plot_mod_plotly else
    plot_mod_ggplot2

  plot_mod_fun(
    p_obj = p_obj,
    tbl_pred = tbl_pred,
    title_trace = title_trace
  )
}


#' Adding Estimated Population Lines of a Separated Mixture Model to a
#' Probability Plot
#'
#' This function adds one or multiple estimated regression lines to an existing
#' probability plot (\code{\link{plot_prob}}). Depending on the output of the
#' function \code{\link{mixmod_regression}} or \code{\link{mixmod_em}} one or
#' multiple lines are plotted.
#'
#' The name of the legend entry is a combination of the \code{title_trace} and
#' the number of determined subgroups. If \code{title_trace = "Line"} and the
#' data could be splitted in two groups, the legend entries would be "Line: 1"
#' and "Line: 2".
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @param p_obj A plot object provided by function \code{\link{plot_prob}}.
#' @param mix_output A list provided by \code{\link{mixmod_regression}} or
#'   \code{\link{mixmod_em}}, which consists of elements necessary to visualize
#'   the regression lines.
#' @inheritParams plot_mod.default
#' @inheritParams plot_prob.default
#'
#' @return Returns a plot object containing the probability plot with
#'   plotting positions and estimated regression line(s).
#'
#' @examples
#' # Data is taken from given reference:
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' id <- 1:length(hours)
#'
#' # Example 1 - Using result of mixmod_em in mix_output:
#' mix_mod_em <- mixmod_em(x = hours, status = state, distribution = "weibull",
#'                         conf_level = 0.95, k = 2, method = "EM", n_iter = 150)
#'
#' plot_weibull_em <- plot_prob(x = hours,
#'                                  status = state,
#'                                  id = id,
#'                                  distribution = "weibull",
#'                                  mix_output = mix_mod_em,
#'                                  title_main = "Weibull Mixture EM",
#'                                  title_x = "Time in Hours",
#'                                  title_y = "Probability of Failure",
#'                                  title_trace = "Subgroup")
#'
#' plot_weibull_emlines <- plot_mod_mix(p_obj = plot_weibull_em,
#'                                    x = hours,
#'                                    status = state,
#'                                    mix_output = mix_mod_em,
#'                                    distribution = "weibull",
#'                                    title_trace = "Fitted Line")
#'
#' # Example 2 - Using result of mixmod_regression in mix_output:
#' john <- johnson_method(x = hours, status = state)
#' mix_mod_reg <- mixmod_regression(x = john$x,
#'                                  y = john$prob,
#'                                  status = john$status,
#'                                  distribution = "weibull")
#'
#' plot_weibull_reg <- plot_prob(x = hours,
#'                                   status = state,
#'                                   id = id,
#'                                   distribution = "weibull",
#'                                   mix_output = mix_mod_reg,
#'                                   title_main = "Weibull Mixture Regression",
#'                                   title_x = "Time in Hours",
#'                                   title_y = "Probability of Failure",
#'                                   title_trace = "Subgroup")
#'
#' plot_weibull_reglines <- plot_mod_mix(p_obj = plot_weibull_reg,
#'                                    x = hours,
#'                                    status = state,
#'                                    mix_output = mix_mod_reg,
#'                                    distribution = "weibull",
#'                                    title_trace = "Fitted Line")
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

  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  plot_mod(
    p_obj = p_obj,
    x = mix_output,
    title_trace = title_trace
  )
}


#' Add Confidence Region(s) for Quantiles or Probabilities
#'
#' This function is used to add estimated confidence region(s) to an existing
#' probability plot which also includes the estimated regression line.
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=plot_conf.confint]{confint}}}{
#'     Preferred. Provide the output of either \code{\link{confint_betabinom}}
#'     or \code{\link{confint_fisher}} directly.
#'   }
#'   \item{\code{\link[=plot_conf.default]{default}}}{
#'     Provide \code{x}, \code{y}, \code{distribution} and \code{direction}
#'     manually.
#'   }
#' }
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a plot object containing the probability plot with
#'   plotting positions, the estimated regression line and the estimated
#'   confidence region(s).
#'
#' @export
#'
plot_conf <- function(p_obj, x, ...) {
  # Dispatch on x, dispatch on p_obj is done in plot_conf.default
  UseMethod("plot_conf", x)
}

#' Add Confidence Region(s) for Quantiles or Probabilities
#'
#' @inherit plot_conf description return references
#'
#' @details
#' It is important that the length of the vectors provided as lists in \code{x}
#' and \code{y} match with the length of the vectors \code{x} and \code{y} in
#' the function \code{\link{plot_mod}}. For this reason the following procedure
#' is recommended:
#' \itemize{
#'   \item Calculate confidence intervals with the function
#'     \code{\link{confint_betabinom}} or \code{\link{confint_fisher}} and store
#'     it in a \code{data.frame}. For instance call it df.
#'   \item Inside \code{\link{plot_mod}} use the output \code{df$x}
#'     for \code{x} and \code{df$prob} for \code{y} of the function(s) named before.
#'   \item In \strong{Examples} the described approach is shown with code.}
#'
#' @param p_obj A plot object returned from \code{\link{plot_mod}}.
#' @param x A list containing the x-coordinates of the confidence region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param y A list containing the y-coordinates of the Confidence Region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param direction A character string specifying the direction of the plotted
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#' @inheritParams plot_prob.default
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#' id <- 1:length(cycles)
#'
#' df_john <- johnson_method(x = cycles, status = state, id = id)

#' # Example 1: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' mrr <- rank_regression(x = df_john$x,
#'                        y = df_john$prob,
#'                        status = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#'
#' conf_betabin <- confint_betabinom(x = df_john$x,
#'                                   status = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_params,
#'                                   distribution = "weibull3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_weibull <- plot_prob(x = df_john$x,
#'                           y = df_john$prob,
#'                           status = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Three-Parametric Weibull",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull,
#'                              x = conf_betabin$x,
#'                              y = conf_betabin$prob,
#'                              loc_sc_params = mrr$loc_sc_params,
#'                              distribution = "weibull3",
#'                              title_trace = "Estimated Weibull CDF")
#'
#' plot_conf_beta <- plot_conf(p_obj = plot_reg_weibull,
#'                             x = list(conf_betabin$x),
#'                             y = list(conf_betabin$lower_bound,
#'                                      conf_betabin$upper_bound),
#'                             direction = "y",
#'                             distribution = "weibull3",
#'                             title_trace = "Confidence Region")
#'
#' # Example 2: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' mrr_ln <- rank_regression(x = df_john$x,
#'                        y = df_john$prob,
#'                        status = df_john$status,
#'                        distribution = "lognormal3",
#'                        conf_level = .90)
#'
#' conf_betabin_ln <- confint_betabinom(x = df_john$x,
#'                                   status = df_john$status,
#'                                   loc_sc_params = mrr_ln$loc_sc_params,
#'                                   distribution = "lognormal3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_lognormal <- plot_prob(x = df_john$x,
#'                           y = df_john$prob,
#'                           status = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "lognormal",
#'                           title_main = "Three-Parametric Lognormal",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal,
#'                              x = conf_betabin_ln$x,
#'                              y = conf_betabin_ln$prob,
#'                              loc_sc_params = mrr_ln$loc_sc_params,
#'                              distribution = "lognormal3",
#'                              title_trace = "Estimated Lognormal CDF")
#'
#' plot_conf_beta_ln <- plot_conf(p_obj = plot_reg_lognormal,
#'                             x = list(conf_betabin_ln$x),
#'                             y = list(conf_betabin_ln$lower_bound,
#'                                      conf_betabin_ln$upper_bound),
#'                             direction = "y",
#'                             distribution = "lognormal3",
#'                             title_trace = "Confidence Region")
#'
#' @export
#'
plot_conf.default <- function(
  p_obj, x, y,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "weibull3", "lognormal3", "loglogistic3"
  ),
  direction = c("y", "x"),
  title_trace = "Confidence Limit",
  ...
) {

  deprecate_soft("2.0.0", "plot_conf.default()", "plot_conf.confint()")

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  # Extracting tbl_mod
  tbl_mod <- if (plot_method == "plotly") {
    plotly::plotly_data(p_obj)
  } else {
    p_obj$layers[[2]]$data
  }

  tbl_p <- plot_conf_helper(
    tbl_mod, x, y, direction, distribution
  )

  plot_conf_fun <- if (plot_method == "plotly") plot_conf_plotly else
    plot_conf_ggplot2

  plot_conf_fun(
    p_obj, tbl_p, title_trace
  )
}

#' Add Confidence Region(s) for Quantiles or Probabilities
#'
#' @inherit plot_conf description return references
#'
#' @inheritParams plot_conf.default
#' @inheritParams plot_prob.cdf_estimation
#' @param p_obj A plot object returned from \code{\link{plot_prob}}.
#' @param x Confindence interval as returned by \code{\link{confint_betabinom}}
#'   or \code{\link{confint_fisher}}.
#'
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#' id <- 1:length(cycles)
#'
#' df_john <- johnson_method(x = cycles, status = state, id = id)

#' # Example 1: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' mrr <- rank_regression(x = df_john$x,
#'                        y = df_john$prob,
#'                        status = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#'
#' conf_betabin <- confint_betabinom(x = df_john$x,
#'                                   status = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_params,
#'                                   distribution = "weibull3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_weibull <- plot_prob(x = df_john$x,
#'                           y = df_john$prob,
#'                           status = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Three-Parametric Weibull",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull,
#'                              x = conf_betabin$x,
#'                              y = conf_betabin$prob,
#'                              loc_sc_params = mrr$loc_sc_params,
#'                              distribution = "weibull3",
#'                              title_trace = "Estimated Weibull CDF")
#'
#' plot_conf_beta <- plot_conf(p_obj = plot_reg_weibull,
#'                             x = list(conf_betabin$x),
#'                             y = list(conf_betabin$lower_bound,
#'                                      conf_betabin$upper_bound),
#'                             direction = "y",
#'                             distribution = "weibull3",
#'                             title_trace = "Confidence Region")
#'
#' # Example 2: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' mrr_ln <- rank_regression(x = df_john$x,
#'                        y = df_john$prob,
#'                        status = df_john$status,
#'                        distribution = "lognormal3",
#'                        conf_level = .90)
#'
#' conf_betabin_ln <- confint_betabinom(x = df_john$x,
#'                                   status = df_john$status,
#'                                   loc_sc_params = mrr_ln$loc_sc_params,
#'                                   distribution = "lognormal3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_lognormal <- plot_prob(x = df_john$x,
#'                           y = df_john$prob,
#'                           status = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "lognormal",
#'                           title_main = "Three-Parametric Lognormal",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal,
#'                              x = conf_betabin_ln$x,
#'                              y = conf_betabin_ln$prob,
#'                              loc_sc_params = mrr_ln$loc_sc_params,
#'                              distribution = "lognormal3",
#'                              title_trace = "Estimated Lognormal CDF")
#'
#' plot_conf_beta_ln <- plot_conf(p_obj = plot_reg_lognormal,
#'                             x = list(conf_betabin_ln$x),
#'                             y = list(conf_betabin_ln$lower_bound,
#'                                      conf_betabin_ln$upper_bound),
#'                             direction = "y",
#'                             distribution = "lognormal3",
#'                             title_trace = "Confidence Region")
#'
#' @export
#'
plot_conf.confint <- function(p_obj,
                              x,
                              mod,
                              title_trace_mod = "Fit",
                              title_trace_conf = "Confidence Limit",
                              ...
) {

  # Plot method is determined by p_obj
  plot_method <- if (inherits(p_obj, "gg")) {
    "ggplot2"
  } else if (inherits(p_obj, "plotly")) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  p_mod <- if (inherits(mod, "model_estimation")) {
    failed_data <- dplyr::filter(mod$data, status == 1)

    plot_mod.default(
      p_obj = p_obj,
      x = x$x,
      loc_sc_params = mod$loc_sc_params,
      distribution = mod$distribution,
      title_trace = title_trace_mod
    )
  } else if (inherits(mod, "model_estimation_list")) {
    methods <- names(mod)

    tbl_pred <- purrr::map2_dfr(mod, methods, function(model_estimation, method) {
      plot_mod_helper(
        x = x$x,
        loc_sc_params = model_estimation$loc_sc_params,
        distribution = model_estimation$distribution,
        method = method
      )
    })

    plot_mod_fun <- if (plot_method == "plotly") plot_mod_plotly else
      plot_mod_ggplot2

    plot_mod_fun(
      p_obj = p_obj,
      tbl_pred = tbl_pred,
      title_trace = title_trace_mod
    )
  } else {
    stop("'mod' must be an object returned from 'ml_estimation()' or
         'rank_regression()'")
  }

  distribution <- x$distribution[1]

  tbl_p <- plot_conf_helper_2(
    x, distribution
  )

  plot_conf_fun <- if (plot_method == "plotly") plot_conf_plotly else
    plot_conf_ggplot2

  plot_conf_fun(
    p_mod, tbl_p, title_trace_conf
  )
}

#' Add Population Line(s) to an Existing Grid
#'
#' @description
#' This function adds one or multiple linearized CDFs to an existing plot grid.
#'
#' @details
#' \code{loc_sc_params_tbl} is a data.frame with two or three columns. For
#' location-scale distributions the first column contains the location parameter
#' and the second column contains the scale parameter. For three-parametric
#' distributions the third column contains the threshold parameter.
#'
#' If only one population line should be displayed, a numeric vector of length
#' two or three is also supported (\code{c(loc, sc)} or \code{c(loc, sc, thres)}).
#'
#' @param p_obj A plot object to which the population lines are added or
#'   \code{NULL}. If \code{NULL} the population lines are plotted in an empty grid.
#' @param x A numeric vector of length two or greater used for the x coordinates
#'   of the population line. If \code{length(x) == 2} a sequence of length 200
#'   between \code{x[1]} and \code{x[2]} is created. This sequence is equidistant
#'   with respect to the scale of the x axis. If \code{length(x) > 2} the elements
#'   of \code{x} are the x coordinates of the population line.
#' @param loc_sc_params_tbl A tibble. See 'Details'.
#' @param distribution Supposed distribution of the random variable. In the
#'   context of this function \code{"weibull"}, \code{"lognormal"} and
#'   \code{"loglogistic"} stand for the two- \strong{and} the three-parametric
#'   version of the respective distribution. The distinction is made via
#'   \code{loc_sc_params_tbl}.
#' @param tol The failure probability is restricted to the interval
#'   \eqn{[tol, 1 - tol]}. The default value is in accordance with the decimal
#'   places shown in the hover for \code{plot_method = "plotly"}.
#' @param plot_method Plot package, which produces the visual output. Only
#'   used with \code{p_obj = NULL}, otherwise \code{p_obj} is used to determine
#'   the plot method.
#' @inheritParams plot_prob.default
#'
#' @return A plot object which contains the linearized CDF(s).
#'
#' @examples
#' x <- rweibull(n = 100, shape = 1, scale = 20000)
#'
#' grid_weibull <- plot_layout(
#'   x = x,
#'   distribution = "weibull",
#'   title_main = "Weibull Analysis",
#'   title_x = "Time to Failure",
#'   title_y = "Failure Probability"
#' )
#' # Example 1: Two-parametric straight line
#' pop_weibull <- plot_pop(
#'   p_obj = grid_weibull,
#'   x = range(x),
#'   loc_sc_params_tbl = c(log(20000), 1),
#'   distribution = "weibull",
#'   title_trace = "Population"
#' )
#'
#' # Example 2: Three-parametric curved line
#' x2 <- rweibull(n = 100, shape = 1, scale = 20000) + 5000
#'
#' pop_weibull_2 <- plot_pop(
#'   p_obj = NULL,
#'   x = x2,
#'   loc_sc_params_tbl = c(log(20000 - 5000), 1, 5000),
#'   distribution = "weibull",
#'   title_trace = "Population"
#' )
#'
#' # Example 3: Multiple lines
#' pop_weibull_3 <- plot_pop(
#'   p_obj = NULL,
#'   x = x,
#'   loc_sc_params_tbl = tibble(
#'     p_1 = c(log(20000), log(20000), log(20000)),
#'     p_2 = c(1, 1.5, 2)
#'     ),
#'   distribution = "weibull",
#'   title_trace = "Population",
#'   plot_method = "ggplot2"
#' )
#'
#' # Example 4: Compare two- and three-parametric distributions
#' pop_weibull_4 <- plot_pop(
#'   p_obj = NULL,
#'   x = x,
#'   loc_sc_params_tbl = tibble(
#'     param_1 = c(log(20000), log(20000)),
#'     param_2 = c(1, 1),
#'     param_3 = c(NA, 5)
#'   )
#' )
#'
#' @export
plot_pop <- function(
  p_obj = NULL, x, loc_sc_params_tbl,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
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
  } else {
    # Plot method is determined by p_obj
    plot_method <- if (inherits(p_obj, "gg")) {
      "ggplot2"
    } else if (inherits(p_obj, "plotly")) {
      "plotly"
    }  else {
      stop(
        "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
      )
    }
  }

  # Support vector instead of tibble for ease of use in loc_sc_params_tbl
  if (!inherits(loc_sc_params_tbl, "data.frame")) {
    loc_sc_params_tbl <- tibble::tibble(
      loc = loc_sc_params_tbl[1],
      sc = loc_sc_params_tbl[2],
      # NA if length 2
      thres = loc_sc_params_tbl[3]
    )
  }

  # Add thres column if not present
  if (ncol(loc_sc_params_tbl) == 2) {
    loc_sc_params_tbl$thres <- NA_real_
  }

  # Ensure correct naming
  names(loc_sc_params_tbl) <- c("loc", "sc", "thres")

  tbl_pop <- plot_pop_helper(x, loc_sc_params_tbl, distribution, tol)

  plot_pop_fun <- if (plot_method == "plotly") plot_pop_plotly else
    plot_pop_ggplot2

  plot_pop_fun(
    p_obj, tbl_pop, title_trace
  )
}
