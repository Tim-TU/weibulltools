#' Layout of the Probability Plot
#'
#' This function is used to create the layout of a probability plot.
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
#' @return Returns a plotly object which contains the layout
#'   that is used for probability plotting.
#' @importFrom magrittr "%>%"
#'
#' @export
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

  deprecate_soft("1.1.0", "plot_layout()")

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
#' The marker label for x and y are determined by the first word provided in the
#' argument \code{title_x} respective \code{title_y}, i.e. if
#' \code{title_x = "Mileage in km"} the x label of the marker is "Mileage".
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=plot_prob.cdf_estimation]{cdf_estimation}}}{
#'     Preferred. Provide the output of \code{\link{estimate_cdf}} directly.
#'   }
#'   \item{\code{\link[=plot_prob.default]{default}}}{
#'     Provide \code{x}, \code{y}, \code{event} and \code{id} manually.
#'   }
#' }
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a plotly object containing the layout of the probability plot
#'   provided by \code{\link{plot_layout}} and the plotting positions.
#'
#' @export
#'
plot_prob <- function(
  x, ...
) {
  UseMethod("plot_prob")
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
#' @param event A vector of binary data (0 or 1) indicating whether unit \emph{i}
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
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#' data <- reliability_data(x = cycles, status = status)
#'
#' tbl_john <- estimate_cdf(data, methods = c("johnson", "kaplan"))
#'
#' # Example 1: Probability Plot Weibull:
#' plot_weibull <- plot_prob(
#'   tbl_john,
#'   distribution = "weibull",
#'   title_main = "Weibull Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' # Example 2: Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   tbl_john,
#'   distribution = "lognormal",
#'   title_main = "Lognormal Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' @export
plot_prob.default <- function(
  x, y, event,
  id = rep("XXXXXX", length(x)),
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic", title_y = "Unreliability",
  title_trace = "Sample",
  plot_method = c("plotly", "ggplot2")
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  cdf_estimation <- tibble::tibble(
    characteristic = x, prob = y, status = event, id = id,
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

#' Probability Plotting Method for Univariate Lifetime Distributions
#'
#' @inherit plot_prob description details return references
#'
#' @param x CDF estimation returned from \code{\link{estimate_cdf}}.
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
#' @examples
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' status <- c(rep(0, 5), rep(1, 67))
#' data <- reliability_data(x = cycles, status = status)
#'
#' tbl_john <- estimate_cdf(data, methods = c("johnson", "kaplan"))
#'
#' # Example 1: Probability Plot Weibull:
#' plot_weibull <- plot_prob(
#'   tbl_john,
#'   distribution = "weibull",
#'   title_main = "Weibull Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
#'
#' # Example 2: Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(
#'   tbl_john,
#'   distribution = "lognormal",
#'   title_main = "Lognormal Analysis",
#'   title_x = "Cycles",
#'   title_y = "Probability of Failure in %",
#'   title_trace = "Failed Items"
#' )
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
  plot_method = c("plotly", "ggplot2")
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

plot_prob_ <- function(
  cdf_estimation, distribution, title_main, title_x, title_y, title_trace, plot_method
) {

  prob_tbl <- plot_prob_helper(
    cdf_estimation, distribution
  )

  p_obj <- plot_layout(
    x = prob_tbl$characteristic,
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
    prob_tbl = prob_tbl,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace
  )
}


#' Probability Plot for Separated Mixture Models
#'
#' This function is used to apply the graphical technique of probability
#' plotting to univariate mixture models that were separated with functions
#' \code{\link{mixmod_regression}} or \code{\link{mixmod_em}}.
#'
#' Depending on the separation method the function \code{\link{johnson_method}}
#' is called in various ways. If \code{mixmod_regression} is used, \code{johnson_method}
#' is applied to all data. If data was splitted by \code{mixmod_em} the function
#' \code{johnson_method} is applied to subgroup-specific data. The calculated plotting
#' positions are colored regarding the obtained split of the used splitting function.
#' If \code{mix_output = NULL} \code{johnson_method} is applied to all data, too.
#' The obtained plot is then equal to \code{\link{plot_prob}}. See \strong{Examples}
#' for all three cases.
#'
#' In \code{\link{mixmod_regression}} a maximum of three subgroups can be determined
#' and thus being plotted. The intention of this function is to give the
#' user a hint for the existence of a mixture model. An in-depth analysis should
#' be done afterwards.
#'
#' The marker label for x is determined by the first word provided in the
#' argument \code{title_x}, i.e. if \code{title_x = "Mileage in km"} the x label
#' of the marker is "Mileage".
#'
#' The marker label for y is determined by the string provided in the
#' argument \code{title_y}, i.e. if \code{title_y = "Probability in percent"} the y
#' label of the marker is "Probability".
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
#' @return Returns a plotly object containing the layout of the probability plot
#'   provided by \code{\link{plot_layout}} and the plotting positions.
#' @export
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
#' # Example 1 - mix_output = NULL:
#' plot_weibull <- plot_prob_mix(x = hours,
#'                               event = state,
#'                               id = id,
#'                               distribution = "weibull",
#'                               mix_output = NULL,
#'                               title_main = "Weibull Probability Plot",
#'                               title_x = "Time in Hours",
#'                               title_y = "Probability of Failure",
#'                               title_trace = "Failed Items")
#'
#' # Example 2 - Using result of mixmod_em in mix_output:
#' mix_mod_em <- mixmod_em(x = hours, event = state, distribution = "weibull",
#'                         conf_level = 0.95, k = 2, method = "EM", n_iter = 150)
#'
#' plot_weibull_em <- plot_prob_mix(x = hours,
#'                                  event = state,
#'                                  id = id,
#'                                  distribution = "weibull",
#'                                  mix_output = mix_mod_em,
#'                                  title_main = "Weibull Mixture EM",
#'                                  title_x = "Time in Hours",
#'                                  title_y = "Probability of Failure",
#'                                  title_trace = "Subgroup")
#'
#' # Example 3 - Using result of mixmod_regression in mix_output:
#' john <- johnson_method(x = hours, event = state)
#' mix_mod_reg <- mixmod_regression(
#'   x = john$characteristic,
#'   y = john$prob,
#'   event = john$status,
#'   distribution = "weibull"
#' )
#'
#' plot_weibull_reg <- plot_prob_mix(
#'   x = hours,
#'   event = state,
#'   id = id,
#'   distribution = "weibull",
#'   mix_output = mix_mod_reg,
#'   title_main = "Weibull Mixture Regression",
#'   title_x = "Time in Hours",
#'   title_y = "Probability of Failure",
#'   title_trace = "Subgroup"
#' )
#'
plot_prob_mix <- function(x, ...) {
  UseMethod("plot_prob_mix")
}

#' @export
plot_prob_mix.default <- function(
  x,
  event,
  id = rep("XXXXXX", length(x)),
  distribution = c("weibull", "lognormal", "loglogistic"),
  mix_output = NULL,
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  title_trace = "Sample",
  plot_method = c("plotly", "ggplot2")
) {

  distribution <- match.arg(distribution)
  plot_method <- match.arg(plot_method)

  if (("em_results" %in%  names(mix_output)) && distribution != "weibull") {
    stop("No valid distribution! Use weibull to visualize EM results")
  }

  data <- reliability_data(x = x, status = event, id = id)
  tbl_group <- plot_prob_mix_helper(
    data, distribution, mix_output, title_trace
  )

  # Plot layout:
  p_obj <- plot_layout(
    x = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    plot_method = plot_method
  )

  plot_prob_mix_fun <- if (plot_method == "plotly") plot_prob_mix_plotly else
    plot_prob_mix_ggplot2

  plot_prob_mix_fun(
    p_obj = p_obj,
    tbl_group = tbl_group,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    title_trace = title_trace
  )
}

#' Add Estimated Population Line(s) to a Probability Plot
#'
#' This function adds regression lines to an existing probability plot using a
#' model estimated by \code{\link{rank_regression}} or
#' \code{\link{ml_estimation}}.
#'
#' @section Methods (by class):
#' \describe{
#'   \item{\code{\link[=plot_mod.model_estimation]{model_estimation(_list)}}}{
#'     Preferred. Provide the output of either \code{\link{ml_estimation}} or
#'     \code{\link{rank_regression}} directly.
#'   }
#'   \item{\code{\link[=plot_mod.default]{default}}}{
#'     Provide \code{x}, \code{loc_sc_params} and \code{distribution} manually.
#'   }
#' }
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions and the estimated regression line.
#'
#' @export
#'
plot_mod <- function(
  p_obj, x, ...
) {
  UseMethod("plot_mod", x)
}

#' Add Estimated Population Line(s) to a Probability Plot
#'
#' @inherit plot_mod description return references
#'
#' @param p_obj A plot object returned from \code{\link{plot_prob}}.
#' @param x An object of class \code{model_estimation} or
#'   \code{model_estimation_list} returned by either
#'   \code{\link{ml_estimation}} or \code{\link{rank_regression}}.
#' @inheritParams plot_prob.cdf_estimation
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
plot_mod.model_estimation <- function(
  p_obj, x, title_trace = "Fit"
) {

  plot_mod.default(
    p_obj = p_obj,
    x = range(x$data$characteristic),
    loc_sc_params = x$loc_sc_params,
    distribution = x$distribution,
    title_trace = title_trace
  )
}

#' @rdname plot_mod.model_estimation
#'
#' @export
#'
plot_mod.model_estimation_list <- function(
  p_obj, x, title_trace = "Fit"
) {
  # Plot method is determined by p_obj
  plot_method <- if ("gg" %in% class(p_obj)) {
    "ggplot2"
  } else if ("plotly" %in% class(p_obj)) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  methods <- names(x)

  tbl_pred <- purrr::map2_dfr(x, methods, function(model_estimation, method) {
    plot_mod_helper(
      x = range(model_estimation$data$characteristic),
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
#'   model_estimation = mrr,
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
#'   model_estimation = mrr_ln,
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
                             title_trace = "Fit"
) {

  distribution <- match.arg(distribution)

  # Plot method is determined by p_obj
  plot_method <- if ("gg" %in% class(p_obj)) {
    "ggplot2"
  } else if ("plotly" %in% class(p_obj)) {
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
#' probability plot (\code{\link{plot_prob_mix}}). Depending on the output of the
#' function \code{\link{mixmod_regression}} or \code{\link{mixmod_em}} one or
#' multiple lines are plotted.
#'
#' The name of the legend entry is a combination of the \code{title_trace} and the
#' number of determined subgroups. If \code{title_trace = "Line"} and the data
#' could be splitted in two groups, the legend entries would be "Line 1" and "Line 2".
#'
#' @encoding UTF-8
#' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
#'
#' @param p_obj A plotly object provided by function \code{\link{plot_prob_mix}}.
#' @param mix_output A list provided by \code{\link{mixmod_regression}} or
#'   \code{\link{mixmod_em}}, which consists of elements necessary to visualize
#'   the regression lines.
#' @inheritParams plot_mod.default
#' @inheritParams plot_prob.default
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions and estimated regression line(s).
#' @export
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
#' mix_mod_em <- mixmod_em(x = hours, event = state, distribution = "weibull",
#'                         conf_level = 0.95, k = 2, method = "EM", n_iter = 150)
#'
#' plot_weibull_em <- plot_prob_mix(x = hours,
#'                                  event = state,
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
#'                                    event = state,
#'                                    mix_output = mix_mod_em,
#'                                    distribution = "weibull",
#'                                    title_trace = "Fitted Line")
#'
#' # Example 2 - Using result of mixmod_regression in mix_output:
#' john <- johnson_method(x = hours, event = state)
#' mix_mod_reg <- mixmod_regression(x = john$characteristic,
#'                                  y = john$prob,
#'                                  event = john$status,
#'                                  distribution = "weibull")
#'
#' plot_weibull_reg <- plot_prob_mix(x = hours,
#'                                   event = state,
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
#'                                    event = state,
#'                                    mix_output = mix_mod_reg,
#'                                    distribution = "weibull",
#'                                    title_trace = "Fitted Line")


plot_mod_mix <- function(p_obj, x, event, mix_output,
  distribution = c("weibull", "lognormal", "loglogistic"),
  title_trace = "Fit") {

  distribution <- match.arg(distribution)

  if (("em_results" %in%  names(mix_output)) && distribution != "weibull") {
    stop("No valid distribution! Use weibull to visualize EM results")
  }

  # Plot method is determined by p_obj
  plot_method <- if ("gg" %in% class(p_obj)) {
    "ggplot2"
  } else if ("plotly" %in% class(p_obj)) {
    "plotly"
  }  else {
    stop(
      "p_obj is not a valid plot object. Provide either a ggplot2 or a plotly
      plot object."
    )
  }

  tbl_group <- plot_mod_mix_helper(
    x, event, mix_output, distribution, title_trace
  )

  plot_mod_mix_fun <- if (plot_method == "plotly") plot_mod_mix_plotly else
    plot_mod_mix_ggplot2

  plot_mod_mix_fun(
    p_obj, tbl_group, title_trace
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
#' @return Returns a plotly object containing the probability plot with
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
#'   \item Inside \code{\link{plot_mod}} use the output \code{df$characteristic}
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
#' df_john <- johnson_method(x = cycles, event = state, id = id)

#' # Example 1: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#'
#' conf_betabin <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_params,
#'                                   distribution = "weibull3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Three-Parametric Weibull",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull,
#'                              x = conf_betabin$characteristic,
#'                              y = conf_betabin$prob,
#'                              loc_sc_params = mrr$loc_sc_params,
#'                              distribution = "weibull3",
#'                              title_trace = "Estimated Weibull CDF")
#'
#' plot_conf_beta <- plot_conf(p_obj = plot_reg_weibull,
#'                             x = list(conf_betabin$characteristic),
#'                             y = list(conf_betabin$lower_bound,
#'                                      conf_betabin$upper_bound),
#'                             direction = "y",
#'                             distribution = "weibull3",
#'                             title_trace = "Confidence Region")
#'
#' # Example 2: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' mrr_ln <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "lognormal3",
#'                        conf_level = .90)
#'
#' conf_betabin_ln <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr_ln$loc_sc_params,
#'                                   distribution = "lognormal3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_lognormal <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "lognormal",
#'                           title_main = "Three-Parametric Lognormal",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal,
#'                              x = conf_betabin_ln$characteristic,
#'                              y = conf_betabin_ln$prob,
#'                              loc_sc_params = mrr_ln$loc_sc_params,
#'                              distribution = "lognormal3",
#'                              title_trace = "Estimated Lognormal CDF")
#'
#' plot_conf_beta_ln <- plot_conf(p_obj = plot_reg_lognormal,
#'                             x = list(conf_betabin_ln$characteristic),
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
  title_trace = "Confidence Limit"
) {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  # Plot method is determined by p_obj
  plot_method <- if ("gg" %in% class(p_obj)) {
    "ggplot2"
  } else if ("plotly" %in% class(p_obj)) {
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
#' df_john <- johnson_method(x = cycles, event = state, id = id)

#' # Example 1: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Weibull:
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#'
#' conf_betabin <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_params,
#'                                   distribution = "weibull3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Three-Parametric Weibull",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull,
#'                              x = conf_betabin$characteristic,
#'                              y = conf_betabin$prob,
#'                              loc_sc_params = mrr$loc_sc_params,
#'                              distribution = "weibull3",
#'                              title_trace = "Estimated Weibull CDF")
#'
#' plot_conf_beta <- plot_conf(p_obj = plot_reg_weibull,
#'                             x = list(conf_betabin$characteristic),
#'                             y = list(conf_betabin$lower_bound,
#'                                      conf_betabin$upper_bound),
#'                             direction = "y",
#'                             distribution = "weibull3",
#'                             title_trace = "Confidence Region")
#'
#' # Example 2: Probability Plot, Regression Line and Confidence Bounds for Three-Parameter-Lognormal:
#' mrr_ln <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "lognormal3",
#'                        conf_level = .90)
#'
#' conf_betabin_ln <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr_ln$loc_sc_params,
#'                                   distribution = "lognormal3",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_lognormal <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "lognormal",
#'                           title_main = "Three-Parametric Lognormal",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal,
#'                              x = conf_betabin_ln$characteristic,
#'                              y = conf_betabin_ln$prob,
#'                              loc_sc_params = mrr_ln$loc_sc_params,
#'                              distribution = "lognormal3",
#'                              title_trace = "Estimated Lognormal CDF")
#'
#' plot_conf_beta_ln <- plot_conf(p_obj = plot_reg_lognormal,
#'                             x = list(conf_betabin_ln$characteristic),
#'                             y = list(conf_betabin_ln$lower_bound,
#'                                      conf_betabin_ln$upper_bound),
#'                             direction = "y",
#'                             distribution = "lognormal3",
#'                             title_trace = "Confidence Region")
#'
#' @export
#'
plot_conf.confint <- function(p_obj, x, title_trace) {
  bounds <- attr(x, "bounds")
  direction <- attr(x, "direction")
  distribution <- attr(x, "distribution")

  if (direction == "x") {
    x <- switch(
      bounds,
      "two_sided" = list(x$lower_bound, x$upper_bound),
      "lower" = list(x$lower_bound),
      "upper" = list(x$upper_bound)
    )

    y <- x$characteristic
  } else {
    x <- x$characteristic

    y <- switch(
      bounds,
      "two_sided" = list(x$lower_bound, x$upper_bound),
      "lower" = list(x$lower_bound),
      "upper" = list(x$upper_bound)
    )
  }

  plot_conf.default(
    p_obj, x, y, direction, distribution, title_trace
  )
}

#' Add Population Line(s) to an Existing Grid
#'
#' @description
#' This function adds one or multiple linearized CDFs to an existing plot grid.
#'
#' @details
#' \code{loc_sc_params_tbl} is a tibble with the following columns:
#'   \itemize{
#'     \item \code{loc}: Location parameter \eqn{\mu}.
#'     \item \code{sc}: Scale parameter \eqn{\sigma}.
#'   }
#'   If only one line should be displayed, a numeric vector of length two is
#'   also supported \code{c(loc, sc)}.
#'
#' @param p_obj A plot object to which the population lines are added or
#'   \code{NULL}. If \code{NULL} the population lines are added to an empty grid.
#' @param x A numeric vector of length two or greater used for the x coordinates
#'   of the population line. If \code{length(x) == 2} a sequence of length 200
#'   between \code{x[1]} and \code{x[2]} is created. This sequence is equidistant
#'   with respect to the scale of the x axis. If \code{length(x) > 2} the elements
#'   of \code{x} are the x coordinates of the population line.
#' @param loc_sc_params_tbl A tibble. See 'Details'.
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
#' # Example 1: One line
#' pop_weibull <- plot_pop(
#'   p_obj = grid_weibull,
#'   x = range(x),
#'   loc_sc_params_tbl = c(log(20000), 1),
#'   distribution = "weibull",
#'   title_trace = "Population"
#' )
#'
#' # Example 2: Multiple lines
#' pop_weibull2 <- plot_pop(
#'   p_obj = NULL,
#'   x = x,
#'   loc_sc_params_tbl = tibble(
#'     loc = c(log(20000), log(20000), log(20000)),
#'     sc = c(1, 1.5, 2)
#'     ),
#'   distribution = "weibull",
#'   title_trace = "Population",
#'   plot_method = "ggplot2"
#' )
#'
#' @export
plot_pop <- function(
  p_obj = NULL, x, loc_sc_params_tbl,
  distribution = c("weibull", "lognormal", "loglogistic"),
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
    plot_method <- if ("gg" %in% class(p_obj)) {
      "ggplot2"
    } else if ("plotly" %in% class(p_obj)) {
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
      sc = loc_sc_params_tbl[2]
      )
  }

  tbl_pop <- plot_pop_helper(x, loc_sc_params_tbl, distribution, tol)

  plot_pop_fun <- if (plot_method == "plotly") plot_pop_plotly else
    plot_pop_ggplot2

  plot_pop_fun(
    p_obj, tbl_pop, title_trace
  )
}
