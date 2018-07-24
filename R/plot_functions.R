#' Layout of the Probability Plot
#'
#' This function is used to create the layout of a probability plot.
#'
#' @param x a numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the grid of the plot.
#' @param distribution supposed distribution of the random variable. Can be
#'   \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}. Other
#'   distributions have not been implemented yet.
#' @param title_main a character string which is assigned to the main title
#'   of the plot.
#' @param title_x a character string which is assigned to the title of the
#'   x axis.
#' @param title_y a character string which is assigned to the title of the
#'   y axis.
#'
#' @return Returns a plotly object which contains the layout
#'   that is used for probability plotting.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' x_layout <- seq(1e-5, 1e+07, length.out = 10)
#' grid_weibull <- plot_layout(x = x_layout,
#'                             distribution = "weibull",
#'                             title_main = "Weibull Analysis",
#'                             title_x = "Time to Failure",
#'                             title_y = "Failure Probability in %")

plot_layout <- function(x, distribution = c("weibull", "lognormal", "loglogistic"),
                        title_main = "Probability Plot",
                        title_x = "Characteristic",
                        title_y = "Unreliability") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  # layout dependent on data x
  # define x-ticks of logarithm to the base of 10
  x_base <- function(xb) floor(log10(xb))
  xlog10_range <- (x_base(min(x)) - 1):x_base(max(x))

  # x-ticks and x-labels
  x_ticks <- sapply(xlog10_range, function(z) seq(10 ^ z, 10 ^ (z + 1), 10 ^ z),
                    simplify = TRUE)
  x_ticks <- round(as.numeric(x_ticks), digits = 10)
  x_ticks <- x_ticks[!duplicated(x_ticks)]
  x_labels <- x_ticks
  x_labels[c(rep(F, 3), rep(T, 6))] <- ''

  # y-ticks and y-labels
  # hard coded but it's okay since range is always between 0 and 1.
  y_s <- c(.0000001, .000001, .00001, .0001, .001, .01, .05, .1, .2, .3, .5, .6,
    .7, .8, .9, .95, .99, .999, .9999, .99999)

  if (distribution == "weibull") {
    y_ticks <- SPREDA::qsev(y_s)
  }
  if (distribution == "lognormal") {
    y_ticks <- stats::qnorm(y_s)
  }
  if (distribution == "loglogistic") {
    y_ticks <- stats::qlogis(y_s)
  }

  y_labels <- y_s * 100

  # configuration x axis
  x_config <- list(
    color = "#000000",
    title = title_x,
    titlefont = list(
      family = "Arial",
      size = 12,
      color = "#A3A3A3"),
    type = "log",
    autorange = TRUE,
    rangemode = "nonnegative",
    tickvals = x_ticks,
    ticktext = x_labels,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )

  # configuration y axis
  y_config <- list(
    color = "#000000",
    title = title_y,
    titlefont = list(
      family = "Arial",
      size = 12,
      color = "#A3A3A3"),
    autorange = TRUE,
    tickvals = y_ticks,
    ticktext = y_labels,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )

  # configuration legend
  l <- list(
    titlefont = list(
      family = "Arial",
      size = 10,
      color = "#000000")
  )

  # margins layout
  m <- list(
    l = 55,
    r = 10,
    b = 55,
    t = 25,
    pad = 4
  )


  # create grid
  p <- plotly::plotly_empty() %>%
       plotly::layout(title = title_main, titlefont = list(family = "Arial",
                      size = 16, color = "#000000"), separators = ".",
                      legend = l, xaxis = x_config, yaxis = y_config, margin = m)
  return(p)
}


#' Probability Plotting Method for Univariate Lifetime Distributions
#'
#' This function is used to apply the graphical technique of probability
#' plotting.
#'
#' The marker label for x is determined by the first word provided in the
#' argument \code{title_x}, i.e. if \code{title_x = "Mileage in km"} the x label
#' of the marker is "Mileage".
#'
#' The marker label for y is determined by the string provided in the
#' argument \code{title_y}, i.e. if \code{title_y = "Probability"} the y label
#' of the marker is "Probability".
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y a numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#' @param distribution supposed distribution of the random variable. Can be
#'   \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}. Other
#'   distributions have not been implemented yet.
#' @param title_main a character string which is assigned to the main title
#'   of the plot.
#' @param title_x a character string which is assigned to the title of the
#'   x axis.
#' @param title_y a character string which is assigned to the title of the
#'   y axis.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the layout of the probability plot
#'   provided by \code{\link{plot_layout}} and the plotting positions.
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' id <- LETTERS[1:length(obs)]
#'
#' df_john <- johnson_method(x = obs, event = state, id = id)
#'
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Weibull Analysis",
#'                           title_x = "Mileage in miles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")

plot_prob <- function(x, y, event, id = rep("XXXXXX", length(x)),
                      distribution = c("weibull", "lognormal", "loglogistic"),
                      title_main = "Probability Plot",
                      title_x = "Characteristic", title_y = "Unreliability",
                      title_trace = "Sample") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  # Filtering failed units:
  x_s <- x[event == 1]
  y_s <- y[event == 1]
  id_s <- id[event == 1]
  x_s <- x_s[order(x_s)]
  y_s <- y_s[order(x_s)]
  id_s <- id_s[order(x_s)]

  # Plot layout:
  p <- plot_layout(x = x, distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y)

  mark_x <- unlist(strsplit(title_x, " "))[1]

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(y_s)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(y_s)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(y_s)
  }

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(x = ~x_s, y = ~q, type = "scatter",
    mode = "markers", hoverinfo = "text", color = I("#3C8DBC"),
    name = title_trace,
    text = ~paste("ID:", id_s,
      paste("<br>", paste0(mark_x, ":")), x_s,
      paste("<br>", paste0(title_y, ":")), round(y_s, digits = 5))
  )
  return(plot)
}


#' Probability Plot for separated Mixture Models
#'
#' This function is used to apply the graphical technique of probability
#' plotting to univariate mixture models that where separated with the function
#' \code{\link{mixmod_regression}}. A maximum of three subgroups can be plotted.
#' The intention of this function is to give the user a hint for the existence
#' of a mixture model. An in-depth analysis should be done afterwards.
#'
#' The marker label for x is determined by the first word provided in the
#' argument \code{title_x}, i.e. if \code{title_x = "Mileage in km"} the x label
#' of the marker is "Mileage".
#'
#' The marker label for y is determined by the string provided in the
#' argument \code{title_y}, i.e. if \code{title_y = "Probability"} the y label
#' of the marker is "Probability".
#'
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param y a numeric vector which consists of estimated failure probabilities
#'   regarding the lifetime data in \code{x}.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#' @param distribution supposed distribution of the random variable. Can be
#'   \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}. Other
#'   distributions have not been implemented yet.
#' @param reg_output a list provided by \code{\link{mixmod_regression}} which
#'   consists of values necessary to visualize the segments calculated by
#'   \code{\link{mixmod_regression}}.The default value of \code{reg_output} is
#'   \code{NULL}.
#' @param title_main a character string which is assigned to the main title
#'   of the plot.
#' @param title_x a character string which is assigned to the title of the
#'   x axis.
#' @param title_y a character string which is assigned to the title of the
#'   y axis.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the layout of the probability plot
#'   provided by \code{\link{plot_layout}} and the plotting positions.
#' @export
#'
#' @examples
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' john <- johnson_method(x = hours, event = state)
#'
#' mix_mod <- mixmod_regression(x = john$characteristic,
#'                              y = john$prob,
#'                              event = john$status,
#'                              distribution = "weibull")
#'
#' plot_weibull_mix <- plot_prob_mix(x = john$characteristic,
#'                                   y = john$prob,
#'                                   event = john$status,
#'                                   id = john$id,
#'                                   distribution = "weibull",
#'                                   reg_output = mix_mod,
#'                                   title_main = "Mixture Weibull Analysis",
#'                                   title_x = "Time in Hours",
#'                                   title_y = "Probability of Failure",
#'                                   title_trace = "classification")

plot_prob_mix <- function(x, y, event, id = rep("XXXXXX", length(x)),
                          distribution = c("weibull", "lognormal", "loglogistic"),
                          reg_output = NULL,
                          title_main = "Probability Plot",
                          title_x = "Characteristic",
                          title_y = "Unreliability",
                          title_trace = "Sample") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  # Function to split x values regarding determined subgroups:
  subset_x <- function(x, mrr_model) {
    subset(x, x >= mrr_model$x_range[[1]] & x <= mrr_model$x_range[[2]])
  }

  # Filtering failed units:
  x_s <- x[event == 1]
  y_s <- y[event == 1]
  id_s <- id[event == 1]
  x_s <- x_s[order(x_s)]
  y_s <- y_s[order(x_s)]
  id_s <- id_s[order(x_s)]

  group_df <- data.frame(x_s = x_s, y_s = y_s, id_s = id_s)

  # Check for mixtures and separate data regarding results from segmented
  # regression:
  if (!is.null(reg_output)) {
    if (exists("mod_3", where = reg_output)) {
      x_1 <- subset_x(x = x_s, mrr_model = reg_output$mod_1)
      x_2 <- subset_x(x = x_s, mrr_model = reg_output$mod_2)
      x_3 <- subset_x(x = x_s, mrr_model = reg_output$mod_3)

      group_df$groups <- as.factor(c(rep("Subgroup 1", length(x_1)),
                                    rep("Subgroup 2", length(x_2)),
                                    rep("Subgroup 3", length(x_3))))
    } else if (exists("mod_2", where = reg_output)) {
      x_1 <- subset_x(x = x_s, mrr_model = reg_output$mod_1)
      x_2 <- subset_x(x = x_s, mrr_model = reg_output$mod_2)
      group_df$groups <- as.factor(c(rep("Subgroup 1", length(x_1)),
                                    rep("Subgroup 2", length(x_2))))
    } else {
      group_df$groups <- as.factor(c(rep("Subgroup 1", length(x_s))))
    }
  } else {
    group_df$groups <- as.factor(c(rep("Subgroup 1", length(x_s))))
  }

  # Plot layout:
  p <- plot_layout(x = x_s, distribution = distribution,
                   title_main = title_main,
                   title_x = title_x,
                   title_y = title_y)

  mark_x <- unlist(strsplit(title_x, " "))[1]

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(y_s)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(y_s)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(y_s)
  }
  group_df$q <- q

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(data = group_df, x = ~x_s, y = ~q, type = "scatter",
                      mode = "markers", hoverinfo = "text",
                      color = ~groups,
                      colors = c("Subgroup 1" = I("#3C8DBC"),
                        "Subgroup 2" = I("#FF0000"),
                        "Subgroup 3" = I("#008000")),
                      name = title_trace,
                      text = ~paste("ID:", id_s,
                                    paste("<br>", paste0(mark_x, ":")), x_s,
                                    paste("<br>", paste0(title_y, ":")),
                                    round(y_s, digits = 5))
  )
  return(plot)
}

#' Adding an estimated Population Line to a Probability Plot
#'
#' This function adds a regression line to an existing probability plot using a
#' model estimated by \code{\link{rank_regression}} or \code{\link{ml_estimation}}.
#'
#' @param p_obj a plotly object provided by function \code{\link{plot_prob}}.
#' @param x a numeric vector containing the x-coordinates of the regression line.
#' @param y a numeric vector containing the y-coordinates of the regression line.
#'   The default value of y is \code{NULL}. If \code{y} is set \code{NULL} the
#'   y-coordinates with respect to \code{x} are calculated by function
#'   \code{predict_prob} using estimated coefficients in \code{reg_output}.
#' @param loc_sc_params a (named) numeric vector of estimated location and scale
#'   parameters for a specified distribution. The order of elements is
#'   important. First entry needs to be the location parameter \eqn{\mu} and the
#'   second element needs to be the scale parameter \eqn{\sigma}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions and the estimated regression line.
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' id <- LETTERS[1:length(obs)]
#'
#' df_john <- johnson_method(x = obs, event = state, id = id)
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull",
#'                        conf_level = .90)
#'
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Weibull Analysis",
#'                           title_x = "Mileage in miles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull, x = obs,
#'                              loc_sc_params = mrr$loc_sc_coefficients,
#'                              distribution = "weibull",
#'                              title_trace = "Estimated Weibull CDF")

plot_mod <- function(p_obj, x, y = NULL, loc_sc_params,
                     distribution = c("weibull", "lognormal", "loglogistic"),
                     title_trace = "Fit") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  if (is.null(y)) {
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    x_low <- x_min - 10 ^ floor(log10(x_min)) * .5
    x_high <- x_max + 10 ^ floor(log10(x_max)) * .25

    x_p <- seq(x_low, x_high, length.out = 200)
    y_p <- predict_prob(q = x_p, loc_sc_params = loc_sc_params,
                        distribution = distribution)
  } else {
    x_p <- x
    y_p <- y
  }

  df_p <- data.frame(x_p = x_p, y_p = y_p)

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title, " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title, " "))[1]

  if (distribution == "weibull") {
    q <- SPREDA::qsev(y_p)
    param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
      round(1 / loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03B7:", "\u03B2:")
  }
  if (distribution == "lognormal") {
    q <- stats::qnorm(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
      round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")
  }
  if (distribution == "loglogistic") {
    q <- stats::qlogis(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
      round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")
  }

  p_mod <- plotly::add_lines(p = p_obj, data = df_p, x = ~x_p, y = ~q,
      type = "scatter", mode = "lines", hoverinfo = "text",
    color = I("#CC2222"), name = title_trace,
      text = ~paste(paste0(x_mark, ":"), round(x_p, digits = 2),
                    paste("<br>", paste0(y_mark, ":")), round(y_p, digits = 5),
                    "<br>", paste(param_label[1], param_val[1]),
                    "<br>", paste(param_label[2], param_val[2])))
  return(p_mod)
}


#' Adding estimated Population Lines of a separated Mixture Model to a
#' Probability Plot
#'
#' This function adds one or multiple estimated regression lines to an existing
#' probability plot (\code{\link{plot_prob_mix}}). Depending on the output of the
#' function \code{\link{mixmod_regression}} one or multiple lines are plotted.
#'
#' @param p_obj a plotly object provided by function \code{\link{plot_prob_mix}}.
#' @param x a numeric vector containing the x-coordinates of the regression line.
#' @param y a numeric vector containing the y-coordinates of the regression line.
#'   The default value of y is \code{NULL}. If \code{y} is set \code{NULL} the
#'   y-coordinates with respect to \code{x} are calculated by function
#'   \code{predict_prob} using estimated coefficients in \code{reg_output}.
#' @param reg_output a list provided by \code{\link{mixmod_regression}} which
#'   consists of values necessary to visualize the regression lines.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions and estimated regression line(s).
#' @export
#'
#' @examples
#' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
#'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
#'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
#'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
#'           226, 278, 314, 328, 377)
#' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
#'           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
#'           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'           0, 1, 1, 1, 1, 1, 1)
#' john <- johnson_method(x = hours, event = state)
#'
#' mix_mod <- mixmod_regression(x = john$characteristic,
#'                              y = john$prob,
#'                              event = john$status,
#'                              distribution = "weibull")
#'
#' plot_weibull_mix <- plot_prob_mix(x = john$characteristic,
#'                                   y = john$prob,
#'                                   event = john$status,
#'                                   id = john$id,
#'                                   distribution = "weibull",
#'                                   reg_output = mix_mod,
#'                                   title_main = "Mixture Weibull Analysis",
#'                                   title_x = "Time in Hours",
#'                                   title_y = "Probability of Failure",
#'                                   title_trace = "classification")
#' plot_weibull_reg_mix <- plot_mod_mix(p_obj = plot_weibull_mix, x = hours,
#'                              reg_output = mix_mod,
#'                              distribution = "weibull",
#'                              title_trace = "model")

plot_mod_mix <- function(p_obj, x, y = NULL, reg_output,
                     distribution = c("weibull", "lognormal", "loglogistic"),
                     title_trace = "Fit") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  subset_x <- function(x, mrr_model) {
    subset(x, x >= mrr_model$x_range[[1]] & x <= mrr_model$x_range[[2]])
  }

  if (exists("mod_2", where = reg_output)) {
    x_1 <- subset_x(x = x, mrr_model = reg_output$mod_1)
    x_2 <- subset_x(x = x, mrr_model = reg_output$mod_2)

    if (exists("mod_3", where = reg_output)) {
      x_3 <- subset_x(x = x, mrr_model = reg_output$mod_3)
    }
  }

  plot_mod_groups <- function(p_obj, x, y. = y, loc_sc_params, color, title_trace.) {
    if (is.null(y.)) {
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)
      x_low <- x_min - 10 ^ floor(log10(x_min)) * .5
      x_high <- x_max + 10 ^ floor(log10(x_max)) * .25

      x_p <- seq(x_low, x_high, length.out = 200)
      y_p <- predict_prob(q = x_p, loc_sc_params = loc_sc_params,
                          distribution = distribution)
    } else {
      x_p <- x
      y_p <- y.
    }

    df_p <- data.frame(x_p = x_p, y_p = y_p)

    x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,  " "))[1]
    y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,  " "))[1]

    if (distribution == "weibull") {
      q <- SPREDA::qsev(y_p)
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
        round(1 / loc_sc_params[[2]], digits = 2))
      param_label <- c("\u03B7:", "\u03B2:")
    }
    if (distribution == "lognormal") {
      q <- stats::qnorm(y_p)
      param_val <- c(round(loc_sc_params[[1]], digits = 2),
        round(loc_sc_params[[2]], digits = 2))
      param_label <- c("\u03BC:", "\u03C3:")
    }
    if (distribution == "loglogistic") {
      q <- stats::qlogis(y_p)
      param_val <- c(round(loc_sc_params[[1]], digits = 2),
        round(loc_sc_params[[2]], digits = 2))
      param_label <- c("\u03BC:", "\u03C3:")
    }

    p_mod <- plotly::add_lines(p = p_obj, data = df_p, x = ~x_p, y = ~q,
      type = "scatter", mode = "lines", hoverinfo = "text",
      line = list(color = color), name = title_trace.,
      text = ~paste(paste0(x_mark, ":"), round(x_p, digits = 2),
                    paste("<br>", paste0(y_mark, ":")), round(y_p, digits = 5),
                    "<br>", paste(param_label[1], param_val[1]),
                    "<br>", paste(param_label[2], param_val[2])))


    return(p_mod)
  }

  if (exists("mod_2", where = reg_output)) {
    plot <- p_obj %>% plot_mod_groups(x = x_1,
      loc_sc_params = reg_output$mod_1$loc_sc_coefficients,
      color = "blue", title_trace = paste(title_trace, "1", sep = " ")) %>%
      plot_mod_groups(x = x_2, loc_sc_params = reg_output$mod_2$loc_sc_coefficients,
        color = "#9a0808", title_trace. = paste(title_trace, "2", sep = " "))

    if (exists("mod_3", where = reg_output)) {
      plot <- plot_mod_groups(p_obj = plot, x = x_3,
        loc_sc_params = reg_output$mod_3$loc_sc_coefficients, color = "limegreen",
        title_trace. = paste(title_trace, "3", sep = " "))
    }
  } else {
    plot <- p_obj %>% plot_mod_groups(x = x,
      loc_sc_params = reg_output$loc_sc_coefficients, color = "blue",
      title_trace. = title_trace)
  }
  return(plot)
}


#' Add Confidence Region(s) for Quantiles or Probabilities
#'
#' This function is used to add estimated confidence region(s) to an existing
#' probability plot which also includes the estimated regression line.
#'
#' @param p_obj a plotly object provided by function \code{\link{plot_mod}}.
#' @param x a list containing the x-coordinates of the confidence region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param y a list containing the y-coordinates of the Confidence Region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param direction a character string specifying the direction of the plotted
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param title_trace a character string which is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions, the estimated regression line and the estimated
#'   confidence region(s).
#' @export
#'
#' @examples
#' obs   <- seq(10000, 100000, 10000)
#' state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#' id <- LETTERS[1:length(obs)]
#'
#' df_john <- johnson_method(x = obs, event = state, id = id)
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull",
#'                        conf_level = .95)
#' conf_betabin <- confint_betabinom(x = df_john$characteristic,
#'                                   event = df_john$status,
#'                                   loc_sc_params = mrr$loc_sc_coefficients,
#'                                   distribution = "weibull",
#'                                   bounds = "two_sided",
#'                                   conf_level = 0.95,
#'                                   direction = "y")
#'
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Weibull Analysis",
#'                           title_x = "Mileage in miles",
#'                           title_y = "Probability of Failure",
#'                           title_trace = "Failed Items")
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull,
#'                              x = conf_betabin$characteristic,
#'                              y = conf_betabin$prob,
#'                              loc_sc_params = mrr$loc_sc_coefficients,
#'                              distribution = "weibull",
#'                              title_trace = "Estimated Weibull CDF")
#' plot_conf_beta <- plot_conf(p_obj = plot_reg_weibull,
#'                             x = list(conf_betabin$characteristic),
#'                             y = list(conf_betabin$lower_bound,
#'                                      conf_betabin$upper_bound),
#'                             direction = "y",
#'                             distribution = "weibull",
#'                             title_trace = "Confidence Region")

plot_conf <- function(p_obj, x, y, direction = c("y", "x"),
                      distribution = c("weibull", "lognormal", "loglogistic"),
                      title_trace = "Confidence Limit") {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  lst <- do.call(Map, c(data.frame, list(x = x, y = y)), quote = TRUE)
  df_p <- as.data.frame(dplyr::bind_rows(lst, .id = "group"))

  df_mod <- plotly::plotly_data(p_obj)

  if (direction == "y") {
    df_p$group <- ifelse(test = df_p$y < df_mod$y_p, yes = "Lower", no = "Upper")
  } else {
    df_p$group <- ifelse(test = df_p$x < df_mod$x_p, yes = "Lower", no = "Upper")
  }

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,
                            " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,
                            " "))[1]

  if (distribution == "weibull") {
    q_1 <- SPREDA::qsev(df_p$y[df_p$group == unique(df_p$group)[1]])
  } else if (distribution == "lognormal") {
    q_1 <- stats::qnorm(df_p$y[df_p$group == unique(df_p$group)[1]])
  } else if (distribution == "loglogistic") {
    q_1 <- stats::qlogis(df_p$y[df_p$group == unique(df_p$group)[1]])
  }

  p_conf <- plotly::add_lines(
    p = p_obj, data = dplyr::filter(df_p, group == unique(df_p$group)[1]),
    x = ~x, y = ~q_1, type = "scatter", mode = "lines",
    hoverinfo = "text", line = list(dash = "dash", width = 1),
    color = I("#CC2222"), name = title_trace, legendgroup = "Interval",
    text = ~paste(paste0(x_mark, ":"), round(x, digits = 2),
                  paste("<br>", paste0(y_mark, ":")), round(y, digits = 5)))

  if (length(unique(df_p$group)) > 1) {

    if (distribution == "weibull") {
      q_2 <- SPREDA::qsev(df_p$y[df_p$group == unique(df_p$group)[2]])
    } else if (distribution == "lognormal") {
      q_2 <- stats::qnorm(df_p$y[df_p$group == unique(df_p$group)[2]])
    } else if (distribution == "loglogistic") {
      q_2 <- stats::qlogis(df_p$y[df_p$group == unique(df_p$group)[2]])
    }

    p_conf <- p_conf %>%
      plotly::add_lines(
        data = dplyr::filter(df_p, group == unique(df_p$group)[2]),
        x = ~x, y = ~q_2, type = "scatter", mode = "lines",
        hoverinfo = "text", line = list(dash = "dash", width = 1),
        color = I("#CC2222"), name = title_trace, legendgroup = "Interval",
        showlegend = FALSE,
        text = ~paste(paste0(x_mark, ":"), round(x, digits = 2),
                      paste("<br>", paste0(y_mark, ":")), round(y, digits = 5)))
  }
  return(p_conf)
}


#' Add Population line to an existing Grid
#'
#' This function adds a linearized CDF to an existing plotly grid.
#'
#' @param p_obj a plotly object, which at least includes the layout provided
#'   by \code{\link{plot_layout}}.
#' @param x a numeric vector containing the x-coordinates of the population line.
#' @param params a (named) numeric vector, where the first entry is the location
#'   parameter \eqn{\mu} and the second entry is the scale parameter \eqn{\sigma}
#'   of a lognormal or loglogistic distribution. If the distribution value is
#'   \code{"weibull"} the first entry must be the scale parameter \eqn{\eta} and
#'   the second entry must be the shape parameter \eqn{\beta}. Parametrization
#'   is the same used in \code{\link{rweibull}}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"}.
#'   Other distributions have not been implemented yet.
#' @param color the color of the population line should be added as follows:
#'   For hexadecimal codes: \code{color = I("#3C8DBC")} and for a color specified
#'   with a string: \code{color = I("blue")}.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return A plotly object which contains the supposed linearized population
#'   CDF. Failure probabilities must be strictly below 1 and for this very reason
#'
#' @export
#'
#' @examples
#' x <- rweibull(n = 100, shape = 1, scale = 20000)
#' grid_weibull <- plot_layout(x = x,
#'                             distribution = "weibull",
#'                             title_main = "Weibull Analysis",
#'                             title_x = "Time to Failure",
#'                             title_y = "Failure Probability")
#' pop_weibull <- plot_pop(p_obj = grid_weibull,
#'                         x = x, params = c(20000, 1),
#'                         distribution = "weibull")
#'
plot_pop <- function(p_obj, x, params,
                     distribution = c("weibull", "lognormal", "loglogistic"),
                     color = I("#FF0000"),
                     title_trace = "Population") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_low <- x_min - 10 ^ floor(log10(x_min)) * .5
  x_high <- x_max + 10 ^ floor(log10(x_max)) * .25

  x_s <- seq(x_low, x_high, length.out = 200)

  if (distribution == "weibull") {
    loc <- log(params[1])
    sc <- 1 / params[2]
  }
  if (distribution == "lognormal" | distribution == "loglogistic") {
    loc <- params[1]
    sc <- params[2]
  }

  y_s <- predict_prob(q = x_s, loc_sc_params = c(loc, sc),
    distribution = distribution)

  y_s <- y_s[y_s < 1]
  x_s <- x_s[y_s < 1]

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,
    " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,
    " "))[1]

  if (distribution == "weibull") {
    q <- SPREDA::qsev(y_s)
    param_val <- c(round(exp(loc), digits = 2),
      round(1 / sc, digits = 2))
    param_label <- c("\u03B7:", "\u03B2:")
  }
  if (distribution == "lognormal") {
    q <- stats::qnorm(y_s)
    param_val <- c(round(loc, digits = 2),
      round(sc, digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")
  }
  if (distribution == "loglogistic") {
    q <- stats::qlogis(y_s)
    param_val <- c(round(loc, digits = 2),
      round(sc, digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")
  }

  p_pop <- plotly::add_lines(p = p_obj, x = ~x_s, y = ~q,
    type = "scatter", mode = "lines", hoverinfo = "text", color = color,
    name = title_trace, line = list(width = 1),
    text = ~paste(paste0(x_mark, ":"), round(x_s, digits = 2),
      paste("<br>", paste0(y_mark, ":")), round(y_s, digits = 5),
      "<br>", paste(param_label[1], param_val[1]),
      "<br>", paste(param_label[2], param_val[2])))

  return(p_pop)
}
