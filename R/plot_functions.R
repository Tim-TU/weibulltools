#' Layout of the Probability Plot
#'
#' This function is used to create the layout of a probability plot.
#'
#' @param x a numeric vector which consists of lifetime data. \code{x} is used to
#'   specify the grid of the plot.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"} or \code{"sev"} \emph{(smallest extreme value)}.
#'   Other distributions have not been implemented yet.
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

plot_layout <- function(x,
                        distribution = c("weibull", "lognormal", "loglogistic",
                                         "normal", "logistic", "sev"),
                        title_main = "Probability Plot",
                        title_x = "Characteristic",
                        title_y = "Unreliability") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev"))) {
    stop("No valid distribution!")
  }


  # Define x-ticks of logarithm to the base of 10 for Log-Location-Scale Distributions:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {

    # Layout dependent on data x, function to build helpful sequences:
    x_base <- function(xb) floor(log10(xb))
    xlog10_range <- (x_base(min(x)) - 1):x_base(max(x))
    # x-ticks and x-labels
    x_ticks <- sapply(xlog10_range, function(z) seq(10 ^ z, 10 ^ (z + 1), 10 ^ z),
      simplify = TRUE)
    x_ticks <- round(as.numeric(x_ticks), digits = 10)
    x_ticks <- x_ticks[!duplicated(x_ticks)]
    x_labels <- x_ticks
    x_labels[c(rep(F, 3), rep(T, 6))] <- ''
  }

  # y-ticks and y-labels
  # hard coded but it's okay since range is always between 0 and 1.
  y_s <- c(.0000001, .000001, .00001, .0001, .001, .01, .05, .1, .2, .3, .5, .6,
    .7, .8, .9, .95, .99, .999, .9999, .99999)

  if (distribution %in% c("weibull", "sev")) {
    y_ticks <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    y_ticks <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    y_ticks <- stats::qlogis(y_s)
  }

  y_labels <- y_s * 100

  # Plot:
  ## Type of x axis:
  xaxs_type <- ifelse(test = distribution %in% c("sev", "normal", "logistic"),
                      yes = "-",
                      no = "log")

  ## Configuration x axis:
  x_config <- list(
    color = "#000000",
    title = list(
      text = title_x,
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    type = xaxs_type,
    autorange = TRUE,
    rangemode = "nonnegative",
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    #tickmode = "array",
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )

  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    #x_config <- c(x_config, list(tickvals = x_ticks,ticktext = x_labels))
    x_config <- c(x_config, list(tickvals = x_ticks,ticktext = x_ticks))

  }


  ## Configuration y axis:
  y_config <- list(
    color = "#000000",
    title = list(
      text = title_y,
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
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
    title = list(
      font = list(
        family = "Arial",
        size = 10,
        color = "#000000"
      )
    )
  )

  # margins layout
  m <- list(
    l = 55,
    r = 10,
    b = 55,
    t = 25,
    pad = 4
  )

  title <- list(
    text = title_main,
    font = list(
      family = "Arial",
      size = 16,
      color = "#000000"
    )
  )


  # create grid
  p <- plotly::plotly_empty() %>%
       plotly::layout(title = title, separators = ".",
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
#' argument \code{title_y}, i.e. if \code{title_y = "Probability in percent"} the y
#' label of the marker is "Probability".
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
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
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"} or \code{"sev"} \emph{(smallest extreme value)}.
#'   Other distributions have not been implemented yet.
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
#' # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
#' cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
#'               224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
#'               180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
#'               159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
#'               139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
#'               112, 108, 104, 99, 99, 96, 94)
#' state <- c(rep(0, 5), rep(1, 67))
#'
#' df_john <- johnson_method(x = cycles, event = state)
#'
#' # Example 1: Probability Plot Weibull:
#' plot_weibull <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "weibull",
#'                           title_main = "Weibull Analysis",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")
#'
#' # Example 2: Probability Plot Lognormal:
#' plot_lognormal <- plot_prob(x = df_john$characteristic,
#'                           y = df_john$prob,
#'                           event = df_john$status,
#'                           id = df_john$id,
#'                           distribution = "lognormal",
#'                           title_main = "Lognormal Analysis",
#'                           title_x = "Cycles",
#'                           title_y = "Probability of Failure in %",
#'                           title_trace = "Failed Items")

plot_prob <- function(x, y, event, id = rep("XXXXXX", length(x)),
                      distribution = c("weibull", "lognormal", "loglogistic",
                                       "normal", "logistic", "sev"),
                      title_main = "Probability Plot",
                      title_x = "Characteristic", title_y = "Unreliability",
                      title_trace = "Sample") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev"))) {
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
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Choice of distribution:
  if (distribution %in% c("weibull", "sev")) {
    q <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    q <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    q <- stats::qlogis(y_s)
  }

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(x = ~x_s, y = ~q, type = "scatter",
    mode = "markers", hoverinfo = "text", color = I("#3C8DBC"),
    name = title_trace,
    text = ~paste("ID:", id_s,
      paste("<br>", paste0(mark_x, ":")), x_s,
      paste("<br>", paste0(mark_y, ":")), round(y_s, digits = 5))
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(plot)
}


#' Probability Plot for Separated Mixture Models
#'
#' This function is used to apply the graphical technique of probability
#' plotting to univariate mixture models that where separated with functions
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
#' @param x a numeric vector which consists of lifetime data. Lifetime
#'   data could be every characteristic influencing the reliability of a product,
#'   e.g. operating time (days/months in service), mileage (km, miles), load
#'   cycles.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param id a character vector for the identification of every unit.
#' @param distribution supposed distribution of the random variable. For output
#'   provided by \code{mixmod_em} distribution must be \code{"weibull"}. Can be
#'   \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"} for output provided
#'   \code{mixmod_regression}. Other distributions have not been implemented yet.
#' @param mix_output a list provided by \code{\link{mixmod_regression}} or
#'   \code{\link{mixmod_em}}, which consists of values necessary to visualize the
#'   subgroups.The default value of \code{mix_output} is \code{NULL}.
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

plot_prob_mix <- function(x, event, id = rep("XXXXXX", length(x)),
                          distribution = c("weibull", "lognormal", "loglogistic"),
                          mix_output = NULL,
                          title_main = "Probability Plot",
                          title_x = "Characteristic",
                          title_y = "Unreliability",
                          title_trace = "Sample") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  if (("em_results" %in%  names(mix_output)) & distribution != "weibull") {
    stop("No valid distribution! Use weibull to visualize EM results")
  }

  # check if mix_output is NULL or "em_results" is not a name of lists in mix_output!
  if (is.null(mix_output) | !("em_results" %in%  names(mix_output))) {

    # applying johnson_method() to all data! Used for printing results of
    # mixmod_regression() and for the case is mix_output = NULL.
    john_df <- johnson_method(x = x, event = event, id = id) %>%
      dplyr::filter(status == 1)

    x_s <- john_df$characteristic
    y_s <- john_df$prob
    id_s <- john_df$id
    group_df <- data.frame(x_s = x_s, y_s = y_s, id_s = id_s)
    group_df$groups <- as.factor(c(rep(title_trace, length(x_s))))
  }

    # Check for mixtures and separate data regarding results from segmented
    # regression:
    if (!is.null(mix_output) & !("em_results" %in%  names(mix_output))) {

       # Defining subset function for x_ranges provided by mixmod_regression():
      subset_x <- function(x, mrr_model) {
        subset(x, x >= mrr_model$x_range[[1]] & x <= mrr_model$x_range[[2]])
      }

      if (exists("mod_3", where = mix_output)) {
        x_1 <- subset_x(x = x_s, mrr_model = mix_output$mod_1)
        x_2 <- subset_x(x = x_s, mrr_model = mix_output$mod_2)
        x_3 <- subset_x(x = x_s, mrr_model = mix_output$mod_3)

        group_df$groups <- as.factor(c(rep(paste(title_trace, 1), length(x_1)),
          rep(paste(title_trace, 2), length(x_2)),
          rep(paste(title_trace, 3), length(x_3))))
      } else if (exists("mod_2", where = mix_output)) {
        x_1 <- subset_x(x = x_s, mrr_model = mix_output$mod_1)
        x_2 <- subset_x(x = x_s, mrr_model = mix_output$mod_2)
        group_df$groups <- as.factor(c(rep(paste(title_trace, 1), length(x_1)),
          rep(paste(title_trace, 2), length(x_2))))
      } else {
        group_df$groups <- as.factor(c(rep(title_trace, length(x_s))))
      }
    }

  if (("em_results" %in%  names(mix_output))) {
    # Split observations by maximum a-posteriori method (MAP) used in mixmod_em:
    groups <- mix_output$em_results$groups
    x_split <- split(x, groups, lex.order = T)
    ev_split <- split(event, groups, lex.order = T)
    id_split <- split(id, groups, lex.order = T)

    # Apply johnson_method() for splitted observations:
    john_lst <- mapply(x_split, ev_split, id_split,
                       FUN = function(x, d, id) johnson_method(x = x, event = d, id = id),
                       SIMPLIFY = FALSE)

    # Store dataframes in one dataframe:
    group_df <- do.call("rbind", john_lst)

    # add group names using row.names() if splitted groups exist. Otherwise use
    # title_trace:

    if (length(john_lst) == 1) {
      group_df$groups <- as.factor(title_trace)
    } else {
      group_df$groups <- as.factor(paste(title_trace,
        floor(as.numeric(row.names(group_df)))))
    }

    # Preparation for plot:
    group_df <- group_df %>%
      dplyr::filter(status == 1) %>%
      dplyr::rename(id_s = id, x_s = characteristic, y_s = prob)
  }

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(group_df$y_s)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(group_df$y_s)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(group_df$y_s)
  }
  group_df$q <- q

  # Plot layout:
  p <- plot_layout(x = group_df$x_s, distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y)

  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Defining colors (max 5 subgroups):
  cols <- c(I("#3C8DBC"), I("#FF0000"), I("#008000"), I("#ffa500"), I("#000000"))
  cols <- cols[seq_along(unique(group_df$groups))]

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(data = group_df, x = ~x_s, y = ~q, type = "scatter",
                      mode = "markers", hoverinfo = "text",
                      color = ~groups,
                      colors = cols,
                      text = ~paste("ID:", id_s,
                                    paste("<br>", paste0(mark_x, ":")), x_s,
                                    paste("<br>", paste0(mark_y, ":")),
                                    round(y_s, digits = 5))) %>%
    plotly::layout(showlegend = TRUE)
  return(plot)
}

#' Adding an Estimated Population Line to a Probability Plot
#'
#' This function adds a regression line to an existing probability plot using a
#' model estimated by \code{\link{rank_regression}} or \code{\link{ml_estimation}}.
#'
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param p_obj a plotly object provided by function \code{\link{plot_prob}}.
#' @param x a numeric vector containing the x-coordinates of the regression line.
#' @param y a numeric vector containing the y-coordinates of the regression line.
#'   The default value of y is \code{NULL}. If \code{y} is set \code{NULL} the
#'   y-coordinates with respect to \code{x} are calculated by function
#'   \code{predict_prob} using estimated coefficients in \code{loc_sc_params}. If
#'   confidence interval(s) should be added to the plot y should not be set to
#'   \code{NULL}. For more information see \strong{Details} in \code{\link{plot_conf}}.
#' @param loc_sc_params a (named) numeric vector of estimated location
#'   and scale parameters for a specified distribution. The order of
#'   elements is important. First entry needs to be the location
#'   parameter \eqn{\mu} and the second element needs to be the scale
#'   parameter \eqn{\sigma}. If a three-parametric model is used the third element
#'   is the threshold parameter \eqn{\gamma}.
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
#'   Other distributions have not been implemented yet.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
#'
#' @return Returns a plotly object containing the probability plot with
#'   plotting positions and the estimated regression line.
#' @export
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
#'
#' # Example 1: Probability Plot and Regression Line Three-Parameter-Weibull:
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
#' mrr <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "weibull3",
#'                        conf_level = .90)
#'
#' plot_reg_weibull <- plot_mod(p_obj = plot_weibull, x = cycles,
#'                              loc_sc_params = mrr$loc_sc_coefficients,
#'                              distribution = "weibull3",
#'                              title_trace = "Estimated Weibull CDF")
#'
#'
#'
#' # Example 2: Probability Plot and Regression Line Three-Parameter-Lognormal:
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
#' mrr_ln <- rank_regression(x = df_john$characteristic,
#'                        y = df_john$prob,
#'                        event = df_john$status,
#'                        distribution = "lognormal3",
#'                        conf_level = .90)
#'
#' plot_reg_lognormal <- plot_mod(p_obj = plot_lognormal, x = cycles,
#'                              loc_sc_params = mrr_ln$loc_sc_coefficients,
#'                              distribution = "lognormal3",
#'                              title_trace = "Estimated Lognormal CDF")

plot_mod <- function(p_obj, x, y = NULL, loc_sc_params,
                     distribution = c("weibull", "lognormal", "loglogistic",
                                      "normal", "logistic", "sev", "weibull3",
                                      "lognormal3", "loglogistic3"),
                     title_trace = "Fit") {

  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }


  if (is.null(y)) {
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    x_low <- x_min - 10 ^ floor(log10(x_min)) * .25
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

  if (distribution %in% c("weibull", "weibull3", "sev")) {
    q <- SPREDA::qsev(y_p)

    param_val <- c(round(loc_sc_params[[1]], digits = 2),
      round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "weibull") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
        round(1 / loc_sc_params[[2]], digits = 2))
      param_label <- c("\u03B7:", "\u03B2:")
    }
    if (distribution == "weibull3") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
        round(1 / loc_sc_params[[2]], digits = 2), round(loc_sc_params[[3]], digits = 2))
      param_label <- c("\u03B7:", "\u03B2:", "\u03B3:")
    }

  }
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    q <- stats::qnorm(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
      round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "lognormal3") {
      param_val <- c(param_val, round(loc_sc_params[[3]], digits = 2))
      param_label <- c(param_label, "\u03B3:")
    }
  }
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    q <- stats::qlogis(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
      round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "loglogistic3") {
      param_val <- c(param_val, round(loc_sc_params[[3]], digits = 2))
      param_label <- c(param_label, "\u03B3:")
    }
  }

  # Defining hovertext regarding amount of parameters:
  hovertext <- paste(paste0(x_mark, ":"), round(x_p, digits = 2),
    paste("<br>", paste0(y_mark, ":")), round(y_p, digits = 5),
    "<br>", paste(param_label[1], param_val[1]),
    "<br>", paste(param_label[2], param_val[2]))

  if (length(loc_sc_params) == 3) {
    hovertext <- paste(hovertext,
      "<br>", paste(param_label[3], param_val[3]))
  }

  p_mod <- plotly::add_lines(p = p_obj, data = df_p, x = ~x_p, y = ~q,
      type = "scatter", mode = "lines", hoverinfo = "text",
    color = I("#CC2222"), name = title_trace,
      text = ~hovertext)
  return(p_mod)
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
#' @param p_obj a plotly object provided by function \code{\link{plot_prob_mix}}.
#' @param x a numeric vector containing the x-coordinates of the regression line.
#' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
#'   is a right censored observation (= 0) or a failure (= 1).
#' @param mix_output a list provided by \code{\link{mixmod_regression}} or
#'   \code{\link{mixmod_em}}, which consists of elements necessary to visualize
#'   the regression lines.
#' @param distribution supposed distribution of the random variable. For output
#'   provided by \code{mixmod_em} distribution must be \code{"weibull"}. Can be
#'   \code{"weibull"}, \code{"lognormal"} or \code{"loglogistic"} for output provided
#'   \code{mixmod_regression}. Other distributions have not been implemented yet.
#' @param title_trace a character string whis is assigned to the trace shown in
#'   the legend.
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

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic"))) {
    stop("No valid distribution!")
  }

  if (("em_results" %in%  names(mix_output)) & distribution != "weibull") {
    stop("No valid distribution! Use weibull to visualize EM results")
  }

  # Case where mixmod_regression() was used in mix_output!
  if (!("em_results" %in%  names(mix_output))) {

    # Defining subset function for x_ranges provided by mixmod_regression():
    subset_x <- function(x, mod) {
      subset(x, x >= mod$x_range[[1]] & x <= mod$x_range[[2]])
    }

    # Defining function that calculates probabilities and store results in df.
    compute_line <- function(x, mod, distribution) {
      x_split <- subset_x(x = x, mod = mod)

      x_min <- min(x_split, na.rm = TRUE)
      x_max <- max(x_split, na.rm = TRUE)
      x_low <- x_min - 10 ^ floor(log10(x_min)) * .25
      x_high <- x_max + 10 ^ floor(log10(x_max)) * .25

      x_p <- seq(x_low, x_high, length.out = 200)
      y_p <- predict_prob(q = x_p, loc_sc_params = mod$loc_sc_coefficients,
        distribution = distribution)

      # Prepare hovertexts for regression lines:
      if (distribution == "weibull") {
        param_1 <- round(mod$coefficients[[1]], digits = 2)
        param_2 <- round(mod$coefficients[[2]], digits = 2)
        label_1 <- "\u03B7:"
        label_2 <- "\u03B2:"
      } else {
        param_1 <- round(mod$loc_sc_coefficients[[1]], digits = 2)
        param_2 <- round(mod$loc_sc_coefficients[[2]], digits = 2)
        label_1 <- "\u03BC:"
        label_2 <- "\u03C3:"
      }

      df_p <- data.frame(x_p = x_p, y_p = y_p, par_1 = param_1, par_2 = param_2,
        lab_1 = label_1, lab_2 = label_2)
    }
    lines_split <- lapply(mix_output, compute_line, x = x,
      distribution = distribution)
  }

  # case where mixmod_regression() was used in mix_output!
  if ("em_results" %in%  names(mix_output)) {

    # Split observations by maximum a-posteriori method (MAP) used in mixmod_em:
    groups <- mix_output$em_results$groups
    x_split <- split(x, groups, lex.order = T)
    ev_split <- split(event, groups, lex.order = T)

    # Apply predict_prob() for splitted observations (which failed) and parameters.
    lines_split <- mapply(x_split, ev_split, mix_output[-length(mix_output)],
      FUN = function(x, d, mod) {
        x_min <- min(x[d == 1], na.rm = TRUE)
        x_max <- max(x[d == 1], na.rm = TRUE)
        x_low <- x_min - 10 ^ floor(log10(x_min)) * .25
        x_high <- x_max + 10 ^ floor(log10(x_max)) * .25
        x_p <- seq(x_low, x_high, length.out = 200)

        # Prepare hovertexts for regression lines:
        if (distribution == "weibull") {
          param_1 <- round(mod$coefficients[[1]], digits = 2)
          param_2 <- round(mod$coefficients[[2]], digits = 2)
          label_1 <- "\u03B7:"
          label_2 <- "\u03B2:"
        } else {
          param_1 <- round(mod$loc_sc_coefficients[[1]], digits = 2)
          param_2 <- round(mod$loc_sc_coefficients[[2]], digits = 2)
          label_1 <- "\u03BC:"
          label_2 <- "\u03C3:"
        }

        data.frame(x_p = x_p, y_p = predict_prob(q = x_p,
          loc_sc_params = mod$loc_sc_coefficients,
          distribution = distribution), par_1 = param_1, par_2 = param_2,
          lab_1 = label_1, lab_2 = label_2)
      }, SIMPLIFY = FALSE)
  }

  # Bind stored dataframes in one dataframe:
  group_df <- do.call("rbind", lines_split)

  # Add group names using row.names() if splitted groups exist. Otherwise use
  # title_trace:
  if (length(lines_split) == 1) {
    group_df$groups <- as.factor(title_trace)
  }

  if (!("em_results" %in%  names(mix_output)) & length(lines_split) > 1) {
    group_df$groups <- as.factor(paste(title_trace,
      floor(as.numeric(gsub(row.names(group_df), pattern = "mod_",
        replacement = "")))))
  }

  if (("em_results" %in%  names(mix_output)) & length(lines_split) > 1) {
    group_df$groups <- as.factor(paste(title_trace,
      floor(as.numeric(row.names(group_df)))))
  }

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(group_df$y_p)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(group_df$y_p)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(group_df$y_p)
  }
  group_df$q <- q

  # Defining colors (max 5 subgroups):
  cols <- c(I("blue"), I("#9a0808"), I("#006400"), I("orange"), I("grey"))
  cols <- cols[seq_along(unique(group_df$groups))]

  # Add color to grouped data.frame to be in line with line colors:
  group_df$cols <- rep(cols, each = 200)

  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,  " "))[1]

  p_mod <- p_obj %>% plotly::add_lines(data = group_df %>% dplyr::group_by(groups),
    x = ~x_p, y = ~q, type = "scatter",
    mode = "lines", line = list(color = ~cols), name = ~groups, hoverinfo = "text",
    text = ~paste(paste0(x_mark, ":"), round(x_p, digits = 2),
      paste("<br>", paste0(y_mark, ":")), round(y_p, digits = 5),
      "<br>", paste(lab_1, par_1),
      "<br>", paste(lab_2, par_2)))
  return(p_mod)
}


#' Add Confidence Region(s) for Quantiles or Probabilities
#'
#' This function is used to add estimated confidence region(s) to an existing
#' probability plot which also includes the estimated regression line.
#'
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
#' @encoding UTF-8
#' @references Meeker, William Q; Escobar, Luis A., Statistical methods for
#'   reliability data, New York: Wiley series in probability and statistics, 1998
#'
#' @param p_obj a plotly object provided by function \code{\link{plot_mod}}.
#' @param x a list containing the x-coordinates of the confidence region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param y a list containing the y-coordinates of the Confidence Region(s).
#'   The list can be of length 1 or 2. For more information see \strong{Details}.
#' @param direction a character string specifying the direction of the plotted
#'   interval(s). Must be either "y" (failure probabilities) or "x" (quantiles).
#' @param distribution supposed distribution of the random variable. The
#'   value can be \code{"weibull"}, \code{"lognormal"}, \code{"loglogistic"},
#'   \code{"normal"}, \code{"logistic"}, \code{"sev"} \emph{(smallest extreme value)},
#'   \code{"weibull3"}, \code{"lognormal3"} or \code{"loglogistic3"}.
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
#'                                   loc_sc_params = mrr$loc_sc_coefficients,
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
#'                              loc_sc_params = mrr$loc_sc_coefficients,
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
#'                                   loc_sc_params = mrr_ln$loc_sc_coefficients,
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
#'                              loc_sc_params = mrr_ln$loc_sc_coefficients,
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

plot_conf <- function(p_obj, x, y, direction = c("y", "x"),
                      distribution = c("weibull", "lognormal", "loglogistic",
                                       "normal", "logistic", "sev", "weibull3",
                                       "lognormal3", "loglogistic3"),
                      title_trace = "Confidence Limit") {

  direction <- match.arg(direction)
  distribution <- match.arg(distribution)

  if (!(distribution %in% c("weibull", "lognormal", "loglogistic", "normal",
                            "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
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

  if (distribution %in% c("weibull", "weibull3", "sev")) {
    q_1 <- SPREDA::qsev(df_p$y[df_p$group == unique(df_p$group)[1]])
  }
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    q_1 <- stats::qnorm(df_p$y[df_p$group == unique(df_p$group)[1]])
  }
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
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

    if (distribution %in% c("weibull", "weibull3", "sev")) {
      q_2 <- SPREDA::qsev(df_p$y[df_p$group == unique(df_p$group)[2]])
    }
    if (distribution %in% c("lognormal", "lognormal3", "normal")) {
      q_2 <- stats::qnorm(df_p$y[df_p$group == unique(df_p$group)[2]])
    }
    if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
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


#' Add Population Line to an Existing Grid
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
#' @param title_trace a character string which is assigned to the trace shown in
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
#'                         distribution = "weibull", color = I("green"),
#'                         title_trace = "Population")
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
      "<br>", paste(param_label[2], param_val[2]))) %>%
    plotly::layout(showlegend = TRUE)

  return(p_pop)
}
