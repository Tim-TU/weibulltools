#' @export
plot_layout_vis.plotly <- function(p_obj, # An empty plotly object.
                                   x, # Named list with x_ticks and x_labels.
                                   y, # Named list with y_ticks and y_labels.
                                   distribution = c(
                                     "weibull", "lognormal", "loglogistic",
                                     "sev", "normal", "logistic",
                                     "exponential"
                                   ),
                                   title_main = "Probability Plot",
                                   title_x = "Characteristic",
                                   title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  # Configuration of x axis:
  x_config <- list(
    title = list(
      text = title_x
    ),
    autorange = TRUE,
    rangemode = "nonnegative",
    ticks = "inside",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10),
    #tickmode = "array",
    tickangle = 90,
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )

  # Distributions that need a log transformed x axis:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x_config <- c(
      x_config,
      list(
        type = "log",
        tickvals = x$x_ticks,
        ticktext = x$x_labels
      )
    )
  }

  ## Configuration y axis:
  y_config <- list(
    title = list(
      text = title_y
    ),
    autorange = TRUE,
    tickvals = y$y_ticks,
    ticktext = y$y_labels,
    ticks = "inside",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )

  # Configuration of legend:
  l <- list(
    title = list(
      font = list(
        family = "Arial",
        size = 10,
        color = "#000000"
      )
    )
  )

  # Layout margins:
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


  # Create grid:
  p_obj <- p_obj %>%
    plotly::layout(
      title = title,
      separators = ".",
      legend = l,
      xaxis = x_config,
      yaxis = y_config,
      margin = m
    )

  p_obj
}



#' @export
plot_prob_vis.plotly <- function(p_obj,
                                 tbl_prob,
                                 distribution = c(
                                   "weibull", "lognormal", "loglogistic",
                                   "sev", "normal", "logistic",
                                   "exponential"
                                 ),
                                 title_main = "Probability Plot",
                                 title_x = "Characteristic",
                                 title_y = "Unreliability",
                                 title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Suppress warning by subsetting with character:
  n_group <- length(unique(tbl_prob[["group"]]))
  n_method <- length(unique(tbl_prob$cdf_estimation_method))

  color <- if (n_method == 1) I("#3C8DBC") else ~cdf_estimation_method
  symbol <- if (n_group == 0) NULL else ~group

  name <- to_name(tbl_prob, n_method, n_group, title_trace)

  # Construct probability plot:
  p_prob <- p_obj %>%
    plotly::add_trace(
      data = tbl_prob,
      x = ~x,
      y = ~q,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      name = name,
      color = color,
      colors = "Set2",
      symbol = symbol,
      legendgroup = ~cdf_estimation_method,
      text = paste(
        "ID:", tbl_prob$id,
        paste("<br>", paste0(mark_x, ":")), format(tbl_prob$x, digits = 3),
        paste("<br>", paste0(mark_y, ":")), format(tbl_prob$prob, digits = 6)
      )
    ) %>%
    plotly::layout(showlegend = TRUE)

  p_prob
}



#' @export
plot_mod_vis.plotly <- function(p_obj,
                                tbl_mod,
                                title_trace = "Fit"
) {

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text, " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text, " "))[1]

  n_method <- length(unique(tbl_mod$cdf_estimation_method))
  n_group <- length(unique(tbl_mod$group))

  color <- if (n_method == 1) I("#CC2222") else ~cdf_estimation_method

  ## Creation of hovertext
  arg_list <- list(
    x = tbl_mod$x_p,
    y = tbl_mod$y_p,
    param_val = tbl_mod$param_val,
    param_label = tbl_mod$param_label
  )

  # tbl_mod has names lower / upper if set in plot_conf()
  if (hasName(tbl_mod, "lower")) {
    arg_list$lower <- tbl_mod$lower
  }

  if (hasName(tbl_mod, "upper")) {
    arg_list$upper <- tbl_mod$upper
  }

  tbl_mod <- tbl_mod %>%
    dplyr::mutate(
      hovertext = purrr::pmap_chr(
        arg_list,
        hovertext_mod,
        x_mark = x_mark,
        y_mark = y_mark
      )
    )

  # Reminder: Splitting the line by group happens by using the name
  name <- to_name(tbl_mod, n_method, n_group, title_trace)

  p_mod <- plotly::add_lines(
    p = p_obj,
    data = tbl_mod,
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    name = name,
    color = color,
    colors = "Set2",
    legendgroup = ~cdf_estimation_method,
    text = ~hovertext
  )

  p_mod
}



#' @export
plot_conf_vis.plotly <- function(p_obj,
                                 tbl_p,
                                 title_trace
) {

  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  n_method <- length(unique(tbl_p$cdf_estimation_method))

  color <- if (n_method == 1) I("#CC2222") else ~cdf_estimation_method

  name <- to_name(tbl_p, n_method, n_group = 0, title_trace)

  p_conf <- plotly::add_lines(
    p = p_obj,
    # tbl_p is grouped by bound. Therefore two separate lines are drawn
    # for two-sided confidence intervals
    data = tbl_p,
    x = ~x, y = ~q,
    type = "scatter", mode = "lines",
    # hoverinfo text is set in plot_mod
    hoverinfo = "skip",
    line = list(dash = "dash", width = 1),
    color = color,
    colors = "Set2",
    name = name,
    legendgroup = ~cdf_estimation_method
  )

  p_conf
}



#' @export
plot_pop_vis.plotly <- function(p_obj,
                                tbl_pop,
                                title_trace
) {

  # Get axis labels in hover
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  # Hovertext and name
  tbl_pop <- tbl_pop %>%
    dplyr::mutate(
      hovertext = purrr::pmap_chr(
        list(
          x = .data$x_s,
          y = .data$y_s,
          param_val = .data$param_val,
          param_label = .data$param_label
        ),
        hovertext_mod,
        x_mark = x_mark,
        y_mark = y_mark
      ),
      name = purrr::map2_chr(.data$param_val, .data$param_label, to_name_pop)
    )

  p_pop <- plotly::add_lines(
    p = p_obj, data = tbl_pop,
    x = ~x_s, y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    # color = ~group,
    colors = "Set2",
    name = ~name,
    line = list(width = 1),
    text = ~hovertext
  ) %>%
    plotly::layout(showlegend = TRUE)

  p_pop
}



# Hover text for plot_mod() and plot_conf():
hovertext_mod <- function(x,
                          y,
                          param_val,
                          param_label,
                          x_mark,
                          y_mark,
                          lower = NULL,
                          upper = NULL
) {

  not_na <- !is.na(param_val)

  x_text <- paste0(x_mark, ": ", format(x, digits = 3))
  y_text <- paste0(y_mark, ": ", format(y, digits = 3))

  lower_text <- if (!is.null(lower))
    paste("Lower Bound:", format(lower, digits = 3))
  upper_text <- if (!is.null(upper))
    paste("Upper Bound:", format(upper, digits = 3))

  param_text <- paste(param_label[not_na], param_val[not_na], collapse = ", ")

  paste(
    x_text,
    y_text,
    lower_text,
    upper_text,
    param_text,
    sep = "<br>"
  )
}



# Trace name for plot_pop():
to_name_pop <- function(param_val,
                        param_label
) {

  not_na <- !is.na(param_val)
  paste(param_label[not_na], param_val[not_na], collapse = ", ")
}



# Trace name for plot_prob(), plot_mod() and plot_conf():
to_name <- function(tbl,
                    n_method,
                    n_group,
                    title_trace
) {

  if (n_method <= 1) {
    if (n_group <= 1) {
      title_trace
    } else {
      paste0(title_trace, ": ", tbl$group)
    }
  } else {
    if (n_group <= 1) {
      paste0(title_trace, ": ", tbl$cdf_estimation_method)
    } else {
      paste0(title_trace, ": ", tbl$cdf_estimation_method, ", ", tbl$group)
    }
  }
}
