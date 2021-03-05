#' @export
plot_layout_vis.plotly <- function(
  p_obj,
  x,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  layout_helper <- plot_layout_helper(x, distribution, "plotly")

  ## Type of x axis:
  x_axis_type <- if (distribution %in% c("sev", "normal", "logistic")) "-" else "log"

  ## Configuration x axis:
  x_config <- list(
    title = list(
      text = title_x
    ),
    type = x_axis_type,
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

  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x_config <- c(
      x_config,
      list(
        tickvals = layout_helper$x_ticks,
        ticktext = layout_helper$x_labels
      )
    )
  }

  ## Configuration y axis:
  y_config <- list(
    title = list(
      text = title_y
    ),
    autorange = TRUE,
    tickvals = layout_helper$y_ticks,
    ticktext = layout_helper$y_labels,
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
  p_obj <- p_obj %>%
    plotly::layout(
      title = title,
      separators = ".",
      legend = l,
      xaxis = x_config,
      yaxis = y_config,
      margin = m
    )
  return(p_obj)
}

#' @export
plot_prob_vis.plotly <- function(
  p_obj, tbl_prob,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Suppress warning by subsetting with character
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

  return(p_prob)
}

#' @export
plot_mod_vis.plotly <- function(
  p_obj, tbl_mod, title_trace = "Fit"
) {

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text, " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text, " "))[1]

  n_method <- length(unique(tbl_mod$cdf_estimation_method))
  n_group <- length(unique(tbl_mod$group))

  color <- if (n_method == 1) I("#CC2222") else ~cdf_estimation_method

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

  return(p_mod)
}

#' @export
plot_conf_vis.plotly <- function(p_obj, tbl_p, title_trace) {
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

  return(p_conf)
}

#' @export
plot_pop_vis.plotly <- function(
  p_obj, tbl_pop, title_trace
) {
  # Get axis labels in hover
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  # Hovertext and name
  tbl_pop <- tbl_pop %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hovertext = to_hovertext(
        .data$x_s, .data$y_s, .data$param_val, .data$param_label, x_mark, y_mark
      ),
      name = to_name_pop(
        .data$param_val, .data$param_label
      )
    ) %>%
    dplyr::ungroup()

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

  return(p_pop)
}



mod_hovertext <- function(x, y, param_val, param_label, x_mark, y_mark) {
  param_val <- unlist(param_val)
  param_label <- unlist(param_label)

  text <- paste(
    paste0(x_mark, ":"),
    format(x, digits = 3),
    paste("<br>", paste0(y_mark, ":")),
    format(y, digits = 6),
    "<br>",
    paste(param_label[1], param_val[1]),
    "<br>",
    paste(param_label[2], param_val[2])
  )

  if (!is.na(param_val[3])) {
    text <- paste(
      text,
      "<br>",
      paste(param_label[3], param_val[3])
    )
  }
  text
}

to_name_pop <- function(param_val, param_label) {
  param_val <- unlist(param_val)
  param_label <- unlist(param_label)

  text <- paste0(
    param_label[1], " ",
    param_val[1], ", ",
    param_label[2], " ",
    param_val[2]
  )

  if (!is.na(param_val[3])) {
    text <- paste0(
      text, ", ",
      param_label[3], " ",
      param_val[3]
    )
  }

  text
}

to_name <- function(tbl, n_method, n_group, title_trace) {
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
