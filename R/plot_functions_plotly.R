plot_layout_plotly <- function(
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
    color = "#000000",
    title = list(
      text = title_x,
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    type = x_axis_type,
    autorange = TRUE,
    rangemode = "nonnegative",
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    tickmode = "array",
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
    tickvals = layout_helper$y_ticks,
    ticktext = layout_helper$y_labels,
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
  p <- plotly::plotly_empty(
    type = "scatter",
    mode = "markers"
  ) %>%
    plotly::layout(
      title = title,
      separators = ".",
      legend = l,
      xaxis = x_config,
      yaxis = y_config,
      margin = m
    )
  return(p)
}

plot_prob_plotly <- function(
  p_obj, prob_tbl,
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

  name <- if (length(unique(prob_tbl$method)) == 1) {
    title_trace
  } else {
    paste0(title_trace, ": ", prob_tbl$method)
  }

  # Construct probability plot:
  p_prob <- p_obj %>%
    plotly::add_trace(
      data = prob_tbl,
      x = ~characteristic,
      y = ~q,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      name = name,
      color = ~method,
      legendgroup = ~method,
      text = paste(
        "ID:", prob_tbl$id,
        paste("<br>", paste0(mark_x, ":")), prob_tbl$characteristic,
        paste("<br>", paste0(mark_y, ":")), round(prob_tbl$prob, digits = 5)
      )
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_prob)
}

plot_prob_mix_plotly <- function(
  p_obj,
  tbl_group,
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

  # Defining colors (max 5 subgroups):
  colors <- c(I("#3C8DBC"), I("#FF0000"), I("#008000"), I("#ffa500"), I("#000000"))
  colors <- colors[seq_along(unique(tbl_group$groups))]

  # Construct probability plot:
  plot <- p_obj %>%
    plotly::add_trace(
      data = tbl_group,
      x = ~x_s,
      y = ~q,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      color = ~groups,
      colors = colors,
      text = ~paste(
        "ID:", id_s,
        paste("<br>", paste0(mark_x, ":")),
        x_s,
        paste("<br>", paste0(mark_y, ":")),
        round(y_s, digits = 5)
      )
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(plot)
}

plot_mod_plotly <- function(
  p_obj, tbl_pred, title_trace = "Fit"
) {

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text, " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text, " "))[1]

  to_hovertext <- function(x_p, y_p, param_val, param_label) {
    param_val <- unlist(param_val)
    param_label <- unlist(param_label)

    text <- paste(
      paste0(x_mark, ":"),
      round(x_p, digits = 2),
      paste("<br>", paste0(y_mark, ":")),
      round(y_p, digits = 5),
      "<br>",
      paste(param_label[1], param_val[1]),
      "<br>",
      paste(param_label[2], param_val[2])
    )

    if (length(param_val) == 3) {
      text <- paste(
        text,
        "<br>",
        paste(param_label[3], param_val[3])
      )
    }
    text
  }

  # Defining hovertext regarding amount of parameters:
  tbl_pred <- tbl_pred %>%
    # Enables access of list column
    dplyr::mutate(hovertext = to_hovertext(x_p, y_p, param_val, param_label))

  name <- if (length(unique(tbl_pred$method)) == 1) {
    title_trace
  } else {
    paste0(title_trace, ": ", tbl_pred$method)
  }

  p_mod <- plotly::add_lines(
    p = p_obj,
    data = tbl_pred,
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    color = ~method,
    name = name,
    legendgroup = ~method,
    text = ~hovertext
  )

  return(p_mod)
}

plot_mod_mix_plotly <- function(p_obj, tbl_group, title_trace) {

  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  p_mod <- p_obj %>% plotly::add_lines(
    data = tbl_group %>% dplyr::group_by(groups),
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    line = list(color = ~cols),
    name = ~groups,
    hoverinfo = "text",
    text = paste(
      paste0(x_mark, ":"),
      round(tbl_group$x_p, digits = 2),
      paste(
        "<br>",
        paste0(y_mark, ":")
      ),
      round(tbl_group$y_p, digits = 5),
      "<br>",
      paste(tbl_group$lab_1, tbl_group$par_1),
      "<br>",
      paste(tbl_group$lab_2, tbl_group$par_2))
  )

  return(p_mod)
}

plot_conf_plotly <- function(p_obj, tbl_p, title_trace) {
  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  p_conf <- plotly::add_lines(
    p = p_obj,
    # tbl_p is grouped by bound. Therefore two separate lines are drawn
    # for two-sided confidence intervals
    data = tbl_p,
    x = ~x, y = ~q,
    type = "scatter", mode = "lines",
    hoverinfo = "text",
    line = list(dash = "dash", width = 1),
    color = I("#CC2222"),
    name = title_trace,
    legendgroup = "Interval",
    text = paste(
      paste0(x_mark, ":"),
      round(tbl_p$x, digits = 2),
      paste("<br>", paste0(y_mark, ":")),
      round(tbl_p$y, digits = 5)
    )
  )

  return(p_conf)
}

plot_pop_plotly <- function(
  p_obj, tbl_pop, title_trace
) {
  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  tbl_pop <- dplyr::group_by(tbl_pop, group)

  # preparation of hovertexts:
  text <- paste(
    paste0(x_mark, ":"),
    round(tbl_pop$x_s, digits = 2),
    paste("<br>", paste0(y_mark, ":")),
    round(tbl_pop$y_s, digits = 5),
    "<br>",
    paste(tbl_pop$param_label_1, tbl_pop$param_val_1),
    "<br>",
    paste(tbl_pop$param_label_2, tbl_pop$param_val_2)
  )

  if ("param_label_3" %in% names(tbl_pop)) {
    text <- paste(
      text,
      "<br>",
      paste(tbl_pop$param_label_3, tbl_pop$param_val_3)
    )
  }

  p_pop <- plotly::add_lines(
    p = p_obj, data = tbl_pop,
    x = ~x_s, y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    # color = ~group,
    name = ~group,
    line = list(width = 1),
    text = text
  ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_pop)
}
