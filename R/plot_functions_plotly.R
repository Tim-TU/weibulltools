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

  n_group <- length(unique(tbl_prob$group))
  n_method <- length(unique(tbl_prob$method))

  name <- to_prob_mod_name(tbl_prob, n_method, n_group, title_trace)

  # Prevent warning in RColorBrewer::brewer.pal
  if (n_method < 3) {
    tbl_prob$method <- factor(
      tbl_prob$method,
      c(unique(tbl_prob$method), "_null1", "_null2")
    )
  }

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
      color = ~method,
      symbol = ~group,
      legendgroup = ~method,
      text = paste(
        "ID:", tbl_prob$id,
        paste("<br>", paste0(mark_x, ":")), format(tbl_prob$x, digits = 3),
        paste("<br>", paste0(mark_y, ":")), format(tbl_prob$prob, digits = 6)
      )
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_prob)
}

# only used in plot_prob_mix.default
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

  # Construct probability plot:
  plot <- p_obj %>%
    plotly::add_trace(
      data = tbl_group,
      x = ~x_s,
      y = ~q,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      color = ~method,
      symbols = ~group,
      text = ~paste(
        "ID:", id_s,
        paste("<br>", paste0(mark_x, ":")),
        format(x_s, digits = 3),
        paste("<br>", paste0(mark_y, ":")),
        format(y_s, digits = 6)
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

  # Defining hovertext regarding amount of parameters:
  tbl_pred <- tbl_pred %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hovertext = to_hovertext(
      x_p, y_p, param_val, param_label, x_mark, y_mark
    )) %>%
    dplyr::ungroup()

  n_method <- length(unique(tbl_pred$method))
  n_group <- length(unique(tbl_pred$group))

  name <- to_prob_mod_name(tbl_pred, n_method, n_group, title_trace)

  if (n_method < 3) {
    tbl_pred$method <- factor(
      tbl_pred$method,
      levels = c(unique(tbl_pred$method, "_null1", "_null2"))
    )
  }


  p_mod <- plotly::add_lines(
    p = p_obj,
    data = tbl_pred,
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    name = name,
    color = ~method,
    legendgroup = ~method,
    text = ~hovertext
  )

  return(p_mod)
}

# only used in plot_mod_mix.default
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
      format(tbl_group$x_p, digits = 3),
      paste(
        "<br>",
        paste0(y_mark, ":")
      ),
      format(tbl_group$y_p, digits = 6),
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
      format(tbl_p$x, digits = 3),
      paste("<br>", paste0(y_mark, ":")),
      format(tbl_p$y, digits = 6)
    )
  )

  return(p_conf)
}

plot_pop_plotly <- function(
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
        x_s, y_s, param_val, param_label, x_mark, y_mark
      ),
      name = to_name(
        param_val, param_label
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
    name = ~name,
    line = list(width = 1),
    text = ~hovertext
  ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_pop)
}



to_hovertext <- function(x, y, param_val, param_label, x_mark, y_mark) {
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

to_name <- function(param_val, param_label) {
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

to_prob_mod_name <- function(tbl, n_method, n_group, title_trace) {
  if (n_method == 1) {
    if (n_group == 1) {
      title_trace
    } else {
      paste0(title_trace, ": ", tbl$group)
    }
  } else {
    if (n_group == 1) {
      paste0(title_trace, ": ", tbl$method)
    } else {
      paste0(title_trace, ": ", tbl$method, ", ", tbl$group)
    }
  }
}
