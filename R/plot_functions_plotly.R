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
    x_config <- c(
      x_config,
      list(
        tickvals = layout_helper$x_ticks,
        ticktext = layout_helper$x_ticks
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
  p <- plotly::plotly_empty() %>%
    plotly::layout(title = title, separators = ".",
                   legend = l, xaxis = x_config, yaxis = y_config, margin = m)
  return(p)
}

plot_prob_plotly <- function(
  p_obj, x, prob_df,
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

  color <- if (length(unique(prob_df$distribution)) == 1) I("#3C8DBC") else
    ~distribution

  # Construct probability plot:
  p_prob <- p_obj %>%
    plotly::add_trace(data = prob_df, x = ~x_s, y = ~q, type = "scatter",
                      mode = "markers", hoverinfo = "text",
                      color = color,
                      name = title_trace,
                      text = ~paste("ID:", ~id,
                                    paste("<br>", paste0(mark_x, ":")), prob_df$x_s,
                                    paste("<br>", paste0(mark_y, ":")), round(prob_df$y_s, digits = 5))
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_prob)
}

plot_prob_mix_plotly <- function(
  group_df,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  # Plot layout:
  p <- plot_layout(
    x = group_df$x_s,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    plot_method = "plotly"
  )

  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Defining colors (max 5 subgroups):
  cols <- c(I("#3C8DBC"), I("#FF0000"), I("#008000"), I("#ffa500"), I("#000000"))
  cols <- cols[seq_along(unique(group_df$groups))]

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(
      data = group_df,
      x = ~x_s,
      y = ~q,
      type = "scatter",
      mode = "markers",
      hoverinfo = "text",
      color = ~groups,
      colors = cols,
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
  p_obj, df_pred, param_val, param_label, title_trace = "Fit"
) {

  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text, " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text, " "))[1]

  # Defining hovertext regarding amount of parameters:
  hovertext <- paste(
    paste0(x_mark, ":"),
    round(df_pred$x_p, digits = 2),
    paste("<br>", paste0(y_mark, ":")),
    round(df_pred$y_p, digits = 5),
    "<br>",
    paste(param_label[1], param_val[1]),
    "<br>",
    paste(param_label[2], param_val[2])
  )

  if (length(param_val) == 3) {
    hovertext <- paste(
      hovertext,
      "<br>",
      paste(param_label[3], param_val[3])
    )
  }

  p_mod <- plotly::add_lines(
    p = p_obj,
    data = df_pred,
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    color = I("#CC2222"),
    name = title_trace,
    text = hovertext
  )

  return(p_mod)
}

plot_mod_mix_plotly <- function(p_obj, group_df, title_trace) {

  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  p_mod <- p_obj %>% plotly::add_lines(
    data = group_df %>% dplyr::group_by(groups),
    x = ~x_p,
    y = ~q,
    type = "scatter",
    mode = "lines",
    line = list(color = ~cols),
    name = ~groups,
    hoverinfo = "text",
    text = paste(
      paste0(x_mark, ":"),
      round(group_df$x_p, digits = 2),
      paste(
        "<br>",
        paste0(y_mark, ":")
      ),
      round(group_df$y_p, digits = 5),
      "<br>",
      paste(group_df$lab_1, group_df$par_1),
      "<br>",
      paste(group_df$lab_2, group_df$par_2))
  )

  return(p_mod)
}

plot_conf_plotly <- function(p_obj, df_p, title_trace) {
  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  p_conf <- plotly::add_lines(
    p = p_obj,
    # df_p is grouped by bound. Therefore two separate lines are drawn
    # for two-sided confidence intervals
    data = df_p,
    x = ~x, y = ~q,
    type = "scatter", mode = "lines",
    hoverinfo = "text",
    line = list(dash = "dash", width = 1),
    color = I("#CC2222"),
    name = title_trace,
    legendgroup = "Interval",
    text = paste(
      paste0(x_mark, ":"),
      round(x, digits = 2),
      paste("<br>", paste0(y_mark, ":")),
      round(y, digits = 5)
    )
  )

  return(p_conf)
}

plot_pop_plotly <- function(
  p_obj, df_pop, param_val, param_label, color, title_trace
) {
  # Get axis labels in hover:
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title$text,  " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title$text,  " "))[1]

  p_pop <- plotly::add_lines(
    p = p_obj, data = df_pop,
    x = ~x_s, y = ~q,
    type = "scatter",
    mode = "lines",
    hoverinfo = "text",
    color = color,
    name = title_trace,
    line = list(width = 1),
    text = paste(
      paste0(x_mark, ":"),
      round(df_pop$x_s, digits = 2),
      paste("<br>", paste0(y_mark, ":")),
      round(df_pop$y_s, digits = 5),
      "<br>",
      paste(param_label[1], param_val[1]),
      "<br>",
      paste(param_label[2], param_val[2])
    )
  ) %>%
    plotly::layout(showlegend = TRUE)

  return(p_pop)
}
