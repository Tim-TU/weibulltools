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
  x, y, event,
  id = rep("XXXXXX", length(x)),
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
    x = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )

  prob_helper <- plot_prob_helper(
    x, y, event, id, distribution
  )

  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]

  # Construct probability plot:
  plot <- p %>%
    plotly::add_trace(x = prob_helper$x, y = prob_helper$q, type = "scatter",
                      mode = "markers", hoverinfo = "text", color = I("#3C8DBC"),
                      name = title_trace,
                      text = ~paste("ID:", prob_helper$id,
                                    paste("<br>", paste0(mark_x, ":")), prob_helper$x,
                                    paste("<br>", paste0(mark_y, ":")), round(prob_helper$y, digits = 5))
    ) %>%
    plotly::layout(showlegend = TRUE)

  return(plot)
}
