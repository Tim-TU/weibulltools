plot_layout_ggplot2 <- function(
  x,
  distribution = c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  layout_helper <- plot_layout_helper(x, distribution, "ggplot2")

  p <- ggplot2::ggplot()

  p <- if (distribution %in% c("sev", "normal", "logistic")) {
    p +
      ggplot2::scale_x_continuous(
        breaks = layout_helper$x_ticks,
        minor_breaks = NULL,
        labels = layout_helper$x_labels,
        limits = range(x)
      )
  } else {
    p +
      ggplot2::scale_x_log10(
        breaks = layout_helper$x_ticks,
        minor_breaks = NULL,
        labels = layout_helper$x_labels,
        limits = range(x)
      )
  }

  p <- p +
    ggplot2::scale_y_continuous(
      breaks = layout_helper$y_ticks,
      minor_breaks = NULL,
      labels = layout_helper$y_labels,
      limits = range(layout_helper$y_ticks)
    )

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # Rotate x axis labels
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      # Center titel
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = title_main, x = title_x, y = title_y)

  return(p)
}

plot_prob_ggplot2 <- function(
  x,
  y,
  event,
  id = rep("XXXXXX", length(x)),
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic", title_y = "Unreliability",
  title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  prob_helper <- plot_prob_helper(
    x, y, event, id, distribution
  )

  # Plot layout:
  p <- ggplot_layout(
    x = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )

  p <- p +
    ggplot2::geom_point(data = prob_helper, mapping = aes(x = x, y = q))

  return(p)
}
