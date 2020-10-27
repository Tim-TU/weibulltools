plot_layout_ggplot2 <- function(
  x,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
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
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(title = title_main, x = title_x, y = title_y)

  return(p)
}

plot_prob_ggplot2 <- function(
  x, prob_df,
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
  p <- plot_layout_ggplot2(
    x = x,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )

  p <- p +
    ggplot2::geom_point(data = prob_df, mapping = ggplot2::aes(x = x_s, y = q)) +
    # Ensure meaningful y limits
    ggplot2::coord_cartesian(ylim = range(prob_df$q))

  return(p)
}

plot_prob_mix_ggplot2 <- function(
  group_df,
  distribution = c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  ),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  # Plot layout:
  p <- plot_layout_ggplot2(
    x = group_df$x_s,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )

  p <- p + ggplot2::geom_point(
      data = group_df, mapping = ggplot2::aes(x = x_s, y = q, color = groups)
    ) +
    # Ensure meaningful y limits
    ggplot2::coord_cartesian(ylim = range(group_df$q))

  return(p)
}

plot_mod_ggplot2 <- function(
  p_obj, df_pred, param_val, param_label, title_trace = "Fit"
) {

}
