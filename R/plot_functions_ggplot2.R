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
        labels = layout_helper$x_labels
      )
  } else {
    p +
      ggplot2::scale_x_log10(
        breaks = layout_helper$x_ticks,
        minor_breaks = NULL,
        labels = layout_helper$x_labels
      )
  }

  p <- p +
    ggplot2::scale_y_continuous(
      breaks = layout_helper$y_ticks,
      minor_breaks = NULL,
      labels = layout_helper$y_labels,
    ) +
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
    ggplot2::geom_point(
      data = prob_df, mapping = ggplot2::aes(x = x_s, y = q), color = I("#3C8DBC")
    )

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
    )

  return(p)
}

plot_mod_ggplot2 <- function(
  p_obj, df_pred, param_val, param_label, title_trace = "Fit"
) {

  # Compute limits, if prediction limits goes beyond current limits
  xlim <- ggplot_build(p_obj)$layout$coord$limits$x
  ylim <- ggplot_build(p_obj)$layout$coord$limits$y

  if (is.null(xlim)) {
    xlim <- range(df_pred$x_p)
  } else {
    pred_range <- range(df_pred$x_p)
    xlim[1] <- min(xlim[1], pred_range[1])
    xlim[2] <- max(xlim[2], pred_range[2])
  }

  if (is.null(ylim)) {
    ylim <- range(df_pred$q)
  } else {
    pred_range <- range(df_pred$q)
    ylim[1] <- min(ylim[1], pred_range[1])
    ylim[2] <- max(ylim[2], pred_range[2])
  }

  p <- p_obj +
    ggplot2::geom_line(
      data = df_pred, mapping = ggplot2::aes(x = x_p, y = q),
      color = I("#CC2222")
    )

  return(p)
}
