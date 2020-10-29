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
      data = prob_df, mapping = ggplot2::aes(x = x_s, y = q, color = I("#3C8DBC"))
    ) +
    ggplot2::labs(color = title_trace) +
    ggplot2::scale_color_identity(guide = "legend", labels = "")

  return(p)
}

plot_prob_mix_ggplot2 <- function(
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
    ggplot2::labs(color = title_trace)

  return(p)
}

plot_mod_ggplot2 <- function(
  p_obj, df_pred, param_val, param_label, title_trace = "Fit"
) {
  p_mod <- p_obj +
    ggnewscale::new_scale_color() +
    ggplot2::geom_line(
      data = df_pred, mapping = ggplot2::aes(x = x_p, y = q, color = I("#CC2222"))
    ) +
    ggplot2::labs(color = title_trace) +
    ggplot2::scale_color_identity(guide = "legend", labels = "")

  return(p_mod)
}

plot_mod_mix_ggplot2 <- function(p_obj, group_df, title_trace) {
  p_mod <- p_obj +
    ggnewscale::new_scale_color() +
    ggplot2::geom_line(
      data = group_df, mapping = ggplot2::aes(x = x_p, y = q, color = groups)
    ) +
    ggplot2::labs(color = title_trace)

  return(p_mod)
}

plot_conf_ggplot2 <- function(p_obj, df_p, title_trace) {
  p_conf <- p_obj +
    ggnewscale::new_scale_color() +
    ggplot2::geom_line(
      data = df_p,
      mapping = ggplot2::aes(
        x = x, y = q, group = bound, color = I("#CC2222")
      ),
      linetype = "CC"
    ) +
    ggplot2::labs(color = title_trace) +
    ggplot2::scale_color_identity(guide = "legend", labels = "")

  return(p_conf)
}

plot_pop_ggplot2 <- function(
  p_obj, df_pop, param_val, param_label, color, title_trace
) {
  p_pop <- p_obj +
    ggplot2::geom_line(
      data = df_pop, mapping = ggplot2::aes(x = x_s, y = q, color = color)
    ) +
    ggplot2::labs(color = title_trace) +
    ggplot2::scale_color_identity(guide = "legend", labels = "")

  return(p_pop)
}
