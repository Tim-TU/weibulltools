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

  n_method <- length(unique(tbl_prob$method))
  n_group <- length(unique(tbl_prob$group))

  if (n_method == 1) {
    tbl_prob$method <- ""
  }

  mapping <- if (n_group == 1) {
    ggplot2::aes(x = x, y = q, color = method)
  } else {
    ggplot2::aes(x = x, y = q, color = method, shape = group)
  }

  labs <- if (n_group == 1) {
    ggplot2::labs(color = title_trace)
  } else {
    ggplot2::labs(color = title_trace, shape = "Subgroups")
  }

  p_prob <- p_obj +
    ggplot2::geom_point(
      data = tbl_prob, mapping = mapping
    ) +
    labs

  return(p_prob)
}

plot_prob_mix_ggplot2 <- function(
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

  p_prob_mix <- p_obj + ggplot2::geom_point(
      data = tbl_group, mapping = ggplot2::aes(x = x_s, y = q, color = groups)
    ) +
    ggplot2::labs(color = title_trace)

  return(p_prob_mix)
}

plot_mod_ggplot2 <- function(
  p_obj, tbl_pred, title_trace = "Fit"
) {

  n_method <- length(unique(tbl_pred$method))
  n_group <- length(unique(tbl_pred$group))

  if (n_method == 1) {
    tbl_pred$method <- ""
  }

  mapping <- if (n_group == 1) {
    ggplot2::aes(x = x_p, y = q, color = method)
  } else {
    # group aesthetic must be paste of method and group to create distinct
    # groups
    ggplot2::aes(x = x_p, y = q, color = method, group = paste(method, group))
  }

  p_mod <- p_obj +
    ggplot2::geom_line(
      data = tbl_pred, mapping = mapping
    ) +
    ggplot2::labs(
      color = paste(p_obj$labels$colour, "+", title_trace)
    )

  return(p_mod)
}

plot_mod_mix_ggplot2 <- function(p_obj, tbl_group, title_trace) {
  p_mod <- p_obj +
    ggplot2::geom_line(
      data = tbl_group, mapping = ggplot2::aes(x = x_p, y = q, color = method)
    ) +
    ggplot2::labs(color = paste(p_obj$labels$colour, "+", title_trace))

  return(p_mod)
}

plot_conf_ggplot2 <- function(p_obj, tbl_p, title_trace) {
  p_conf <- p_obj +
    ggplot2::geom_line(
      data = tbl_p,
      mapping = ggplot2::aes(
        x = x, y = q, group = bound, color = I("#CC2222")
      ),
      linetype = "CC"
    )

  return(p_conf)
}

plot_pop_ggplot2 <- function(
  p_obj, tbl_pop, title_trace
) {
  p_pop <- p_obj +
    ggplot2::geom_line(
      data = tbl_pop, mapping = ggplot2::aes(x = x_s, y = q, color = group)
    ) +
    ggplot2::labs(color = title_trace)

  return(p_pop)
}
