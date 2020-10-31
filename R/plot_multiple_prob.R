plot_multiple_prob <- function(
  cdf_estimation,
  distributions,
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability",
  title_trace = "Sample",
  plot_method = c("plotly", "ggplot2")
) {
  plot_method <- match.arg(plot_method)

  # All distributions should be either on log scale or identity scale
  on_log_scale <- distributions %in% c("weibull", "lognormal", "loglogistic")

  if (!(all(on_log_scale) || all(!on_log_scale))) {
    stop(
      'distributions must either consist of a subset of weibull, lognormal and
      loglogistic or a subset of normal, logistic, sev.'
      )
  }

  # Remove duplicate distributions
  distributions <- unique(distributions)

  # Plot layout:
  p_obj <- plot_layout(
    x = cdf_estimation$characteristic,
    distribution = distributions[1],
    title_main = title_main,
    title_x = title_x,
    title_y = title_y,
    plot_method = plot_method
  )

  plot_prob_fun <- if (plot_method == "plotly") plot_prob_plotly else
    plot_prob_ggplot2

  prob_df_list <- list()
  for (i in seq_along(distributions)) {
    prob_df_list[[i]] <- plot_prob_helper(
      x = cdf_estimation$characteristic,
      y = cdf_estimation$prob,
      event = cdf_estimation$status,
      id = cdf_estimation$id,
      distributions[i]
    )
  }
  prob_df <- dplyr::bind_rows(prob_df_list)
  prob_df <- dplyr::group_by(prob_df, distribution)

  p_prob <- plot_prob_fun(
    p_obj = p_obj,
    x = x,
    prob_df = prob_df,
    distribution = distribution,
    title_main = title_main,
    title_x = title_x,
    title_y = title_y
  )

  return(p_prob)
}
