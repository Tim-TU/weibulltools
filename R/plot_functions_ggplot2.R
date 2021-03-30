#' @export
plot_layout_vis.ggplot <- function(p_obj, # An empty ggplot object.
                                   x, # Named list with x_ticks and x_labels.
                                   y, # Named list with y_ticks and y_labels.
                                   distribution = c(
                                     "weibull", "lognormal", "loglogistic",
                                     "sev", "normal", "logistic",
                                     "exponential"
                                   ),
                                   title_main = "Probability Plot",
                                   title_x = "Characteristic",
                                   title_y = "Unreliability"
) {

  distribution <- match.arg(distribution)

  p_obj <- if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    p_obj +
      ggplot2::scale_x_log10(
        breaks = x$x_ticks,
        minor_breaks = NULL,
        labels = x$x_labels
      )
  } else {
    p_obj +
      ggplot2::scale_x_continuous(
        breaks = ggplot2::waiver(),
        minor_breaks = NULL,
        labels = ggplot2::waiver()
      )
  }

  p_obj <- p_obj +
    ggplot2::scale_y_continuous(
      breaks = y$y_ticks,
      minor_breaks = NULL,
      labels = y$y_labels,
      guide = ggplot2::guide_axis( # experimental!
        check.overlap = TRUE
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # Rotate x axis labels:
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      # Center title:
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(title = title_main, x = title_x, y = title_y)

  p_obj
}



#' @export
plot_prob_vis.ggplot <- function(p_obj,
                                 tbl_prob,
                                 distribution = c(
                                   "weibull", "lognormal", "loglogistic",
                                   "sev", "normal", "logistic",
                                   "exponential"
                                 ),
                                 title_main = "Probability Plot",
                                 title_x = "Characteristic",
                                 title_y = "Unreliability",
                                 title_trace = "Sample"
) {

  distribution <- match.arg(distribution)

  n_method <- length(unique(tbl_prob$cdf_estimation_method))
  n_group <- length(unique(tbl_prob[["group"]]))

  if (n_method == 1) tbl_prob$cdf_estimation_method <- ""
  if (n_group <= 1) tbl_prob$group <- ""

  mapping <- if (n_group <= 1) {
    if (n_method == 1) {
      ggplot2::aes(x = .data$x, y = .data$q, color = I("#3C8DBC"))
    } else {
      ggplot2::aes(x = .data$x, y = .data$q, color = .data$cdf_estimation_method)
    }
  } else {
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x, y = .data$q, color = .data$group
      )
    } else {
      ggplot2::aes(
        x = .data$x, y = .data$q, color = .data$cdf_estimation_method,
        shape = .data$group
      )
    }
  }

  labs <- if (n_group <= 1 || n_method == 1) {
    ggplot2::labs(color = title_trace)
  } else {
    ggplot2::labs(color = title_trace, shape = "Subgroups")
  }

  p_prob <- p_obj +
    ggplot2::geom_point(
      data = tbl_prob, mapping = mapping
    ) +
    labs

  p_prob
}



#' @export
plot_mod_vis.ggplot <- function(p_obj,
                                tbl_mod,
                                title_trace = "Fit"
) {

  n_method <- length(unique(tbl_mod$cdf_estimation_method))
  n_group <- length(unique(tbl_mod$group))

  if (n_method == 1) tbl_mod$cdf_estimation_method <- ""

  mapping <- if (n_group == 1) {
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x_p, y = .data$q, color = I("#CC2222")
      )
    } else {
      ggplot2::aes(
        x = .data$x_p, y = .data$q, color = .data$cdf_estimation_method
      )
    }
  } else {
    # group aesthetic must be paste of method and group to create distinct
    # groups
    if (n_method == 1) {
      ggplot2::aes(
        x = .data$x_p,
        y = .data$q,
        color = .data$group
      )
    } else {
      ggplot2::aes(
        x = .data$x_p,
        y = .data$q,
        color = .data$cdf_estimation_method,
        group = paste(.data$cdf_estimation_method, .data$group)
      )
    }
  }

  p_mod <- p_obj +
    ggplot2::geom_line(
      data = tbl_mod, mapping = mapping
    ) +
    ggplot2::labs(
      color = paste(p_obj$labels$colour, "+\n", title_trace)
    )

  p_mod
}



#' @export
plot_conf_vis.ggplot <- function(p_obj,
                                 tbl_p,
                                 title_trace
) {

  n_method <- length(unique(tbl_p$cdf_estimation_method))

  mapping <- if (all(is.na(tbl_p$cdf_estimation_method)) || n_method == 1) {
    ggplot2::aes(
      x = .data$x, y = .data$q, group = .data$bound, color = I("#CC2222")
    )
  } else {
    ggplot2::aes(
      x = .data$x,
      y = .data$q,
      group = paste(.data$bound, .data$cdf_estimation_method),
      color = .data$cdf_estimation_method
    )
  }

  p_conf <- p_obj +
    ggplot2::geom_line(
      data = tbl_p,
      mapping = mapping,
      linetype = "CC"
    ) +
    ggplot2::labs(
      color = paste(p_obj$labels$colour, "+\n", title_trace)
    )

  p_conf
}



#' @export
plot_pop_vis.ggplot <- function(p_obj,
                                tbl_pop,
                                title_trace
) {
  tbl_pop <- tbl_pop %>%
    dplyr::mutate(
      name = purrr::map2_chr(.data$param_val, .data$param_label, to_name_pop)
    )

  p_pop <- p_obj +
    ggplot2::geom_line(
      data = tbl_pop,
      mapping = ggplot2::aes(x = .data$x_s, y = .data$q, color = .data$name)
    ) +
    ggplot2::labs(color = title_trace)

  p_pop
}
