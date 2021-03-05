plot_layout_helper <- function(x, distribution, plot_method = c("plotly", "ggplot2")) {

  plot_method <- match.arg(plot_method)

  # Define x-ticks of logarithm to the base of 10 for Log-Location-Scale Distributions:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {

    # Layout dependent on data x, function to build helpful sequences:
    x_base <- function(xb) floor(log10(xb))
    xlog10_range <- (x_base(min(x)) - 1):x_base(max(x))
    # x-ticks and x-labels
    x_ticks <- sapply(xlog10_range, function(z) seq(10 ^ z, 10 ^ (z + 1), 10 ^ z),
                      simplify = TRUE)
    x_ticks <- round(as.numeric(x_ticks), digits = 10)
    x_ticks <- x_ticks[!duplicated(x_ticks)]
    x_labels <- x_ticks
    x_labels[c(rep(F, 3), rep(T, 6))] <- " "
  } else {
    # We don't need these values, therefore we return NULL
    x_ticks <- if (plot_method == "plotly") NULL else ggplot2::waiver()
    x_labels <- if (plot_method == "plotly") NULL else ggplot2::waiver()
  }

  # y-ticks and y-labels
  # hard coded but it's okay since range is always between 0 and 1.
  y_s <- c(.0000001, .000001, .00001, .0001, .001, .01, .05, .1, .2, .3, .5, .6,
           .7, .8, .9, .95, .99, .999, .9999, .99999)

  y_ticks <- q_std(y_s, distribution)

  y_labels <- y_s * 100

  # Plot:

  l <- list(
    x_ticks = x_ticks,
    x_labels = x_labels,
    y_ticks = y_ticks,
    y_labels = y_labels
  )

  return(l)
}



plot_prob_helper <- function(
  tbl, distribution
) {
  tbl <- tbl %>%
    dplyr::filter(.data$status == 1) %>%
    dplyr::arrange(.data$x)

  tbl$q <- q_std(tbl$prob, distribution)

  tbl
}



plot_mod_helper <- function(
  x, dist_params, distribution, cdf_estimation_method = NA_character_
) {
  if (length(x) == 2) {
    if (two_parametric(distribution) %in% c("weibull", "lognormal", "loglogistic")) {
      x_p <- 10 ^ seq(log10(x[1]), log10(x[2]), length.out = 100)
    } else {
      x_p <- seq(x[1], x[2], length.out = 100)
    }
  }

  if (length(x) > 2) {
    if (
      length(x) < 30 &&
      distribution %in% c("weibull3", "lognormal3", "loglogistic3")
    ) {
      warning(
        "'x' has less than 30 values and distribution is three-parametric.
        Consider using 'x = range(x)' to avoid visual kinks in regression line.
        ",
        call. = FALSE
      )
    }

    x_p <- x
  }

  y_p <- predict_prob(
    q = x_p,
    dist_params = dist_params,
    distribution = distribution
  )

  tbl_pred <- tibble::tibble(x_p = x_p, y_p = y_p)

  q <- q_std(y_p, two_parametric(distribution))

  # preparation of plotly hovers:
  ## raises problems if one-parameter distributions like exponential will be implemented!
  param_val <- format(dist_params, digits = 3)
  # Enforce length 3
  if (length(dist_params) == 2) param_val <- c(param_val, NA)
  param_label <- if (length(dist_params) == 2) {
    c("\u03BC:", "\u03C3:", NA)
  } else {
    c("\u03BC:", "\u03C3:", "\u03B3:")
  }

  tbl_pred <- tbl_pred %>%
    dplyr::mutate(
      param_val = list(param_val),
      param_label = list(param_label),
      cdf_estimation_method = cdf_estimation_method,
      group = NA_character_,
      q = q
    )
}



plot_mod_mix_helper <- function(model_estimation, cdf_estimation_method, group) {
  distribution <- model_estimation$distribution
  data <- model_estimation$data %>%
    dplyr::filter(.data$status == 1)

  x_min <- min(data$x, na.rm = TRUE)
  x_max <- max(data$x, na.rm = TRUE)

  x_p <- seq(x_min, x_max, length.out = 200)
  y_p <- predict_prob(
    q = x_p,
    dist_params = model_estimation$coefficients,
    distribution = distribution
  )

  param_1 <- format(model_estimation$coefficients[[1]], digits = 3)
  param_2 <- format(model_estimation$coefficients[[2]], digits = 3)
  label_1 <- "\u03BC:"
  label_2 <- "\u03C3:"

  tbl_p <- tibble::tibble(
    x_p = x_p,
    y_p = y_p,
    param_val = list(c(param_1, param_2)),
    param_label = list(c(label_1, label_2)),
    cdf_estimation_method = cdf_estimation_method,
    group = group
  )

  tbl_p$q <- q_std(tbl_p$y_p, distribution)

  tbl_p
}



# plot_conf.wt_confint
plot_conf_helper_2 <- function(confint) {
  direction <- attr(confint, "direction", exact = TRUE)
  distribution <- attr(confint, "distribution", exact = TRUE)

  tbl_upper <- if (hasName(confint, "upper_bound")) {
    if (direction == "x") {
      tibble::tibble(
        x = confint$upper_bound,
        y = confint$prob,
        bound = "Upper",
        cdf_estimation_method = confint$cdf_estimation_method
      )
    } else {
      tibble::tibble(
        x = confint$x,
        y = confint$upper_bound,
        bound = "Upper",
        cdf_estimation_method = confint$cdf_estimation_method
      )
    }
  }

  tbl_lower <- if (hasName(confint, "lower_bound")) {
    if (direction == "x") {
      tibble::tibble(
        x = confint$lower_bound,
        y = confint$prob,
        bound = "Lower",
        cdf_estimation_method = confint$cdf_estimation_method
      )
    } else {
      tibble::tibble(
        x = confint$x,
        y = confint$lower_bound,
        bound = "Lower",
        cdf_estimation_method = confint$cdf_estimation_method
      )
    }
  }

  tbl_p <- dplyr::bind_rows(tbl_upper, tbl_lower)

  tbl_p$q <- q_std(tbl_p$y, two_parametric(distribution))

  tbl_p <- dplyr::group_by(tbl_p, .data$bound)

  return(tbl_p)
}



# plot_conf.default
plot_conf_helper <- function(tbl_mod, x, y, direction, distribution) {
  # Construct x, y from x/y, upper/lower bounds (depending on direction and bounds)
  lst <- Map(tibble::tibble, x = x, y = y)
  tbl_p <- dplyr::bind_rows(lst, .id = "bound")

  if (direction == "y") {
    tbl_p$bound <- ifelse(test = tbl_p$y < tbl_mod$y_p, yes = "Lower", no = "Upper")
  } else {
    tbl_p$bound <- ifelse(test = tbl_p$x < tbl_mod$x_p, yes = "Lower", no = "Upper")
  }

  tbl_p$q <- q_std(tbl_p$y, two_parametric(distribution))

  tbl_p <- dplyr::group_by(tbl_p, .data$bound)
  tbl_p$cdf_estimation_method <- NA_character_

  return(tbl_p)
}



plot_pop_helper <- function(x, dist_params_tbl, distribution, tol = 1e-6) {
  x_s <- if (length(x) == 2) {
    10 ^ seq(log10(x[1]), log10(x[2]), length.out = 200)
  } else {
    x
  }

  tbl_pop <- dist_params_tbl %>%
    dplyr::mutate(group = as.character(dplyr::row_number()))

  # Map predict_prob over tbl_pop
  tbl_pop <- purrr::pmap_dfr(
    tbl_pop,
    x_s = x_s,
    distribution = distribution,
    function(loc, sc, thres = NA, group, x_s, distribution) {
      # Replace NA with NULL, so that thres is ignored in c()
      dist_params <- c(loc, sc, thres %NA% NULL)

      if (length(dist_params) == 3) {
        # Case three-parametric distribution
        distribution <- paste0(distribution, "3")
      }

      tibble::tibble(
        loc = loc,
        sc = sc,
        thres = thres,
        group = group,
        x_s = x_s,
        y_s = predict_prob(
          q = x_s,
          dist_params = dist_params,
          distribution = distribution
        )
      )
    }
  )

  tbl_pop <- tbl_pop %>%
    dplyr::filter(.data$y_s < 1, .data$y_s > 0)

  tbl_pop$q <- q_std(tbl_pop$y_s, distribution)

  # set values and labels for plotlys hoverinfo:
  tbl_pop <- tbl_pop %>%
    dplyr::mutate(
      param_val_1 = format(.data$loc, digits = 3),
      param_val_2 = format(.data$sc, digits = 3),
      param_val_3 = ifelse(is.na(.data$thres), NA, format(.data$thres, digits = 3)),
      param_label_1 = "\u03BC:",
      param_label_2 = "\u03C3:",
      param_label_3 = ifelse(is.na(.data$thres), NA, "\u03B3:")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      param_val = list(
        c(.data$param_val_1, .data$param_val_2, .data$param_val_3)
      ),
      param_label = list(
        c(.data$param_label_1, .data$param_label_2, .data$param_label_3)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$x_s, .data$y_s, .data$q, .data$param_val, .data$param_label,
      .data$group
    ) %>%
    dplyr::filter(.data$y_s <= 1 - tol, .data$y_s >= tol)

  return(tbl_pop)
}
