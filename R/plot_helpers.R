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

  if (distribution %in% c("weibull", "sev")) {
    y_ticks <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    y_ticks <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    y_ticks <- stats::qlogis(y_s)
  }

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
    dplyr::filter(status == 1) %>%
    dplyr::arrange(x)

  # Choice of distribution:
  if (distribution %in% c("weibull", "sev")) {
    q <- SPREDA::qsev(tbl$prob)
  }
  if (distribution %in% c("lognormal", "normal")) {
    q <- stats::qnorm(tbl$prob)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    q <- stats::qlogis(tbl$prob)
  }
  tbl$q <- q

  if (!hasName(tbl, "group")) tbl$group <- "null"

  tbl
}

plot_mod_helper <- function(
  x, loc_sc_params, distribution, method = "null"
) {
  if (length(x) == 2) {
    if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
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
        "x has less than 30 values and distribution is three-parametric.
        Consider using x = range(x) to avoid visual kinks in regression line.
        "
      )
    }

    x_p <- x
  }

  y_p <- predict_prob(
    q = x_p,
    loc_sc_params = loc_sc_params,
    distribution = distribution
  )

  tbl_pred <- tibble::tibble(x_p = x_p, y_p = y_p)

  # Smallest Extreme Value Distribution:
  if (distribution %in% c("weibull", "weibull3", "sev")) {
    q <- SPREDA::qsev(y_p)
  }

  # Standard Normal Distribution:
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    q <- stats::qnorm(y_p)
  }

  # Standard Logistic Distribution:
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    q <- stats::qlogis(y_p)
  }

  # preparation of plotly hovers:
  ## raises problems if one-parameter distributions like exponential will be implemented!
  param_val <- format(loc_sc_params, digits = 3)
  # Enforce length 3
  if (length(loc_sc_params) == 2) param_val <- c(param_val, NA)
  param_label <- if (length(loc_sc_params) == 2) {
    c("\u03BC:", "\u03C3:", NA)
  } else {
    c("\u03BC:", "\u03C3:", "\u03B3:")
  }

  tbl_pred <- tbl_pred %>%
    dplyr::mutate(
      param_val = list(param_val),
      param_label = list(param_label),
      method = method,
      group = "_null",
      q = q
    )
}

plot_mod_mix_helper <- function(model_estimation, method, group) {
  distribution <- model_estimation$distribution
  data <- model_estimation$data %>%
    dplyr::filter(status == 1)

  x_min <- min(data$x, na.rm = TRUE)
  x_max <- max(data$x, na.rm = TRUE)

  x_p <- seq(x_min, x_max, length.out = 200)
  y_p <- predict_prob(
    q = x_p,
    loc_sc_params = model_estimation$loc_sc_params,
    distribution = distribution
  )

  param_1 <- format(model_estimation$loc_sc_params[[1]], digits = 3)
  param_2 <- format(model_estimation$loc_sc_params[[2]], digits = 3)
  label_1 <- "\u03BC:"
  label_2 <- "\u03C3:"

  tbl_p <- tibble::tibble(
    x_p = x_p,
    y_p = y_p,
    param_val = list(c(param_1, param_2)),
    param_label = list(c(label_1, label_2)),
    method = method,
    group = group
  )

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(tbl_p$y_p)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(tbl_p$y_p)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(tbl_p$y_p)
  }
  tbl_p$q <- q

  tbl_p
}

plot_conf_helper <- function(tbl_mod, x, y, direction, distribution) {
  # Construct x, y from x/y, upper/lower bounds (depending on direction and bounds)
  lst <- Map(tibble::tibble, x = x, y = y)
  tbl_p <- dplyr::bind_rows(lst, .id = "bound")

  if (direction == "y") {
    tbl_p$bound <- ifelse(test = tbl_p$y < tbl_mod$y_p, yes = "Lower", no = "Upper")
  } else {
    tbl_p$bound <- ifelse(test = tbl_p$x < tbl_mod$x_p, yes = "Lower", no = "Upper")
  }

  if (distribution %in% c("weibull", "weibull3", "sev")) {
    tbl_p$q <- SPREDA::qsev(tbl_p$y)
  }
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    tbl_p$q <- stats::qnorm(tbl_p$y)
  }
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    tbl_p$q <- stats::qlogis(tbl_p$y)
  }

  tbl_p <- dplyr::group_by(tbl_p, bound)

  return(tbl_p)
}

plot_pop_helper <- function(x, loc_sc_params_tbl, distribution, tol = 1e-6) {
  x_s <- if (length(x) == 2) {
    10 ^ seq(log10(x[1]), log10(x[2]), length.out = 200)
  } else {
    x
  }

  tbl_pop <- loc_sc_params_tbl %>%
    dplyr::mutate(group = as.character(dplyr::row_number()))
  # column x_y is list column that contains a tibble each
  tbl_pop$x_y <- purrr::pmap(
    loc_sc_params_tbl,
    x_s = x_s,
    distribution = distribution,
    function(loc, sc, thres = NA, x_s, distribution) {
      # Replace NA with NULL, so that thres is ignored in c()
      if (is.na(thres)) thres <- NULL

      loc_sc_params <- c(loc, sc, thres)

      if (length(loc_sc_params) == 3) {
        # Case three-parametric distribution
        distribution <- paste0(distribution, "3")
      }

      tibble::tibble(
        x_s = x_s,
        y_s = predict_prob(
          q = x_s,
          loc_sc_params = loc_sc_params,
          distribution = distribution
        )
      )
    }
  )

  tbl_pop <- tbl_pop %>%
    tidyr::unnest(cols = x_y) %>%
    dplyr::filter(y_s < 1, y_s > 0)

  if (distribution %in% c("weibull", "weibull3", "sev")) {
    tbl_pop <- tbl_pop %>%
      dplyr::mutate(q = SPREDA::qsev(y_s))
  }
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    tbl_pop <- tbl_pop %>%
      dplyr::mutate(q = stats::qnorm(y_s))
  }
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    tbl_pop <- tbl_pop %>%
      dplyr::mutate(q = stats::qlogis(y_s))
  }

  # set values and labels for plotlys hoverinfo:
  tbl_pop <- tbl_pop %>%
    dplyr::mutate(
      param_val_1 = format(tbl_pop$loc, digits = 3),
      param_val_2 = format(tbl_pop$sc, digits = 3),
      param_val_3 = ifelse(is.na(tbl_pop$thres), NA, format(tbl_pop$thres, digits = 3)),
      param_label_1 = "\u03BC:",
      param_label_2 = "\u03C3:",
      param_label_3 = ifelse(is.na(tbl_pop$thres), NA, "\u03B3:")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      param_val = list(c(param_val_1, param_val_2, param_val_3)),
      param_label = list(c(param_label_1, param_label_2, param_label_3))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(x_s, y_s, q, param_val, param_label, group) %>%
    dplyr::filter(y_s <= 1 - tol, y_s >= tol)

  return(tbl_pop)
}
