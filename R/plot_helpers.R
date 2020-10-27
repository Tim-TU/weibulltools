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
    x_labels[c(rep(F, 3), rep(T, 6))] <- ''
  } else {
    # We don't need these values, therefore we return NULL
    x_ticks <- if (method == "plotly") NULL else ggplot2::waiver()
    x_labels <- if (method == "plotly") NULL else ggplot2::waiver()
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
  x, y, event, id, distribution
) {
  # Filtering failed units:
  x_s <- x[event == 1]
  y_s <- y[event == 1]
  id_s <- id[event == 1]
  x_s <- x_s[order(x_s)]
  y_s <- y_s[order(x_s)]
  id_s <- id_s[order(x_s)]

  # Choice of distribution:
  if (distribution %in% c("weibull", "sev")) {
    q <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    q <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    q <- stats::qlogis(y_s)
  }

  data.frame(
    x_s = x_s,
    y_s = y_s,
    q = q,
    id_s = id_s
  )
}

plot_prob_mix_helper <- function(
  x, event, id, distribution, mix_output, title_trace
) {
  # check if mix_output is NULL or "em_results" is not a name of lists in mix_output!
  if (is.null(mix_output) || !("em_results" %in%  names(mix_output))) {

    # applying johnson_method() to all data! Used for printing results of
    # mixmod_regression() and for the case is mix_output = NULL.
    john_df <- johnson_method(x = x, event = event, id = id) %>%
      dplyr::filter(status == 1)

    x_s <- john_df$characteristic
    y_s <- john_df$prob
    id_s <- john_df$id
    group_df <- data.frame(x_s = x_s, y_s = y_s, id_s = id_s)
    group_df$groups <- as.factor(c(rep(title_trace, length(x_s))))
  }

  # Check for mixtures and separate data regarding results from segmented
  # regression:
  if (!is.null(mix_output) && !("em_results" %in%  names(mix_output))) {

    # Defining subset function for x_ranges provided by mixmod_regression():
    subset_x <- function(x, mrr_model) {
      subset(x, x >= mrr_model$x_range[[1]] & x <= mrr_model$x_range[[2]])
    }

    if (exists("mod_3", where = mix_output)) {
      x_1 <- subset_x(x = x_s, mrr_model = mix_output$mod_1)
      x_2 <- subset_x(x = x_s, mrr_model = mix_output$mod_2)
      x_3 <- subset_x(x = x_s, mrr_model = mix_output$mod_3)

      group_df$groups <- as.factor(c(rep(paste(title_trace, 1), length(x_1)),
                                     rep(paste(title_trace, 2), length(x_2)),
                                     rep(paste(title_trace, 3), length(x_3))))
    } else if (exists("mod_2", where = mix_output)) {
      x_1 <- subset_x(x = x_s, mrr_model = mix_output$mod_1)
      x_2 <- subset_x(x = x_s, mrr_model = mix_output$mod_2)
      group_df$groups <- as.factor(c(rep(paste(title_trace, 1), length(x_1)),
                                     rep(paste(title_trace, 2), length(x_2))))
    } else {
      group_df$groups <- as.factor(c(rep(title_trace, length(x_s))))
    }
  }

  if (("em_results" %in%  names(mix_output))) {
    # Split observations by maximum a-posteriori method (MAP) used in mixmod_em:
    groups <- mix_output$em_results$groups
    x_split <- split(x, groups, lex.order = T)
    ev_split <- split(event, groups, lex.order = T)
    id_split <- split(id, groups, lex.order = T)

    # Apply johnson_method() for splitted observations:
    john_lst <- mapply(x_split, ev_split, id_split,
                       FUN = function(x, d, id) johnson_method(x = x, event = d, id = id),
                       SIMPLIFY = FALSE)

    # Store dataframes in one dataframe:
    group_df <- do.call("rbind", john_lst)

    # add group names using row.names() if splitted groups exist. Otherwise use
    # title_trace:

    if (length(john_lst) == 1) {
      group_df$groups <- as.factor(title_trace)
    } else {
      group_df$groups <- as.factor(paste(title_trace,
                                         floor(as.numeric(row.names(group_df)))))
    }

    # Preparation for plot:
    group_df <- group_df %>%
      dplyr::filter(status == 1) %>%
      dplyr::rename(id_s = id, x_s = characteristic, y_s = prob)
  }

  # Choice of distribution:
  if (distribution == "weibull") {
    q <- SPREDA::qsev(group_df$y_s)
  } else if (distribution == "lognormal") {
    q <- stats::qnorm(group_df$y_s)
  } else if (distribution == "loglogistic") {
    q <- stats::qlogis(group_df$y_s)
  }
  group_df$q <- q

  return(group_df)
}

plot_mod_helper <- function(
  x, y, log_sc_params, distribution
) {
  if (is.null(y)) {
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    x_low <- x_min - 10 ^ floor(log10(x_min)) * .25
    x_high <- x_max + 10 ^ floor(log10(x_max)) * .25

    x_p <- seq(x_low, x_high, length.out = 200)
    y_p <- predict_prob(
      q = x_p,
      loc_sc_params = loc_sc_params,
      distribution = distribution
    )
  } else {
    x_p <- x
    y_p <- y
  }

  df_pred <- data.frame(x_p = x_p, y_p = y_p)

  if (distribution %in% c("weibull", "weibull3", "sev")) {
    q <- SPREDA::qsev(y_p)

    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "weibull") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
                     round(1 / loc_sc_params[[2]], digits = 2))
      param_label <- c("\u03B7:", "\u03B2:")
    }
    if (distribution == "weibull3") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
                     round(1 / loc_sc_params[[2]], digits = 2), round(loc_sc_params[[3]], digits = 2))
      param_label <- c("\u03B7:", "\u03B2:", "\u03B3:")
    }

  }

  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    q <- stats::qnorm(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "lognormal3") {
      param_val <- c(param_val, round(loc_sc_params[[3]], digits = 2))
      param_label <- c(param_label, "\u03B3:")
    }
  }

  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    q <- stats::qlogis(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("\u03BC:", "\u03C3:")

    if (distribution == "loglogistic3") {
      param_val <- c(param_val, round(loc_sc_params[[3]], digits = 2))
      param_label <- c(param_label, "\u03B3:")
    }
  }

  df_pred$q <- q

  list(
    df_pred = df_pred,
    param_val = param_val,
    param_label = param_label
  )
}
