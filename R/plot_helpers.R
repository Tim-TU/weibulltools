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
    x = x_s,
    y = y_s,
    q = q,
    id = id_s
  )
}
