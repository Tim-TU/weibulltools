# Function that returns starting values for log-likelihood optimization:
start_params <- function(x,
                         status,
                         distribution
) {

  # Probability estimation method w.r.t status:
  method <- if(all(status == 1)) {
    "mr"
  } else {
    "kaplan"
  }

  # Probability estimation using midpoints for probabilities:
  pp <- estimate_cdf(
    x = x,
    status = status,
    method = method
  ) %>%
    dplyr::filter(.data$status == 1)

  x_init <- pp$x
  y_init <- 0.5 * (c(0, dplyr::lag(pp$prob)[-1]) + pp$prob)

  # Initial parameters, i.e. RR parameters:
  rr <- lm_(
    x = x_init,
    y = y_init,
    distribution = distribution
  )

  rr$coefficients
}



# Normal approximation confidence intervals for parameters:
conf_normal_approx <- function(dist_params,
                               dist_varcov,
                               conf_level
) {

  # Standard errors:
  dist_se <- sqrt(diag(dist_varcov))

  # Normal confidence intervals:
  p_conf <- c((1 - conf_level), (1 + conf_level)) / 2
  q_n <- stats::qnorm(p = p_conf)

  # Compute confidence intervals:
  ## Scale parameter is part of all distributions:
  w <- exp(q_n[2] * dist_se[["sigma"]] / dist_params[["sigma"]])
  conf_sigma <- dist_params[["sigma"]] * c(1 / w, w)

  ## Location parameter is not present if distribution is 'exponential':
  if ("mu" %in% names(dist_params)) {
    conf_mu <- dist_params[["mu"]] + q_n * dist_se[["mu"]]
  } else {
    conf_mu <- NULL
  }

  ## Threshold parameter:
  if ("gamma" %in% names(dist_params)) {
    conf_gamma <- dist_params[["gamma"]] + q_n * dist_se[["gamma"]]
  } else {
    conf_gamma <- NULL
  }

  # Form confidence interval matrix:
  conf_int <- matrix(c(conf_mu, conf_sigma, conf_gamma), byrow = TRUE, ncol = 2)
  colnames(conf_int) <- paste(p_conf * 100, "%")
  rownames(conf_int) <- names(dist_params)

  # Return confidence interval:
  conf_int
}
