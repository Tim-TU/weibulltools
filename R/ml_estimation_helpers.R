# Function that returns starting values for log-likelihood optimization:
start_params <- function(x,
                         status,
                         distribution
) {

  # Kaplan-Meier probability estimation using midpoints for probabilities:
  km <- estimate_cdf(
    x = x,
    status = status,
    method = "kaplan"
  ) %>%
    dplyr::filter(.data$status == 1)

  x_init <- km$x
  y_init <- 0.5 * (c(0, dplyr::lag(km$prob)[-1]) + km$prob)

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

  conf_mu <- dist_params[["mu"]] + q_n * dist_se[["mu"]]

  w <- exp(q_n[2] * dist_se[["sigma"]] / dist_params[["sigma"]])
  conf_sigma <- dist_params[["sigma"]] * c(1 / w, w)

  # Form confidence interval matrix:
  conf_int <- matrix(c(conf_mu, conf_sigma), byrow = TRUE, ncol = 2)
  colnames(conf_int) <- paste(p_conf * 100, "%")

  if (length(dist_params) == 3L) {
    conf_gamma <- dist_params[["gamma"]] + q_n * dist_se[["gamma"]]
    conf_int <- rbind(conf_int, conf_gamma)
  }

  rownames(conf_int) <- names(dist_params)

  # Return confidence interval:
  conf_int
}
