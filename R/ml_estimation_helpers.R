# Function that returns starting values for log-likelihood optimization:
start_dist_params <- function(x,
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
  )$coefficients

  # Use log scale parameter for convenience and stability:
 c(rr[1], log(rr[2]))
}
