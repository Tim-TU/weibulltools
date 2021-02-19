add_b_lives <- function(x,
                        dist_params,
                        distribution,
                        b_lives
) {

  # Range of failed items:
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_seq <- seq(x_min, x_max, length.out = 100)

  # Range of probabilities calculated with estimated regression line:
  y_seq <- predict_prob(q = x_seq, dist_params = dist_params,
                        distribution = distribution)

  # Looking for B lives in range of estimated ones:
  b_lives_present <- b_lives[b_lives >= y_seq[1] & b_lives <= y_seq[length(y_seq)]]

  # Add them:
  y_seq <- sort(unique(c(y_seq, b_lives_present)))
  x_seq <- predict_quantile(
    p = y_seq,
    dist_params = dist_params,
    distribution = distribution
  )

  list(
    x_seq = x_seq,
    y_seq = y_seq
  )
}
