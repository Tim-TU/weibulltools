# Function that adds B_p-lives for p's covered by the range of probabilities:
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

  # Looking for B_p-lives in range of estimated ones:
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

# BB Bounds for probabilities (direction = "y"):
conf_bb_y <- function(y, # sequence of probabilities returned by 'add_b_lives()'.
                      n, # sample size of all units (n_f + n_s).
                      r, # interpolated ranks computed with Benards formula.
                      bounds,
                      conf_level
) {

  r_inv <- n - r + 1

  # Confidence intervals:
  p_conf <- switch(
    bounds,
    "two_sided" = c(lower_bound = (1 - conf_level),
                    upper_bound = (1 + conf_level)) / 2,
    "lower" = c(lower_bound = 1 - conf_level),
    "upper" = c(upper_bound = conf_level)
  )

  list_confint <- purrr::map(
    p_conf,
    stats::qbeta,
    shape1 = r,
    shape2 = r_inv
  )

  names(list_confint) <- names(p_conf)

  list_confint
}
