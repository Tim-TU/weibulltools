test_that("delta_method remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## two-parametric:
  dists <- c(
    "weibull", "lognormal", "loglogistic", "sev", "normal", "logistic"
  )

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)
  dist_params <- lapply(ml, "[[", "coefficients")
  dist_varcov <- lapply(ml, "[[", "varcov")

  ## Direction 'y':
  dm <- purrr::pmap(
    list(dist_params, dist_varcov, dists),
    delta_method,
    x = data$x,
    direction = "y"
  )

  expect_snapshot_output(dm)

  ## Direction 'x':
  probs <- predict_prob(q = data$x, dist_params = dist_params[[1]])

  dm <- purrr::pmap(
    list(dist_params, dist_varcov, dists),
    delta_method,
    x = probs,
    direction = "x"
  )

  expect_snapshot_output(dm)

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## log-location scale distributions (three-parametric):
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)
  dist_params <- lapply(ml, "[[", "coefficients")
  dist_varcov <- lapply(ml, "[[", "varcov")

  ## Direction 'y':
  dm <- purrr::pmap(
    list(dist_params, dist_varcov, dists),
    delta_method,
    x = data$x,
    direction = "y"
  )

  expect_snapshot_output(dm)

  ## Direction 'x':
  probs <- predict_prob(q = data$x, dist_params = dist_params[[1]], "weibull3")

  dm <- purrr::pmap(
    list(dist_params, dist_varcov, dists),
    delta_method,
    x = probs,
    direction = "x"
  )

  expect_snapshot_output(dm)
})
