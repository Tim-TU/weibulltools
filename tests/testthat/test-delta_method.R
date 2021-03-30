test_that("delta_method remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## Distributions without threshold:
  dists <- c(
    "weibull", "lognormal", "loglogistic",
    "sev", "normal", "logistic", "exponential"
  )

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)
  dist_params <- purrr::map(ml, "coefficients")
  dist_varcov <- purrr::map(ml, "varcov")

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

  ## Distributions with threshold:
  dists <- c("weibull3", "lognormal3", "loglogistic3", "exponential2")

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)
  dist_params <- purrr::map(ml, "coefficients")
  dist_varcov <- purrr::map(ml, "varcov")

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
