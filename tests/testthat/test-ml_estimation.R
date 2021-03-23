test_that("ml_estimation remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## log-location scale distributions (two-parametric):
  dists <- c("weibull", "lognormal", "loglogistic")

  ml <- purrr::map(
    dists,
    ml_estimation,
    x = data,
    conf_level = 0.90
  )

  expect_snapshot_output(purrr::map(ml, "coefficients"))
  expect_snapshot_output(purrr::map(ml, "confint"))
  expect_snapshot_output(purrr::map(ml, "varcov"))
  expect_snapshot_output(purrr::map(ml, "logL"))

  ## location-scale or scale distributions:
  dists <- c("sev", "normal", "logistic", "exponential")

  ml <- purrr::map(
    dists,
    ml_estimation,
    x = data,
    conf_level = 0.95
  )

  expect_snapshot_output(purrr::map(ml, "coefficients"))
  expect_snapshot_output(purrr::map(ml, "confint"))
  expect_snapshot_output(purrr::map(ml, "varcov"))
  expect_snapshot_output(purrr::map(ml, "logL"))

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## log-location scale or scale distributions with threshold:
  dists <- c("weibull3", "lognormal3", "loglogistic3", "exponential2")

  ml <- purrr::map(
    dists,
    ml_estimation,
    x = data,
    conf_level = 0.99
  )

  expect_snapshot_output(purrr::map(ml, "coefficients"))
  expect_snapshot_output(purrr::map(ml, "confint"))
  expect_snapshot_output(purrr::map(ml, "varcov"))
  expect_snapshot_output(purrr::map(ml, "logL"))
})
