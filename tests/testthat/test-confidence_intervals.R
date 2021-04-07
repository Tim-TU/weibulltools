test_that("confint_betabinom remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## Probability tbl:
  prob_tbl <- estimate_cdf(x = data, methods = "johnson")

  ## Distributions without threshold:
  dists <- c(
    "weibull", "lognormal", "loglogistic",
    "sev", "normal", "logistic"
  )

  ## Rank Regression:
  rr <- purrr::map(dists, rank_regression, x = prob_tbl)

  ## Direction 'y':
  ci <- purrr::map(
    rr,
    confint_betabinom,
    direction = "y"
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- purrr::map(
    rr,
    confint_betabinom,
    direction = "x"
  )

  expect_snapshot_output(ci)

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## Probability tbl:
  prob_tbl <- estimate_cdf(x = data, methods = "johnson")

  ## Distributions with threshold:
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  ## Rank Regression:
  rr <- purrr::map(dists, rank_regression, x = prob_tbl)

  ## Direction 'y':
  ci <- purrr::map(
    rr,
    confint_betabinom,
    direction = "y"
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- purrr::map(
    rr,
    confint_betabinom,
    direction = "x"
  )

  expect_snapshot_output(ci)
})

test_that("confint_fisher remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## Distributions without threshold:
  dists <- c(
    "weibull", "lognormal", "loglogistic",
    "sev", "normal", "logistic"
  )

  ## ML estimation:
  ml <- purrr::map(dists, ml_estimation, x = data, conf_level = 0.90)

  ## Direction 'y':
  ci <- purrr::map(
    ml,
    confint_fisher
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- purrr::map(
    ml,
    confint_fisher,
    direction = "x"
  )

  expect_snapshot_output(ci)

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## Distributions with threshold:
  dists <- c(
    "weibull3", "lognormal3", "loglogistic3"
  )

  ## ML estimation:
  ml <- purrr::map(dists, ml_estimation, x = data, conf_level = 0.90)

  ## Direction 'y':
  ci <- purrr::map(
    ml,
    confint_fisher
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- purrr::map(
    ml,
    confint_fisher,
    direction = "x"
  )

  expect_snapshot_output(ci)
})
