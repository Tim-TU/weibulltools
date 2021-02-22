test_that("confint_betabinom remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## Probability tbl:
  prob_tbl <- estimate_cdf(x = data, methods = "johnson")

  ## two-parametric:
  dists <- c(
    "weibull", "lognormal", "loglogistic", "sev", "normal", "logistic"
  )

  ## Rank Regression:
  rr <- lapply(dists, rank_regression, x = prob_tbl)

  ## Direction 'y':
  ci <- lapply(
    rr,
    confint_betabinom,
    direction = "y"
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- lapply(
    rr,
    confint_betabinom,
    direction = "x"
  )

  expect_snapshot_output(ci)

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## Probability tbl:
  prob_tbl <- estimate_cdf(x = data, methods = "johnson")

  ## three-parametric:
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  ## Rank Regression:
  rr <- lapply(dists, rank_regression, x = prob_tbl)

  ## Direction 'y':
  ci <- lapply(
    rr,
    confint_betabinom,
    direction = "y"
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- lapply(
    rr,
    confint_betabinom,
    direction = "x"
  )

  expect_snapshot_output(ci)
})

test_that("confint_fisher remains stable", {
  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ## two-parametric:
  dists <- c(
    "weibull", "lognormal", "loglogistic", "sev", "normal", "logistic"
  )

  ## ML estimation:
  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)

  ## Direction 'y':
  ci <- lapply(
    ml,
    confint_fisher
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- lapply(
    ml,
    confint_fisher,
    direction = "x"
  )

  expect_snapshot_output(ci)

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ## three-parametric:
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  ## ML estimation:
  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)

  ## Direction 'y':
  ci <- lapply(
    ml,
    confint_fisher
  )

  expect_snapshot_output(ci)

  ## Direction 'x':
  ci <- lapply(
    ml,
    confint_fisher,
    direction = "x"
  )

  expect_snapshot_output(ci)
})
