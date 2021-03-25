test_that("rank_regression remains stable", {
  dists <- c("weibull", "lognormal", "loglogistic",
             "normal", "logistic", "sev",
             "weibull3", "lognormal3", "loglogistic3",
             "exponential", "exponential2")

  direction <- c("x_on_y", "y_on_x")

  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  rr <- purrr::map2(
    rep(dists, length(direction)),
    rep(direction, each = length(dists)),
    rank_regression, x = tbl_john, conf_level = 0.90
  )

  expect_snapshot_output(purrr::map(rr, "coefficients"))
  expect_snapshot_output(purrr::map(rr, "confint"))
  expect_snapshot_output(purrr::map(rr, "r_squared"))

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  rr <- purrr::map2(
    rep(dists, length(direction)),
    rep(direction, each = length(dists)),
    rank_regression, x = tbl_john, conf_level = 0.95
  )

  expect_snapshot_output(purrr::map(rr, "coefficients"))
  expect_snapshot_output(purrr::map(rr, "confint"))
  expect_snapshot_output(purrr::map(rr, "r_squared"))

})

test_that("rank_regression supports multiple methods", {
  data <- reliability_data(shock, x = "distance", status = "status")

  methods <- c("johnson", "nelson", "kaplan")

  cdf_tbl <- estimate_cdf(data, methods)

  rr <- rank_regression(
    x = cdf_tbl,
    distribution = "weibull"
  )

  expect_equal(length(rr), 3)
  expect_true(all(methods %in% names(rr)))
})
