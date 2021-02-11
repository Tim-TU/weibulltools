test_that("r_squared_profiling remains stable", {
  cycles <- alloy$cycles
  status <- alloy$status
  data <- reliability_data(x = cycles, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  threshold <- seq(0, min(cycles[status == 1]) - 0.1, length.out = 100)

  profile_r2 <- r_squared_profiling.default(
    x = tbl_john$x[tbl_john$status == 1],
    y = tbl_john$prob[tbl_john$status == 1],
    thres = threshold,
    distribution = "weibull3"
  )

  expect_snapshot_output(profile_r2)
})

test_that("rank_regression remains stable", {
  dists <- c("weibull", "lognormal", "loglogistic",
             "normal", "logistic", "sev",
             "weibull3", "lognormal3", "loglogistic3")

  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  rr <- lapply(dists, rank_regression, x = tbl_john, conf_level = 0.90)

  expect_snapshot_output(lapply(rr, "[[", "coefficients"))
  expect_snapshot_output(lapply(rr, "[[", "confint"))
  expect_snapshot_output(lapply(rr, "[[", "r_squared"))

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  rr <- lapply(dists, rank_regression, x = tbl_john, conf_level = 0.95)

  expect_snapshot_output(lapply(rr, "[[", "coefficients"))
  expect_snapshot_output(lapply(rr, "[[", "confint"))
  expect_snapshot_output(lapply(rr, "[[", "r_squared"))

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
