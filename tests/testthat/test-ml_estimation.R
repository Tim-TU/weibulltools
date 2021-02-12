test_that("loglik_profiling remains stable", {
  cycles <- alloy$cycles
  status <- alloy$status

  threshold <- seq(0, min(cycles[status == 1]) - 0.1, length.out = 100)
  profile_logL <- loglik_profiling(
    x = cycles,
    status = status,
    thres = threshold,
    distribution = "weibull3"
  )

  expect_snapshot_output(profile_logL)
})

test_that("ml_estimation remains stable", {
  dists <- c("weibull", "lognormal", "loglogistic",
             "normal", "logistic", "sev",
             "weibull3", "lognormal3", "loglogistic3")

  # Test with 'shock':
  data <- reliability_data(data = shock, x = distance, status = status)

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.90)

  expect_snapshot_output(lapply(ml, "[[", "coefficients"))
  expect_snapshot_output(lapply(ml, "[[", "varcov"))
  expect_snapshot_output(lapply(ml, "[[", "logL"))

  # Test with 'alloy':
  data <- reliability_data(data = alloy, x = cycles, status = status)

  ml <- lapply(dists, ml_estimation, x = data, conf_level = 0.95)

  expect_snapshot_output(lapply(ml, "[[", "coefficients"))
  expect_snapshot_output(lapply(ml, "[[", "varcov"))
  expect_snapshot_output(lapply(ml, "[[", "logL"))
})
