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
