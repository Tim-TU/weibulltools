test_that("r_squared_profiling remains stable", {
  cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
                224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
                180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
                159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
                139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
                112, 108, 104, 99, 99, 96, 94)
  status <- c(rep(0, 5), rep(1, 67))
  data <- reliability_data(x = cycles, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  threshold <- seq(0, min(cycles[status == 1]) - 0.1, length.out = 100)

  profile_r2 <- sapply(
    threshold, r_squared_profiling.default,
    x = tbl_john$x[tbl_john$status == 1],
    y = tbl_john$prob[tbl_john$status == 1],
    distribution = "weibull3"
  )

  expect_snapshot_output(profile_r2)
})

test_that("rank_regression remains stable", {
  obs   <- seq(10000, 100000, 10000)
  status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  data <- reliability_data(x = obs, status = status)

  tbl_john <- estimate_cdf(data, "johnson")

  mrr <- rank_regression(
    tbl_john,
    distribution = "weibull",
    conf_level = .90
  )

  expect_snapshot_output(mrr$loc_sc_params)
  expect_snapshot_output(mrr$r_squared)
  expect_snapshot_output(mrr)

  cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
                224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
                180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
                159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
                139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
                112, 108, 104, 99, 99, 96, 94)
  status <- c(rep(0, 5), rep(1, 67))

  data <- reliability_data(x = cycles, status = status)

  tbl_john <- estimate_cdf(data, "johnson")
  mrr <- rank_regression(
    tbl_john,
    distribution = "weibull3",
    conf_level = .90
  )

  expect_snapshot_output(mrr$loc_sc_params)
  expect_snapshot_output(mrr$r_squared)
  expect_snapshot_output(mrr)
})

test_that("rank_regression supports multiple methods", {
  data <- reliability_data(shock, x = "distance", status = "status")

  methods <- c("johnson", "nelson", "kaplan")

  cdf_tbl <- estimate_cdf(data, methods)

  mrr <- rank_regression.cdf_estimation(
    x = cdf_tbl,
    distribution = "weibull"
  )

  expect_equal(length(mrr), 3)
  expect_true(all(methods %in% names(mrr)))
})
