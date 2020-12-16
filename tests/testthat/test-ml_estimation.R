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
  # Example 1: Fitting a two-parameter Weibull:
  obs   <- seq(10000, 100000, 10000)
  status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  data <- reliability_data(x = obs, status = status)

  mle <- ml_estimation(
    data,
    distribution = "weibull",
    conf_level = 0.90
  )

  expect_snapshot_output(mle$coefficients)
  expect_snapshot_output(mle$loc_sc_varcov)
  expect_snapshot_output(mle)

  # Example 2: Fitting a three-parameter Weibull:
  # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
  cycles <- alloy$cycles
  status <- alloy$status
  data_2 <- reliability_data(x = cycles, status = status)

  mle_weib3 <- ml_estimation(
    data,
    distribution = "weibull3",
    conf_level = 0.95
  )

  expect_snapshot_output(mle_weib3$coefficients)
  expect_snapshot_output(mle_weib3$loc_sc_varcov)
  expect_snapshot_output(mle_weib3)
})
