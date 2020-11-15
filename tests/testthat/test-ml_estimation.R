test_that("ml_estimation remains stable", {
  # Example 1: Fitting a two-parameter Weibull:
  obs   <- seq(10000, 100000, 10000)
  status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
  reliability_tbl <- reliability_data(x = obs, status = status)

  mle <- ml_estimation(
    reliability_tbl,
    distribution = "weibull",
    conf_level = 0.90
  )

  expect_snapshot_output(mle$loc_sc_params)
  expect_snapshot_output(mle$loc_sc_varcov)
  expect_snapshot_output(mle)

  # Example 2: Fitting a three-parameter Weibull:
  # Alloy T7987 dataset taken from Meeker and Escobar(1998, p. 131)
  cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
                224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
                180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
                159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
                139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
                112, 108, 104, 99, 99, 96, 94)
  status <- c(rep(0, 5), rep(1, 67))
  reliability_tbl_2 <- reliability_data(x = cycles, status = status)

  mle_weib3 <- ml_estimation(
    reliability_tbl,
    distribution = "weibull3",
    conf_level = 0.95
  )

  expect_snapshot_output(mle_weib3$loc_sc_params)
  expect_snapshot_output(mle_weib3$loc_sc_varcov)
  expect_snapshot_output(mle_weib3)
})
