test_that("mixmod_regression remains stable", {
  hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
             191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
             320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
             13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
             226, 278, 314, 328, 377)

  status <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
              1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
              1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              0, 1, 1, 1, 1, 1, 1)

  data <- reliability_data(x = hours, status = status)

  tbl_john <- estimate_cdf(data, methods = "johnson")

  mix_mod <- mixmod_regression(tbl_john, distribution = "weibull")

  expect_snapshot_output(mix_mod)
})