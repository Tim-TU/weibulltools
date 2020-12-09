test_that("mixmod_regression remains stable", {
  data <- reliability_data(data = voltage, x = hours, status = status)

  tbl_john <- estimate_cdf(data, methods = "johnson")

  mix_mod <- suppressMessages(
    mixmod_regression(tbl_john, distribution = "weibull")
  )
  expect_snapshot_output(mix_mod)
})

test_that("mixmod_em remains stable", {
  # Data is taken from given reference of Doganaksoy, Hahn and Meeker:
  set.seed(1)

  hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
             191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
             320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
             13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
             226, 278, 314, 328, 377)
  state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
             1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
             1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             0, 1, 1, 1, 1, 1, 1)

  mix_mod_em <- mixmod_em(
    x = hours,
    status = state,
    distribution = "weibull",
    conf_level = 0.95,
    k = 2,
    method = "EM",
    n_iter = 150
  )

  expect_snapshot_output(mix_mod_em$mod_1)
  expect_snapshot_output(mix_mod_em$mod_2)

})
