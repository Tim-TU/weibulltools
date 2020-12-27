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

  hours <- voltage$hours
  status <- voltage$status

  mix_mod_em <- mixmod_em(
    x = hours,
    status = status,
    distribution = "weibull",
    conf_level = 0.95,
    k = 2,
    method = "EM",
    n_iter = 150
  )

  expect_snapshot_output(mix_mod_em$mod_1)
  expect_snapshot_output(mix_mod_em$mod_2)

})
