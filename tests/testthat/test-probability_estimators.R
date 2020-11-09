obs <- seq(10000, 100000, 10000)

test_that("mr_method fails for right-censored data", {
  expect_error(
    mr_method(obs, rep(0, times = length(obs))),
    "Use johnson_method, kaplan_method or nelson_method.*"
  )
})

test_that("kaplan_method and nelson_method warn for uncensored data", {
  expect_warning(
    kaplan_method(obs, rep(1, times = length(obs))),
    "Use mr_method.*"
  )

  expect_warning(
    nelson_method(obs, rep(1, times = length(obs))),
    "Use mr_method.*"
  )
})

test_that("all methods fail for vectors of different length", {
  msg <- "x, event and id must be of same length."

  expect_error(mr_method(obs, 0), msg)
  expect_error(johnson_method(obs, 0), msg)
  expect_error(kaplan_method(obs, 0), msg)
  expect_error(nelson_method(obs, 0), msg)
})

test_that("all methods return cdf_estimations", {
  expect_s3_class(mr_method(obs, rep(1, times = length(obs))), "cdf_estimation")
  expect_s3_class(johnson_method(obs, rep(0, times = length(obs))), "cdf_estimation")
  expect_s3_class(kaplan_method(obs, rep(0, times = length(obs))), "cdf_estimation")
  expect_s3_class(nelson_method(obs, rep(0, times = length(obs))), "cdf_estimation")
})
