test_that("estimate_cdf fails for non reliability data", {
  tbl <- tibble::tibble(x = 1:10)
  expect_error(
    estimate_cdf(tbl, methods = "mr"),
    "data must be a tibble returned from reliability_data()"
  )
})

obs <- seq(10000, 100000, 10000)

test_that("mr_method warns for right-censored data", {
  expect_message(
    mr_method(obs, rep(0, times = length(obs))),
    "The mr method only considers.*"
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

test_that("snapshots: examples", {
  obs   <- seq(10000, 100000, 10000)
  state <- rep(1, length(obs))
  uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
             "fl29", "AX23", "Uy12", "kl1a")

  expect_snapshot_output(mr_method(x = obs, event = state, id = uic, method = "benard"))
  expect_snapshot_output(mr_method(x = obs, event = state, id = uic, method = "invbeta"))

  state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)

  expect_snapshot_output(johnson_method(x = obs, event = state, id = uic))
  expect_snapshot_output(nelson_method(x = obs, event = state, id = uic))
  expect_snapshot_output(kaplan_method(x = obs, event = state, id = uic))

  tbl <- tibble(
    obs = c(
      10000, 10000, 20000, 20000, 30000, 30000, 30000, 30000, 40000, 50000,
      50000, 60000, 70000, 70000, 70000, 70000, 80000, 80000, 80000, 80000,
      90000, 90000, 100000
    ),
    state = rep(1, 23)
  )

  expect_snapshot_output(
    suppressWarnings(kaplan_method(x = tbl$obs, event = tbl$state))
  )
})

test_that("snapshots: input with repeating characteristics", {
  x <- rep(1, 4)
  event <- c(0, 1, 0, 1)

  expect_snapshot_output(suppressMessages(mr_method(x, event)))
  expect_snapshot_output(johnson_method(x, event))
  expect_snapshot_output(kaplan_method(x, event))
  expect_snapshot_output(nelson_method(x, event))
})
