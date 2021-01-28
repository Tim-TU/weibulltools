test_that("mr_method_ warns for right-censored data", {
  obs <- seq(10000, 100000, 10000)

  tbl <- tibble(x = obs, status = 0, id = "X")

  expect_message(
    mr_method_(tbl),
    "The 'mr' method only considers.*"
  )
})

test_that("kaplan_method and nelson_method warn for uncensored data", {
  obs <- seq(10000, 100000, 10000)

  tbl <- tibble(x = obs, status = 1, id = "X")

  expect_warning(
    kaplan_method_(tbl),
    'Use methods = "mr".*'
  )

  expect_warning(
    nelson_method_(tbl),
    'Use methods = "mr".*'
  )
})

test_that("snapshots: examples", {
  obs   <- seq(10000, 100000, 10000)
  state <- rep(1, length(obs))
  uic   <- c("3435", "1203", "958X", "XX71", "abcd", "tz46",
             "fl29", "AX23", "Uy12", "kl1a")

  tbl <- tibble(x = obs, status = state, id = uic)

  expect_snapshot_output(mr_method_(tbl, method = "benard"))
  expect_snapshot_output(mr_method_(tbl, method = "invbeta"))

  tbl$status <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)

  expect_snapshot_output(johnson_method_(tbl))
  expect_snapshot_output(nelson_method_(tbl))
  expect_snapshot_output(kaplan_method_(tbl))

  tbl <- tibble(
    x = c(
      10000, 10000, 20000, 20000, 30000, 30000, 30000, 30000, 40000, 50000,
      50000, 60000, 70000, 70000, 70000, 70000, 80000, 80000, 80000, 80000,
      90000, 90000, 100000
    ),
    status = rep(1, 23),
    id = "X"
  )

  expect_snapshot_output(
    suppressWarnings(kaplan_method_(tbl))
  )
})

test_that("snapshots: input with repeating characteristics", {
  x <- rep(1, 4)
  status <- c(0, 1, 0, 1)

  tbl <- tibble(x = x, status = status, id = "X")

  expect_snapshot_output(suppressMessages(mr_method_(tbl)))
  expect_snapshot_output(johnson_method_(tbl))
  expect_snapshot_output(kaplan_method_(tbl))
  expect_snapshot_output(nelson_method_(tbl))
})

test_that(".keep_all keeps additional columns", {
  rel_tbl <- reliability_data(shock, distance, status, .keep_all = TRUE)
  mr <- suppressMessages(mr_method_(rel_tbl))
  johnson <- johnson_method_(rel_tbl)
  kaplan <- kaplan_method_(rel_tbl)
  nelson <- nelson_method_(rel_tbl)

  expect_snapshot_output(mr)
  expect_snapshot_output(johnson)
  expect_snapshot_output(kaplan)
  expect_snapshot_output(nelson)

  expect_true(hasName(mr, "failure_mode"))
  expect_true(hasName(johnson, "failure_mode"))
  expect_true(hasName(kaplan, "failure_mode"))
  expect_true(hasName(nelson, "failure_mode"))
})

options(testthat.progress.max_fails = 20)
