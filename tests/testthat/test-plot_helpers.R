# plot_pop_helper ----
test_that("plot_pop_helper snapshot", {
  suppressWarnings(library(tibble))
  set.seed(1)
  x <- rweibull(n = 100, shape = 1, scale = 20000)
  param_tbl <- tibble(param_1 = 20000, param_2 = 1)
  expect_snapshot_output(
    plot_pop_helper(x, param_tbl, "weibull")
  )
})
