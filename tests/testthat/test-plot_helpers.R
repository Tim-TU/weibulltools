# plot_layout_helper ----
test_that("plot_layout_helper remains stable", {
  x_layout <- seq(1e-5, 1e+07, length.out = 10)
  helper <- plot_layout_helper(
    x = x_layout,
    distribution = "weibull",
    plot_method = "plotly"
  )

  expect_snapshot_output(helper)

  x_layout <- seq(1, 10, length.out = 10)
  helper_2 <- plot_layout_helper(
    x = x_layout,
    distribution = "normal",
    plot_method = "plotly"
  )

  expect_snapshot_output(helper_2)
})

# plot_prob_helper ----
test_that("plot_prob_helper remains stable", {
  cycles   <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
                224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
                180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
                159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
                139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
                112, 108, 104, 99, 99, 96, 94)
  status <- c(rep(0, 5), rep(1, 67))
  data <- reliability_data(x = cycles, status = status)

  tbl_john <- estimate_cdf(data, methods = c("johnson", "kaplan"))

  helper <- plot_prob_helper(
    tbl_john,
    distribution = "weibull"
  )

  helper_2 <- plot_prob_helper(
    tbl_john,
    distribution = "lognormal"
  )

  expect_snapshot_output(helper)
  expect_snapshot_output(helper_2)
})

# plot_mod_helper ----
test_that("plot_mod_helper remains stable", {

})

# plot_mod_mix_helper ----
test_that("plot_mod_mix_helper remains stable", {

})

# plot_pop_helper ----
test_that("plot_pop_helper remains stable", {
  suppressWarnings(library(tibble))
  set.seed(1)
  x <- rweibull(n = 100, shape = 1, scale = 20000)
  loc_sc_params_tbl <- tibble(loc = log(20000), sc = 1, thres = NA)
  expect_snapshot_output(
    plot_pop_helper(x, loc_sc_params_tbl, "weibull")
  )
})
