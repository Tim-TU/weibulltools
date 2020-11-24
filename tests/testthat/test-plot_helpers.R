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

# plot_prob_mix_helper ----
test_that("plot_prob_mix_helper remains stable", {
  set.seed(1)

  # Data is taken from given reference:
  hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
             191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
             320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
             13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
             226, 278, 314, 328, 377)
  state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
             1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
             1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             0, 1, 1, 1, 1, 1, 1)
  id <- 1:length(hours)

  data <- reliability_data(x = hours, status = state, id = id)

  # Example 2 - Using result of mixmod_em in mix_output:
  mix_mod_em <- mixmod_em(
    data,
    distribution = "weibull",
    conf_level = 0.95,
    k = 2,
    method = "EM",
    n_iter = 150
  )

  helper_2 <- plot_prob_mix_helper(
    data,
    distribution = "weibull",
    mix_output = mix_mod_em,
    title_trace = "Subgroup"
  )

  expect_snapshot_output(helper_2)

  # Example 3 - Using result of mixmod_regression in mix_output:
  tbl_john <- estimate_cdf(data, methods = "johnson")

  mix_mod_reg <- mixmod_regression(
    tbl_john,
    distribution = "weibull"
  )

  helper_3 <- plot_prob_mix_helper(
    data,
    distribution = "weibull",
    mix_output = mix_mod_reg,
    title_trace = "Subgroup"
  )

  expect_snapshot_output(helper_3)
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
