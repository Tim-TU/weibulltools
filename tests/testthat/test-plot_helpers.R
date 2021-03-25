# plot_layout_helper ----
test_that("plot_layout_helper remains stable", {
  x_layout <- seq(1e-5, 1e+07, length.out = 10)
  helper <- plot_layout_helper(
    x = x_layout,
    distribution = "weibull"
  )

  expect_snapshot_output(helper)

  x_layout <- seq(1, 10, length.out = 10)
  helper_2 <- plot_layout_helper(
    x = x_layout,
    distribution = "normal"
  )

  expect_snapshot_output(helper_2)
})

# plot_prob_helper ----
test_that("plot_prob_helper remains stable", {
  data <- reliability_data(data = alloy, x = cycles, status = status)

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
  data <- reliability_data(shock, x = distance, status = status)

  cdf <- estimate_cdf(data, "johnson")

  rr <- rank_regression(cdf)

  helper <- plot_mod_helper(
    x = range(rr$data$x),
    dist_params = rr$coefficients,
    distribution = "weibull"
  )

  expect_snapshot_output(helper)
})

# plot_mod_mix_helper ----
test_that("plot_mod_mix_helper remains stable", {
  data <- reliability_data(shock, x = distance, status = status)

  cdf <- estimate_cdf(data, "johnson")

  rr <- rank_regression(cdf)

  helper <- plot_mod_mix_helper(
    model_estimation = rr,
    cdf_estimation_method = "johnson",
    group = "group"
  )

  expect_snapshot_output(helper)
})

# plot_conf_helper ----
test_that("plot_conf_helper_2 remains stable", {
  data <- reliability_data(shock, x = distance, status = status)

  cdf <- estimate_cdf(data, "johnson")

  rr <- rank_regression(cdf)

  confint <- confint_betabinom(rr)

  helper <- plot_conf_helper_2(
    confint
  )

  expect_snapshot_output(helper)
})

# plot_pop_helper ----
test_that("plot_pop_helper remains stable", {
  suppressWarnings(library(tibble))
  set.seed(1)
  x <- rweibull(n = 100, shape = 1, scale = 20000)
  dist_params_tbl <- tibble(loc = log(20000), sc = 1, thres = NA)
  expect_snapshot_output(
    plot_pop_helper(x, dist_params_tbl, "weibull")
  )
})
