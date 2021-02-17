test_that("incompatible distributions are caught", {
  rel_tbl <- reliability_data(shock, distance, status)
  cdf_tbl <- estimate_cdf(rel_tbl, "johnson")
  plot_weib <- plot_prob(cdf_tbl, "logistic")
  models <- get_models()

  purrr::walk(models, function(model) {
    expect_error(
      plot_mod(plot_weib, model), class = "incompatible_distributions"
    )
    expect_error(
      plot_conf(plot_weib, model), class = "incompatible_distributions"
    )
  })
})
