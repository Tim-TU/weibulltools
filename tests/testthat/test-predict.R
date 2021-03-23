test_that("predict_quantile remains stable", {
  ## Distributions without threshold:
  dists <- c(
    "weibull", "lognormal", "loglogistic",
    "sev", "normal", "logistic"
  )

  q_p <- purrr::map(dists,
                    predict_quantile,
                    p = c(0.01, 0.1, 0.5),
                    dist_params =  c(5, 0.5)
  )

  expect_snapshot_output(q_p)

  ## Distributions with threshold:
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  q_p <- purrr::map(dists,
                    predict_quantile,
                    p = c(0.01, 0.1, 0.5),
                    dist_params =  c(5, 0.5, 10)
  )

  expect_snapshot_output(q_p)
})



test_that("predict_prob remains stable", {
  ## Distributions without threshold:
  dists <- c(
    "weibull", "lognormal", "loglogistic",
    "sev", "normal", "logistic"
  )

  p_q <- purrr::map(dists,
                    predict_prob,
                    q = c(2, 5, 10),
                    dist_params =  c(5, 1)
  )

  expect_snapshot_output(p_q)

  ## Distributions with threshold:
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  p_q <- purrr::map(dists,
                    predict_prob,
                    q = c(25, 58, 134),
                    dist_params =  c(5, 0.5, 10)
  )

  expect_snapshot_output(p_q)
})
