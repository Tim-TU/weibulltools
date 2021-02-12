# Log-likelihood function:
test_that("loglik_function remains stable", {
  # Test with 'shock':
  x <- shock$distance
  status <- shock$status

  ## log-location scale distributions (two-parametric):
  dists <- c("weibull", "lognormal", "loglogistic")

  logL <- lapply(
    dists,
    loglik_function,
    x = x,
    status = status,
    wts = rep(1, length(x)),
    dist_params = c(10.23, 0.35)
  )

  expect_snapshot_output(logL)

  ## location-scale distributions (two-parametric):
  dists <- c("sev", "normal", "logistic")

  logL <- lapply(
    dists,
    loglik_function,
    x = x,
    status = status,
    wts = rep(1, length(x)),
    dist_params = c(25000, 8500)
  )

  expect_snapshot_output(logL)

  # Test with 'alloy':
  x <- alloy$cycles
  status <- alloy$status

  ## log-location scale distributions (three-parametric):
  dists <- c("weibull3", "lognormal3", "loglogistic3")

  logL <- lapply(
    dists,
    loglik_function,
    x = x,
    status = status,
    wts = rep(1, length(x)),
    dist_params = c(4.54, 0.76, 93)
  )

  expect_snapshot_output(logL)
})



# Log-likelihood profile function:
test_that("loglik_profiling remains stable", {
  cycles <- alloy$cycles
  status <- alloy$status

  threshold <- seq(0, min(cycles[status == 1]) - 0.1, length.out = 100)
  profile_logL <- loglik_profiling(
    x = cycles,
    status = status,
    thres = threshold,
    distribution = "weibull3"
  )

  expect_snapshot_output(profile_logL)
})
