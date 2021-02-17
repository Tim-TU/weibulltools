# Log-likelihood function:
test_that("loglik_function remains stable", {
  ## vector-based:
  ### Test with 'shock':
  x <- shock$distance
  status <- shock$status

  ### log-location scale distributions (two-parametric):
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

  ## data-based:
  data <- reliability_data(data = shock, x = distance, status = status)
  logL_data <- lapply(
    dists,
    loglik_function,
    x = data,
    wts = rep(1, nrow(data)),
    dist_params = c(10.23, 0.35)
  )

  expect_equal(logL, logL_data)

  ### location-scale distributions (two-parametric):
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
})

# Log-likelihood profile function:
## vector-based:
test_that("loglik_profiling remains stable for vectors", {
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

  ## data-based:
  data <- reliability_data(data = alloy, x = cycles, status = status)
  profile_logL_data <- loglik_profiling(
    x = data,
    thres = threshold,
    distribution = "weibull3"
  )

  expect_equal(profile_logL, profile_logL_data)
})
