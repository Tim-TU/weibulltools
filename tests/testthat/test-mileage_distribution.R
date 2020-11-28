## dist_mileage():
test_that("dist_mileage supports missing distances and times using NA entries", {
  op_time <- c(65, NA, 210, 213, 277, 287, 312, NA, 337, 350, 377, 379, 386,
               413, 426, 436, 451, 472, 483, 512, 525, 556, NA)
  mileage <- c(NA, 15655, 13629, NA, 24291, 34455, NA, 21659, 21737,
               NA, 21068, 22986, NA, 31592, 49050, NA, 10918, 11153,
               NA, 122842, 20349, NA, 40777)
  expect_type(
    dist_mileage(
      mileage = mileage,
      x = op_time
    ),
    "list"
  )
})

test_that("dist_mileage warns if any computed annual distances is smaller or equal to zero", {
  op_time <- c(65, -10, 210, 213, 277, 287, 312, NA, 337, 350, 377, 379, 386,
               413, 426, 436, 451, 472, 483, 512, 525, 556, NA)
  mileage <- c(0, 15655, 13629, NA, 24291, 34455, NA, 21659, 21737,
               NA, 21068, 22986, NA, 31592, 49050, NA, 10918, 11153,
               NA, 122842, 20349, NA, 40777)

  expect_warning(
    dist_mileage(
      mileage = mileage,
      x = op_time
    ),
    "at least one computed annual distance*"
  )
})

test_that("dist_mileage stops if all computed annual distances are smaller or equal to zero", {
  op_time <- c(65, 500, 200, 350)
  mileage <- c(0, -10, 0, -5) # mileage / x

  expect_error(
    dist_mileage(
      mileage = mileage,
      x = op_time
    ),
    "all computed annual distances are smaller or equal to 0*"
  )
})

test_that("dist_mileage stops if all computed annual distances are NAs", {
  expect_error(
    dist_mileage(
      mileage = NA,
      x = NA
    ),
    "all computed annual distances are NA*"
  )
})

test_that("dist_mileage warns when there are cases where both, mileage and x are negative", {

  mileage <- c(100, 1000, 200, NA, 10, -200)
  op_time <- c(10, 200, 30, NA, 10, -10)

  expect_warning(
    dist_mileage(
      mileage = mileage,
      x = op_time
    ),
    "corresponding element\\(s\\) of both,*"
  )
})
