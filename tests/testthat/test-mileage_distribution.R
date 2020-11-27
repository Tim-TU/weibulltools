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

test_that("dist_mileage warns if any x or mileage is smaller or equal to zero", {
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
    "at least one element of 'mileage' and/or 'x'*"
  )
})
