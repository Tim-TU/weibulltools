## dist_mileage():
test_that("dist_mileage supports missing distances and times using NA entries", {
  op_time <- c(65, NA, 210, 213, 277, 287, 312, NA, 337, 350, 377, 379, 386,
               413, 426, 436, 451, 472, 483, 512, 525, 556, NA)
  mileage <- c(NA, 15655, 13629, NA, 24291, 34455, NA, 21659, 21737,
               NA, 21068, 22986, NA, 31592, 49050, NA, 10918, 11153,
               NA, 122842, 20349, NA, 40777)
  expect_s3_class(
    dist_mileage(
    x = mileage,
    time = op_time
    ),
    "wt_mileage_estimation"
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
      x = mileage,
      time = op_time
    ),
    "At least one computed annual distance*"
  )
})

test_that("dist_mileage stops if at least one distance is smaller than zero", {
  op_time <- c(65, 500, 200, 350)
  mileage <- c(0, -10, 0, -5) # mileage / time

  expect_error(
    dist_mileage(
      x = mileage,
      time = op_time
    ),
    "Elements with negative distances are not meaningful and must be removed!"
  )
})

test_that("dist_mileage stops if all computed annual distances are NAs", {
  expect_error(
    dist_mileage(
      x = NA,
      time = NA
    ),
    "All computed annual distances are 'NA'*"
  )
})

test_that("dist_mileage remains stable", {
  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09", NA,
                            NA, "2014-06-16", NA, "2014-05-23", "2014-05-09",
                            "2014-05-31", NA, "2014-04-13", NA, NA, "2014-03-12",
                            NA, "2014-06-02", NA, "2014-03-21", "2014-06-19",
                            NA, NA)
  date_of_repair       <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                            NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                            "2015-06-12", NA, "2015-05-04", NA, NA, "2015-05-22",
                            NA, "2015-09-17", NA, "2015-08-15", "2015-11-26",
                            NA, NA)
  mileage              <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
                            29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
                            NA, 122842, 20349, NA, NA)

  op_time <- as.numeric(
    difftime(
      as.Date(date_of_repair, format = "%Y-%m-%d"),
      as.Date(date_of_registration, format = "%Y-%m-%d"),
      units = "days"
    )
  )

    params_mileage_annual <- dist_mileage(
      x = mileage,
      time = op_time,
      distribution = "lognormal"
    )
  expect_snapshot_output(params_mileage_annual$coefficients)
  expect_snapshot_output(params_mileage_annual$miles_annual)
})

## mcs_mileage():
test_that("mcs_mileage stops if time and mileage differ in length; vector case", {
  mileage <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
                            29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
                            NA, 122842, 20349, NA, NA)

  time_in_service <- rep(1000, 5)

  expect_error(
    mcs_mileage(
      x = mileage,
      time = time_in_service
    ),
    "Elements of 'x' and 'time' differ in length!"
  )
})

test_that("mcs_mileage stops if status is not a binary (0 or 1)", {
  mileage <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
               29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
               NA, 122842, 20349, NA, NA)

  time_in_service <- rep(1000, length(mileage))

  expect_error(
    mcs_mileage(
      x = mileage,
      time = time_in_service,
      status = letters[seq_along(time_in_service)]
    ),
    "'status' must be numeric with elements 0 or 1!"
  )
})


test_that("mcs_mileage remains stable by defining the seed", {

  mileage <- c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
               29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
               NA, 122842, 20349, NA, NA)

  time_in_service <- rep(1000, length(mileage))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  set.seed(1234)

  mcs_distances <- mcs_mileage(
    x = mileage,
    time = time_in_service,
    status = status,
    distribution = "lognormal"
  )

  expect_snapshot_output(mcs_distances$data)
  expect_snapshot_output(mcs_distances$sim_data)
})
