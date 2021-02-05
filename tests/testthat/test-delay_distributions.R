## dist_delay():
test_that("dist_delay supports dates vectors of class 'character'", {
  date_of_production <-   c("2014-02-17", "2014-07-14", "2014-06-26",
                            "2014-05-06", "2014-03-09", "2014-04-13",
                            "2014-05-20", "2014-07-07", "2014-01-27")
  date_of_registration <- c("2014-03-29", "2014-12-06", "2014-09-09",
                            "2014-06-16", "2014-05-23", "2014-05-09",
                            "2014-05-31", "2014-09-13", "2014-06-02")
  expect_type(
    dist_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration
    ),
    "list"
  )
})

test_that("dist_delay supports dates vectors of class 'Date'", {
  date_of_production <- as.Date(
    c("2014-02-17", "2014-07-14", "2014-06-26",
     "2014-05-06", "2014-03-09", "2014-04-13",
     "2014-05-20", "2014-07-07", "2014-01-27"),
    format = "%Y-%m-%d"
  )
  date_of_registration <- as.Date(
    c("2014-03-29", "2014-12-06", "2014-09-09",
      "2014-06-16", "2014-05-23", "2014-05-09",
      "2014-05-31", "2014-09-13", "2014-06-02"),
    format = "%Y-%m-%d"
  )
  expect_type(
    dist_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration
    ),
    "list"
  )
})

test_that("dist_delay supports missing dates using NA entries", {
  date_of_production   <- c(NA, "2014-02-17", "2014-07-14", NA,
                            "2014-03-10", "2014-05-14", NA, "2014-03-07",
                            "2014-03-09", "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17", "2014-02-09",
                            "2014-04-14", "2014-04-20", "2014-03-13", "2014-02-23",
                            NA, "2014-01-08", "2014-01-08")
  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)
  expect_type(
    dist_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration
    ),
    "list"
  )
})

test_that("dist_delay warns if any time difference is smaller or equal to zero", {
  date_of_production   <- c("2014-02-17", "2014-12-02", NA, "2014-03-10",
                            "2014-05-14", NA, "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07", "2014-01-27",
                            "2014-01-30", "2014-03-17", "2014-02-09", "2014-04-14",
                            "2014-04-20", "2014-03-13", "2014-02-23", NA,
                            "2014-01-08", "2014-01-08")
  date_of_registration <- c("2014-02-17", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  expect_warning(
    dist_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration
    ),
    "At least one of the time differences is smaller or equal to 0*"
  )
})

test_that("dist_delay stops if all time difference are smaller or equal to zero", {
  date_of_production   <- c("2014-02-17", "2014-12-06", "2014-09-09", "2014-01-08")
  date_of_registration <- c("2014-02-17", "2014-12-02", "2014-09-09", "2013-05-08")

  expect_error(
    dist_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration
    ),
    "All differences are smaller or equal to 0*"
  )
})

test_that("dist_delay stops if all time differences are NAs", {
  expect_error(
    dist_delay(
      date_1 = NA,
      date_2 = NA
    ),
    "All differences are NA*"
  )
})

test_that("dist_delay remains stable", {
  date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                      NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                      "2015-06-12", NA, "2015-05-04", NA, NA,
                      "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
                      "2015-11-26", NA, NA)

  date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                      NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                      "2015-07-11", NA, "2015-08-14", NA, NA,
                      "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                      "2015-12-02", NA, NA)

  params_delay_report <- dist_delay(
    date_1 = date_of_repair,
    date_2 = date_of_report,
    distribution = "exponential"
  )
  expect_snapshot_output(params_delay_report$coefficients)
  expect_snapshot_output(params_delay_report$delay)
})

## mcs_delay():
test_that("mcs_delay stops if date_1 and date_2 differ in lengths; vector case", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")

  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09")

  time_in_service <- rep(1000, length(date_of_production))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  expect_error(
    mcs_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration,
      time = time_in_service,
      status = status,
      distribution = "lognormal"
    ),
    "Elements of 'date_1' and 'date_2' differ in length!"
  )
})

test_that("mcs_delay stops if status is not a binary (0 or 1)", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")

  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  time_in_service <- rep(1000, length(date_of_production))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  expect_error(
    mcs_delay(
      date_1 = date_of_production,
      date_2 = date_of_registration,
      time = time_in_service,
      status = letters[seq_along(time_in_service)],
      distribution = "lognormal"
    ),
    "'status' must be numeric with elements 0 or 1!"
  )
})


test_that("mcs_delay remains stable by defining the seed", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")

  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  date_of_repair <-       c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                            NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                            "2015-06-12", NA, "2015-05-04", NA, NA,
                            "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
                            "2015-11-26", NA, NA)

  date_of_report <-       c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                            NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                            "2015-07-11", NA, "2015-08-14", NA, NA,
                            "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                            "2015-12-02", NA, NA)

  time_in_service <- rep(1000, length(date_of_production))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  set.seed(1234)

  mcs_delays <- mcs_delay(
    date_1 = list(date_of_production, date_of_repair),
    date_2 = list(date_of_registration, date_of_report),
    time = time_in_service,
    status = status,
    distribution = c("lognormal", "exponential")
  )
  expect_snapshot_output(mcs_delays$data)
  expect_snapshot_output(mcs_delays$sim_data)
})

## dist_delay_register():
test_that("dist_delay_register warns if any time difference is smaller or equal to zero", {
  date_of_production   <- c("2014-02-17", "2014-12-02", NA, "2014-03-10",
                            "2014-05-14", NA, "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07", "2014-01-27",
                            "2014-01-30", "2014-03-17", "2014-02-09", "2014-04-14",
                            "2014-04-20", "2014-03-13", "2014-02-23", NA,
                            "2014-01-08", "2014-01-08")
  date_of_registration <- c("2014-02-17", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  suppressWarnings(
    expect_warning(
      dist_delay_register(
        date_prod = date_of_production,
        date_register = date_of_registration
      ),
      "At least one of the time differences is smaller or equal to 0*"
    )
  )
})

test_that("dist_delay_register stops if all time difference are smaller or equal to zero", {
  date_of_production   <- c("2014-02-17", "2014-12-06", "2014-09-09", "2014-01-08")
  date_of_registration <- c("2014-02-17", "2014-12-02", "2014-09-09", "2013-05-08")

  suppressWarnings(
    expect_error(
      dist_delay_register(
        date_prod = date_of_production,
        date_register = date_of_registration
      ),
      "All differences are smaller or equal to 0*"
    )
  )
})

test_that("dist_delay_register stops if all time differences are NAs", {
  suppressWarnings(
    expect_error(
      dist_delay_register(
        date_prod = NA,
        date_register = NA
      ),
      "All differences are NA*"
    )
  )
})

test_that("dist_delay_register remains stable", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")
  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  suppressWarnings(
    params_delay_register <- dist_delay_register(
      date_prod = date_of_production,
      date_register = date_of_registration,
      distribution = "lognormal"
    )
  )
  expect_snapshot_output(params_delay_register)
})

## dist_delay_report():
test_that("dist_delay_report warns if any time difference is smaller or equal to zero", {
  date_of_repair <- c(NA, NA, "2050-07-04", "2015-04-10", NA,
                      NA, "2100-04-24", NA, "2015-04-25", "2015-04-24",
                      "2015-06-12", NA, "2015-05-04", NA, NA,
                      "2012-05-22", NA, "2015-09-17", NA, "2015-08-15",
                      "2015-11-26", NA, NA)

  date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                      NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                      "2015-07-11", NA, "2015-08-14", NA, NA,
                      "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                      "2015-12-02", NA, NA)

  suppressWarnings(
    expect_warning(
      dist_delay_report(
        date_repair = date_of_repair,
        date_report = date_of_report
      ),
      "At least one of the time differences is smaller or equal to 0*"
    )
  )
})

test_that("dist_delay_report stops if all time difference are smaller or equal to zero", {
  date_of_repair <- c("2015-04-10", "2015-04-25", "2015-04-24", "2015-06-12")

  date_of_report <- c("2015-04-10", "2015-04-25", "2015-04-24", "2015-06-12")

  suppressWarnings(
    expect_error(
      dist_delay_report(
        date_repair = date_of_repair,
        date_report = date_of_report
      ),
      "All differences are smaller or equal to 0*"
    )
  )
})

test_that("dist_delay_report stops if all time differences are NAs", {
  suppressWarnings(
    expect_error(
      dist_delay_report(
        date_repair = NA,
        date_report = NA
      ),
      "All differences are NA*"
    )
  )
})

test_that("dist_delay_report remains stable", {
  date_of_repair <- c(NA, NA, "2050-07-04", "2015-04-10", NA,
                      NA, "2100-04-24", NA, "2015-04-25", "2015-04-24",
                      "2015-06-12", NA, "2015-05-04", NA, NA,
                      "2012-05-22", NA, "2015-09-17", NA, "2015-08-15",
                      "2015-11-26", NA, NA)

  date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                      NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                      "2015-07-11", NA, "2015-08-14", NA, NA,
                      "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                      "2015-12-02", NA, NA)

  suppressWarnings(
    params_del_report <- dist_delay_report(
      date_repair = date_of_repair,
      date_report = date_of_report,
      distribution = "lognormal"
    )
  )
  expect_snapshot_output(params_del_report)
})

# mcs_delay_register():
test_that("mcs_delay_register remains stable by defining the seed", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")
  date_of_registration <- c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                            NA, NA, "2014-06-16", NA, "2014-05-23",
                            "2014-05-09", "2014-05-31", NA, "2014-04-13",
                            NA, NA, "2014-03-12", NA, "2014-06-02",
                            NA, "2014-03-21", "2014-06-19", NA, NA)

  op_time <- rep(1000, length(date_of_production))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  set.seed(1234)

  suppressWarnings(
    mcs_delay_register_list <- mcs_delay_register(
      date_prod = date_of_production,
      date_register = date_of_registration,
      time = op_time,
      status = status,
      distribution = "lognormal",
      details = TRUE
    )
  )
  expect_snapshot_output(mcs_delay_register_list)
})

# mcs_delay_report():
test_that("mcs_delay_report remains stable by defining the seed", {
  date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                      NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                      "2015-06-12", NA, "2015-05-04", NA, NA,
                      "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
                      "2015-11-26", NA, NA)

  date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                      NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                      "2015-07-11", NA, "2015-08-14", NA, NA,
                      "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                      "2015-12-02", NA, NA)

  op_time <- rep(1000, length(date_of_repair))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  set.seed(1234)

  suppressWarnings(
    mcs_delay_report_list <- mcs_delay_report(
      date_repair = date_of_repair,
      date_report = date_of_report,
      time = op_time,
      status = status,
      distribution = "lognormal",
      details = TRUE
    )
  )
  expect_snapshot_output(mcs_delay_report_list)
})

# mcs_delays():
test_that("mcs_delays remains stable by defining the seed", {
  date_of_production   <- c("2014-07-28", "2014-02-17", "2014-07-14",
                            "2014-06-26", "2014-03-10", "2014-05-14",
                            "2014-05-06", "2014-03-07", "2014-03-09",
                            "2014-04-13", "2014-05-20", "2014-07-07",
                            "2014-01-27", "2014-01-30", "2014-03-17",
                            "2014-02-09", "2014-04-14", "2014-04-20",
                            "2014-03-13", "2014-02-23", "2014-04-03",
                            "2014-01-08", "2014-01-08")
  date_of_registration <- c("2014-08-17", "2014-03-29", "2014-12-06",
                            "2014-09-09", "2014-05-14", "2014-07-01",
                            "2014-06-16", "2014-04-03", "2014-05-23",
                            "2014-05-09", "2014-05-31", "2014-08-12",
                            "2014-04-13", "2014-02-15", "2014-07-07",
                            "2014-03-12", "2014-05-27", "2014-06-02",
                            "2014-05-20", "2014-03-21", "2014-06-19",
                            "2014-02-12", "2014-03-27")
  date_of_repair <- c(NA, "2014-09-15", "2015-07-04", "2015-04-10", NA,
                      NA, "2015-04-24", NA, "2015-04-25", "2015-04-24",
                      "2015-06-12", NA, "2015-05-04", NA, NA,
                      "2015-05-22", NA, "2015-09-17", NA, "2015-08-15",
                      "2015-11-26", NA, NA)

  date_of_report <- c(NA, "2014-10-09", "2015-08-28", "2015-04-15", NA,
                      NA, "2015-05-16", NA, "2015-05-28", "2015-05-15",
                      "2015-07-11", NA, "2015-08-14", NA, NA,
                      "2015-06-05", NA, "2015-10-17", NA, "2015-08-21",
                      "2015-12-02", NA, NA)

  op_time <- rep(1000, length(date_of_repair))
  status <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)

  set.seed(1234)

  suppressWarnings(
    mcs_delays_list <- mcs_delays(
      date_prod = date_of_production,
      date_register = date_of_registration,
      date_repair = date_of_repair,
      date_report = date_of_report,
      time = op_time,
      status = status,
      distribution = "lognormal",
      details = TRUE)
  )
  expect_snapshot_output(mcs_delays_list)
})
