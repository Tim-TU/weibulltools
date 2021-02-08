# Prerequisites ----
mileage <- 1:10
time <- 1:10
stat_0 <- 0
stat_1 <- 1
stat_2 <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
id_1 <- "XXXXXX"
id_2 <- LETTERS[1:10]
date1 <- as.Date(1:10, origin = "2020-01-01")
date2 <- as.character(date1)

# mcs_mileage_data:
tbl <- tibble::tibble(a = mileage, b = time, c = stat_2, d = id_2)

test_that("mcs_mileage_data fails if mileage or time are not provided", {
  expect_error(mcs_mileage_data())
  expect_error(mcs_mileage_data(mileage = 1))
})

test_that("data-based approach for mcs_mileage_data works with and without NSE", {
  expect_equal(mcs_mileage_data(tbl, a, b, c), mcs_mileage_data(tbl, "a", "b", "c"))
  x <- "a"; y <- "b"; z <- "c"
  expect_equal(mcs_mileage_data(tbl, a, b, c), mcs_mileage_data(tbl, !!x, !!y, !!z))
})

test_that("data-based approach for mcs_mileage_data fails for meaningless input", {
  expect_error(mcs_mileage_data(tbl, dplyr::vars(c(a, b)), b, c))
})

test_that("vector-based approach for mcs_mileage_data recycles status and id", {
  expect_true(all(mcs_mileage_data(mileage = mileage, time = time, status = stat_0, id = id_1)$status == 0))
  expect_true(all(mcs_mileage_data(mileage = mileage, time = time, status = stat_2, id = id_1)$id == "XXXXXX"))
})

# ID handling ----
test_that("ID inside mcs_mileage_data is sequentiated regardless of approach", {
  expected <- paste0("ID", seq_along(mileage))
  expect_equal(mcs_mileage_data(mileage = mileage, time = time, status = 0)$id, expected)
  expect_equal(mcs_mileage_data(tbl, a, b)$id, expected)
})

# mcs_delay_data:
tbl2 <- tibble::tibble(a1 = date1, a2 = date2, b = time, c = stat_2, d = id_2)

test_that("mcs_delay_data fails if date_1, date_2 or time are not provided", {
  expect_error(mcs_delay_data())
  expect_error(mcs_delay_data(time = 1))
})

test_that("data-based approach for mcs_delay_data works with and without NSE", {
  expect_equal(mcs_delay_data(tbl2, a1, a2, b, c), mcs_delay_data(tbl2, "a1", "a2", "b", "c"))
  x1 <- "a1"; x2 <- "a2"; y <- "b"; z <- "c"
  expect_equal(mcs_delay_data(tbl2, a1, a2, b, c), mcs_delay_data(tbl2, !!x1, !!x2, !!y, !!z))
})

test_that("data-based approach for mcs_delay_data fails for meaningless input", {
  expect_error(mcs_delay_data(tbl2, dplyr::vars(c(a1, a2, b)), b, c))
})

test_that("vector-based approach for mcs_delay_data recycles status and id", {
  expect_true(all(mcs_delay_data(date_1 = date1, date_2 = date2, time = time, status = stat_0, id = id_1)$status == 0))
  expect_true(all(mcs_delay_data(date_1 = date1, date_2 = date2, time = time, status = stat_2, id = id_1)$id == "XXXXXX"))
})

# ID handling ----
test_that("ID in mcs_delay_data is sequentiated regardless of approach", {
  expected <- paste0("ID", seq_along(date1))
  expect_equal(mcs_delay_data(date_1 = date1, date_2 = date2, time = time, status = 0)$id, expected)
  expect_equal(mcs_delay_data(tbl2, a1, a2, b)$id, expected)
})
