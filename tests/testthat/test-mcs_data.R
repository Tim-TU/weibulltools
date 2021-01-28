# Prerequisites ----
mileage <- 1:10
time <- 1:10
stat_0 <- 0
stat_1 <- 1
stat_2 <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
id_1 <- "XXXXXX"
id_2 <- LETTERS[1:10]

# mcs_mileage_data:
tbl <- tibble::tibble(a = mileage, b = time, c = stat_2, d = id_2)

test_that("mcs_mileage_data fails if mileage or time are not provided", {
  expect_error(mcs_mileage_data())
  expect_error(mcs_mileage_data(mileage = 1))
})

test_that("data-based approach works with and without NSE", {
  expect_equal(mcs_mileage_data(tbl, a, b, c), mcs_mileage_data(tbl, "a", "b", "c"))
  x <- "a"; y <- "b"; z <- "c"
  expect_equal(mcs_mileage_data(tbl, a, b, c), mcs_mileage_data(tbl, !!x, !!y, !!z))
})

test_that("data-based approach fails for meaningless input", {
  expect_error(mcs_mileage_data(tbl, dplyr::vars(c(a, b)), b, c))
})

test_that("vector-based approach recycles status and id", {
  expect_true(all(mcs_mileage_data(mileage = mileage, time = time, status = stat_0, id = id_1)$status == 0))
  expect_true(all(mcs_mileage_data(mileage = mileage, time = time, status = stat_2, id = id_1)$id == "XXXXXX"))
})

# ID handling ----
test_that("ID is sequentiated regardless of approach", {
  expected <- paste0("ID", seq_along(mileage))
  expect_equal(mcs_mileage_data(mileage = mileage, time = time, status = 0)$id, expected)
  expect_equal(mcs_mileage_data(tbl, a, b)$id, expected)
})

