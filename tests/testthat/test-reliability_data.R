# Prerequisites ----
x <- 1:10
stat_0 <- 0
stat_1 <- 1
stat_2 <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
id_1 <- "XXXXXX"
id_2 <- LETTERS[1:10]

tbl <- tibble::tibble(a = x, b = stat_2, c = id_2)

test_that("reliability_data fails if x or status are not provided", {
  expect_error(reliability_data())
  expect_error(reliability_data(x = 1))
})

test_that("data-based approach works with and without NSE", {
  expect_equal(reliability_data(tbl, a, b, c), reliability_data(tbl, "a", "b", "c"))
  x <- "a"; y <- "b"; z <- "c"
  expect_equal(reliability_data(tbl, a, b, c), reliability_data(tbl, !!x, !!y, !!z))
})

test_that("data-based approach fails for meaningless input", {
  expect_error(reliability_data(tbl, dplyr::vars(c(a, b)), b, c))
})

test_that("vector-based approach recycles status and id", {
  expect_true(all(reliability_data(x = x, status = stat_0, id = id_1)$status == 0))
  expect_true(all(reliability_data(x = x, status = stat_2, id = id_1)$id == "XXXXXX"))
})

# ID handling ----
test_that("ID is sequentiated regardless of approach", {
  expected <- paste0("ID", seq_along(x))
  expect_equal(reliability_data(x = x, status = 0)$id, expected)
  expect_equal(reliability_data(tbl, a, b)$id, expected)
})

