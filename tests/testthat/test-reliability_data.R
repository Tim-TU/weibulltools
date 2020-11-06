# Prerequisites ----
x <- 1:10
stat_0 <- 0
stat_1 <- 1
stat_2 <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
id_1 <- "XXXXXX"
id_2 <- LETTERS[1:10]

tbl <- tibble(a = x, b = stat_2, c = id_2)

# General tests ----
test_that("reliability_data fails if x, status or id are not provided", {
  expect_error(reliability_data())
  expect_error(reliability_data(x = 1))
  expect_error(reliability_data(x = 1, status = 1))
})

# Data-based approach ----
test_that("data-based approach works with and without NSE", {
  expect_equal(reliability_data(tbl, a, b, c), reliability_data(tbl, "a", "b", "c"))
})

test_that("data-based approach fails for meaningless input", {
  expect_error(reliability_data(tbl, c("a", "b"), b, c))
})


# Vector-based approach ----
test_that("vector-based approach recycles status and id", {
  expect_true(all(reliability_data(x = x, status = stat_0, id = id_1)$status == 0))
  expect_true(all(reliability_data(x = x, status = stat_2, id = id_1)$id == "XXXXXX"))
})

test_that("vector-based approach fails if x is shorter than status or id", {
  expect_error(reliability_data(x = 1, status = stat_2, id = id_2), "length of x.*")
})
