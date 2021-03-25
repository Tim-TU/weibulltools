#' @section Distributions:
#' The following table summarizes the available distributions and their parameters
#'
#' * *location parameter* \eqn{\mu},
#' * *scale parameter* \eqn{\sigma} or \eqn{\theta} and
#' * *threshold parameter* \eqn{\gamma}.
#'
#' The column order within `dist_params_tbl` is given in the table header.
#'
#' | **`distribution`**   | **`dist_params_tbl[1]`** | **`dist_params_tbl[2]`** | **`dist_params_tbl[3]`** |
#' | :------------------- | :----------------------: | :----------------------: | :----------------------: |
#' | `"sev"`              | \eqn{\mu}                | \eqn{\sigma}             | -                        |
#' | `"weibull"`          | \eqn{\mu}                | \eqn{\sigma}             | (\eqn{\gamma})           |
#' | `"normal"`           | \eqn{\mu}                | \eqn{\sigma}             | -                        |
#' | `"lognormal"`        | \eqn{\mu}                | \eqn{\sigma}             | (\eqn{\gamma})           |
#' | `"logistic"`         | \eqn{\mu}                | \eqn{\sigma}             | -                        |
#' | `"loglogistic"`      | \eqn{\mu}                | \eqn{\sigma}             | (\eqn{\gamma})           |
#' | `"exponential"`      | \eqn{\theta}             | (\eqn{\gamma})           | -                        |
#'
#' @md
