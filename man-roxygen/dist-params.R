#' @section Distributions:
#' The following table summarizes the available distributions and their parameters
#'
#' * *location parameter* \eqn{\mu},
#' * *scale parameter* \eqn{\sigma} and
#' * *threshold parameter* \eqn{\gamma}.
#'
#' The order within `dist_params` is given in the table header.
#'
#' | **`distribution`**   | **`dist_params[1]`** | **`dist_params[2]`** | **`dist_params[3]`** |
#' | :------------------- | :------------------: | :------------------: | :------------------: |
#' | `"sev"`              | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"weibull"`          | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"weibull3"`         | \eqn{\mu}            | \eqn{\sigma}         | \eqn{\gamma}         |
#' | `"normal"`           | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"lognormal"`        | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"lognormal3"`       | \eqn{\mu}            | \eqn{\sigma}         | \eqn{\gamma}         |
#' | `"logistic"`         | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"loglogistic"`      | \eqn{\mu}            | \eqn{\sigma}         | -                    |
#' | `"loglogistic3"`     | \eqn{\mu}            | \eqn{\sigma}         | \eqn{\gamma}         |
#'
#' @md
