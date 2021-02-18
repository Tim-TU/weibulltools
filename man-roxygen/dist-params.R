#' @section Distributions:
#' The following table summarizes the available distributions and their parameters
#'
#' * *location parameter* \eqn{\mu},
#' * *scale parameter* \eqn{\sigma} and
#' * *threshold parameter* \eqn{\gamma}.
#'
#' The order within `dist_params` is given in the table header.
#'
#' \tabular{lccc}{
#'   **`distribution`** \tab **`dist_params[1]`** \tab **`dist_params[2]`** \tab **`dist_params[3]`**   \cr
#'   `"sev"`            \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"weibull"`        \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"weibull3"`       \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab \eqn{\gamma}           \cr
#'   `"normal"`         \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"lognormal"`      \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"lognormal3"`     \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab \eqn{\gamma}           \cr
#'   `"logistic"`       \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"loglogistic"`    \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab -                      \cr
#'   `"loglogistic3"`   \tab \eqn{\mu}            \tab \eqn{\sigma}         \tab \eqn{\gamma}
#' }
#'
#' @md
