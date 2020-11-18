#' Distance to failure for vehicle shock absorbers
#'
#' Distance to failure for 38 vehicle shock absorbers.
#'
#' @format A tibble with 38 rows and 3 variables:
#' \describe{
#'   \item{distance}{Observed distance.}
#'   \item{failure_mode}{
#'     One of two failure modes (\code{mode_1} and \code{mode_2})
#'     or \code{censored} if no failure occured yet.
#'   }
#'   \item{status}{
#'     If \code{failure_mode} is either \code{mode_1} or \code{mode_2}
#'     this is {1} else {0}.
#'   }
#' }
#'
#' @source Meeker, William Q; Escobar, Luis A., Statistical methods for
#' reliability data, New York: Wiley series in probability and statistics
#' (1998, p.630)
"shock"
