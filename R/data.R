#' Fatigue life for alloy T7989 specimens
#'
#' A dataset containing the number of cycles of fatigue life for alloy T7987
#' specimens.
#'
#' @format A tibble with 72 rows and 2 variables:
#' \describe{
#'   \item{cycles}{Number of cycles (in thousands).}
#'   \item{status}{If specimen failed before 300 thousand cycles \code{1} else \code{0}.}
#' }
#'
#' @source Meeker, William Q; Escobar, Luis A., Statistical methods for
#' reliability data, New York: Wiley series in probability and statistics
#' (1998, p.131)
"alloy"



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
