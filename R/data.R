#' Fatigue Life for Alloy T7989 Specimens
#'
#' A dataset containing the number of cycles of fatigue life for Alloy T7987
#' specimens.
#'
#' @format A tibble with 72 rows and 2 variables:
#' \describe{
#'   \item{cycles}{Number of cycles (in thousands).}
#'   \item{status}{If specimen failed before 300 thousand cycles \code{1} else \code{0}.}
#' }
#'
#' @source Meeker, William Q; Escobar, Luis A., Statistical Methods for
#' Reliability Data, New York: Wiley series in probability and statistics
#' (1998, p.131)
"alloy"



#' Distance to Failure for Vehicle Shock Absorbers
#'
#' Distance to failure for 38 vehicle shock absorbers.
#'
#' @format A tibble with 38 rows and 3 variables:
#' \describe{
#'   \item{distance}{Observed distance.}
#'   \item{failure_mode}{
#'     One of two failure modes (\code{mode_1} and \code{mode_2})
#'     or \code{censored} if no failure occured.
#'   }
#'   \item{status}{
#'     If \code{failure_mode} is either \code{mode_1} or \code{mode_2}
#'     this is \code{1} else \code{0}.
#'   }
#' }
#'
#' @source Meeker, William Q; Escobar, Luis A., Statistical Methods for
#' Reliability Data, New York: Wiley series in probability and statistics
#' (1998, p.630)
"shock"


#' High Voltage Stress Test for the Dielectric Insulation of Generator armature bars
#'
#' A sample of 58 segments of bars were subjected to a high voltage stress test.
#' Two failure modes occured, Mode D (degradation failure) and Mode E (early failure).
#'
#' @format A tibble with 58 rows and 3 variables:
#' \describe{
#'   \item{hours}{Observed hours.}
#'   \item{failure_mode}{
#'     One of two failure modes (\code{D} and \code{E})
#'     or \code{censored} if no failure occured.
#'   }
#'   \item{status}{
#'     If \code{failure_mode} is either \code{D} or \code{E}
#'     this is \code{1} else \code{0}.
#'   }
#' }
#'
#' @source Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
#' Failure Mode, Quality Progress, 35(6), 47-52, 2002
"voltage"
