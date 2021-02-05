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
#'     or \code{censored} if no failure occurred.
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
#' Two failure modes occurred, Mode D (degradation failure) and Mode E (early failure).
#'
#' @format A tibble with 58 rows and 3 variables:
#' \describe{
#'   \item{hours}{Observed hours.}
#'   \item{failure_mode}{
#'     One of two failure modes (\code{D} and \code{E})
#'     or \code{censored} if no failure occurred.
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



#' Field Data
#'
#' @description
#' An illustrative field dataset that contains a variety of variables commonly
#' collected in the automotive sector.
#'
#' The dataset has complete information about failed and incomplete information
#' about intact vehicles. See 'Format' and 'Details' for further insights.
#'
#' @details
#' All vehicles were produced in 2014 and an analysis of the field data was
#' made at the end of 2015. At the date of analysis, there were 684 failed and
#' 10,000 intact vehicles.
#'
#' \strong{Censored vehicles}:
#'
#' For censored units the service time (\code{dis}) was computed as the difference
#' of the date of analysis \code{"2015-12-31"} and the \code{registration_date}.
#'
#' For many units the latter date is unknown. For these, the difference of the
#' analysis date and \code{production_date} was used to get a rough estimation of
#' the real service time. This uncertainty has to be considered in the subsequent
#' analysis (see \strong{delay in registration} in the section 'Details' of
#' \code{\link{mcs_delay}}).
#'
#' Furthermore, due to the delay in report, the computed service time could also
#' be inaccurate. This uncertainty should be considered as well (see
#' \strong{delay in report} in the section 'Details' of \code{\link{mcs_delay}}).
#'
#' The lifetime characteristic \code{mileage} is unknown for all censored units.
#' If an analysis is to be made for this lifetime characteristic, covered distances
#' for these units have to be estimated (see \code{\link{mcs_mileage}}).
#'
#' \strong{Failed vehicles}:
#' For failed units the service time (\code{dis}) is computed as the difference
#' of \code{repair_date} and \code{registration_date}, which are known for all of them.
#'
#' @format A tibble with 10,684 rows and 20 variables:
#' \describe{
#'   \item{vin}{Vehicle identification number.}
#'   \item{dis}{Days in service.}
#'   \item{mileage}{Distances covered, which are unknown for censored units.}
#'   \item{status}{\code{1} for failed and \code{0} for censored units.}
#'   \item{production_date}{Date of production.}
#'   \item{registration_date}{Date of registration. Known for all failed units and
#'     for a few intact units.}
#'   \item{repair_date}{The date on which the failure was repaired. It is assumed
#'     that the repair date is equal to the date of failure occurrence.}
#'   \item{report_date}{The date on which lifetime information about the failure
#'     were available.}
#'   \item{country}{Delivering country.}
#'   \item{region}{The region within the country of delivery. Known for registered
#'     vehicles, \code{NA} for units with a missing \code{registration_date}.}
#'   \item{climatic_zone}{Climatic zone based on "Köppen-Geiger" climate classification.
#'     Known for registered vehicles, \code{NA} for units with a missing \code{registration_date}.}
#'   \item{climatic_subzone}{Climatic subzone based on "Köppen-Geiger" climate classification.
#'     Known for registered vehicles, \code{NA} for units with a \code{registration_date}.}
#'   \item{brand}{Brand of the vehicle.}
#'   \item{vehicle_model}{Model of the vehicle.}
#'   \item{engine_type}{Type of the engine.}
#'   \item{engine_date}{Date where the engine was installed.}
#'   \item{gear_type}{Type of the gear.}
#'   \item{gear_date}{Date where the gear was installed.}
#'   \item{transmission}{Transmission of the vehicle.}
#'   \item{fuel}{Vehicle fuel.}
#' }
#'
#' @seealso \code{\link{mcs_mileage_data}}
#'
"field_data"
