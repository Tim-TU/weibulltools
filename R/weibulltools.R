#' weibulltools
#'
#' The weibulltools package contains methods for examining bench test or field data using the well-known weibull analysis.
#'
#' @docType package
#' @name weibulltools
#' @useDynLib weibulltools, .registration = TRUE
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
NULL

utils::globalVariables(c('time', 'status', 'failure', 'survivor', 'n_i', 'n_out',
                         'n_in', 'group', 'estimates', 'lam_nel', 'H_nel'))
