#' @name weibulltools-package
#' @aliases weibulltools
#' @title weibulltools
#'
#' @description
#' The \emph{weibulltools} package focuses on statistical methods and
#' visualizations that are often used in reliability engineering. It provides a
#' compact and easily accessible set of methods and visualization tools that
#' make the examination and adjustment as well as the analysis and
#' interpretation of field data (and bench tests) as simple as possible.
#'
#' Besides the well-known Weibull analysis, the package supports multiple
#' lifetime distributions and also contains Monte Carlo methods for the
#' correction and completion of imprecisely recorded or unknown lifetime
#' characteristics.
#'
#' Plots are created statically
#' (\code{\link[ggplot2:ggplot2-package]{ggplot2}}) or
#' interactively (\code{\link[plotly:plot_ly]{plotly}}) and can be
#' customized with functions of the respective visualization package.
#'
#' @docType package
#' @useDynLib weibulltools, .registration = TRUE
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr "%>%"
#' @importFrom utils hasName
#' @importFrom dplyr .data
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecate_warn
## usethis namespace: end
NULL
