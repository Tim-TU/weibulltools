#' @return
#' Returns a list with the classes \code{"rank_regression"} and
#' \code{"model_estimation"} containing the following elements:
#' \itemize{
#'   \item \code{coefficients} : A named vector of estimated coefficients
#'     (parameters of the assumed distribution). \strong{Note}: The parameters
#'     are given in location-scale-parameterization. If \code{distribution} is
#'     \code{"weibull"} this is not the parameterization used in
#'     \code{\link[stats:Weibull]{stats::Weibull}}. The parameters of the
#'     shape-scale-parameterization are given under
#'     \code{shape_scale_coefficients}.
#'   \item \code{shape_scale_coefficients} : Only included if
#'     \code{distribution} is \code{"weibull"} or \code{"weibull3"}. See
#'     \code{coefficients}.
#'   \item \code{confint} : Only included if \code{distribution} is \code{"weibull"}
#'     or \code{"weibull3"}. Approximated confidence intervals for \eqn{\eta} and
#'     \eqn{\beta} (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'   \item \code{loc_sc_confint} : Confidence intervals for (log-)location-scale
#'     parameters. If distribution is \code{"lognormal3"} or \code{"loglogistic3"}
#'     a confidence interval for the threshold parameter is not computed.
#'   \item \code{loc_sc_varcov} : Provided, if \code{distribution} is not
#'     \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'     variance-covariance matrix for the (log-)location-scale parameters.
#'   \item \code{r_squared} : Coefficient of determination.
#'   \item \code{data} : <%=data%>
#'   \item \code{distribution} : Specified distribution.
#' }
