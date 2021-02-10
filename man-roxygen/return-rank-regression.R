#' @return
#' A list with classes \code{wt_model}, \code{wt_rank_regression} and
#' \code{wt_model_estimation} which contains:
#' \itemize{
#'   \item \code{coefficients} : A named vector of estimated coefficients
#'     (parameters of the assumed distribution). \strong{Note}: The parameters
#'     are given in location-scale-parameterization.
#'   \item \code{confint} : Confidence intervals for parameters. If
#'     \code{distribution} is \code{"lognormal3"} or \code{"loglogistic3"} no
#'     confidence interval for the threshold parameter is computed.
#'   \item \code{varcov} : Provided, if \code{distribution} is not
#'     \code{"weibull"} or \code{"weibull3"}. Estimated heteroscedasticity-consistent
#'     variance-covariance matrix for the (log-)location-scale parameters.
#'   \item \code{shape_scale_coefficients} : Only included if
#'     \code{distribution} is \code{"weibull"} or \code{"weibull3"}
#'     (parameterization used in \code{\link[stats:Weibull]{stats::Weibull}}).
#'   \item \code{shape_scale_confint} : Only included if \code{distribution} is
#'     \code{"weibull"} or \code{"weibull3"}. Approximated confidence intervals
#'     for scale \eqn{\eta} and shape \eqn{\beta} (and threshold \eqn{\gamma})
#'     if \code{distribution} is \code{"weibull3"}.
#'   \item \code{r_squared} : Coefficient of determination.
#'   \item \code{data} : <%=data%>
#'   \item \code{distribution} : Specified distribution.
#'   \item \code{direction} : Specified direction.
#' }
