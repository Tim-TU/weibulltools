#' @return
#' Returns a list with the classes \code{"ml_estimation"} and
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
#'     or \code{"weibull3"}. Confidence intervals for \eqn{\eta} and \eqn{\beta}
#'     (and \eqn{\gamma} if \code{distribution} is \code{"weibull3"}).
#'   \item \code{loc_sc_confint} : Confidence intervals the (log-)location-scale
#'     parameters.
#'   \item \code{loc_sc_varcov} : Estimated variance-covariance matrix for the
#'     (log-)location-scale parameters.
#'   \item \code{logL} : The log-likelihood value.
#'   \item \code{aic} : Akaike Information Criterion.
#'   \item \code{bic} : Bayesian Information Criterion.
#'   \item \code{data} : <%=data%>
#'   \item \code{distribution} : Specified distribution.
#' }
