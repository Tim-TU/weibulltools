#' @return
#' Returns a list with classes \code{wt_model}, \code{wt_ml_estimation} and
#' \code{wt_model_estimation} containing the following elements:
#' \itemize{
#'   \item \code{coefficients} : A named vector of estimated coefficients
#'     (parameters of the assumed distribution). \strong{Note}: The parameters
#'     are given in location-scale-parameterization.
#'   \item \code{confint} : Confidence intervals for parameters.
#'   \item \code{varcov} : Estimated variance-covariance matrix for the
#'     parameters.
#'   \item \code{shape_scale_coefficients} : Only included if
#'     \code{distribution} is \code{"weibull"} or \code{"weibull3"}
#'     (parameterization used in \code{\link[stats:Weibull]{stats::Weibull}}).
#'   \item \code{shape_scale_confint} : Only included if \code{distribution} is
#'     \code{"weibull"} or \code{"weibull3"}. Confidence intervals
#'     for scale \eqn{\eta} and shape \eqn{\beta} (and threshold \eqn{\gamma})
#'     if \code{distribution} is \code{"weibull3"}.
#'   \item \code{logL} : The log-likelihood value.
#'   \item \code{aic} : Akaike Information Criterion.
#'   \item \code{bic} : Bayesian Information Criterion.
#'   \item \code{data} : <%=data%>
#'   \item \code{distribution} : Specified distribution.
#' }
