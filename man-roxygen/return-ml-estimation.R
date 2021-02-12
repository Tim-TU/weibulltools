#' @return
#' A list with classes `wt_model`, `wt_ml_estimation` and `wt_model_estimation`
#' which contains:
#'
#' * `coefficients` : A named vector of estimated coefficients (parameters of the
#'   assumed distribution). **Note**: The parameters are given in the
#'   (log-)location-scale-parameterization.
#' * `confint` : Confidence intervals for (log-)location-scale parameters.
#' * `shape_scale_coefficients` : Only included if `distribution` is `"weibull"`
#'   or `"weibull3"` (parameterization used in [Weibull][stats::Weibull]).
#' * `shape_scale_confint` : Only included if `distribution` is `"weibull"`
#'   or `"weibull3"`. Confidence intervals for scale \eqn{\eta} and shape \eqn{\beta}
#'   (and threshold \eqn{\gamma} if `distribution = "weibull3"`).
#' * `varcov` : Estimated variance-covariance matrix of (log-)location-scale parameters.
#' * `logL` : The log-likelihood value.
#' * `aic` : Akaike Information Criterion.
#' * `bic` : Bayesian Information Criterion.
#' * `data` : <%=data%>
#' * `distribution` : Specified distribution.
#'
#' @md
