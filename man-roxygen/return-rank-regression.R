#' @return
#' A list with classes `wt_model`, `wt_rank_regression` and `wt_model_estimation`
#' which contains:
#'
#' * `coefficients` : A named vector of estimated coefficients (parameters of the
#'   assumed distribution). **Note**: The parameters are given in the
#'   (log-)location-scale-parameterization.
#' * `confint` : Confidence intervals for the (log-)location-scale parameters.
#'   For threshold distributions no confidence interval for the threshold parameter
#'   can be computed. If `direction = "y_on_x"`, back-transformed confidence
#'   intervals are provided.
#' * `shape_scale_coefficients` : Only included if `distribution` is `"weibull"`
#'   or `"weibull3"` (parameterization used in [Weibull][stats::Weibull]).
#' * `shape_scale_confint` : Only included if `distribution` is `"weibull"`
#'   or `"weibull3"`. Approximated confidence intervals for scale \eqn{\eta} and
#'   shape \eqn{\beta} (and threshold \eqn{\gamma} if `distribution = "weibull3"`).
#' * `varcov` : Only provided if `options = list(conf_method = "HC")` (default).
#'   Estimated heteroscedasticity-consistent (**HC**) variance-covariance matrix
#'   for the (log-)location-scale parameters.
#' * `r_squared` : Coefficient of determination.
#' * `data` : <%=data%>
#' * `distribution` : Specified distribution.
#' * `direction` : Specified direction.
#'
#' @md
