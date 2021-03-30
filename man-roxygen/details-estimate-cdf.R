#' @details
#' <%=header%>
#'
#' * `"mr"` : Method _Median Ranks_ is used to estimate the failure probabilities
#'   of failed units without considering censored items. Tied observations can be
#'   handled in three ways (See 'Options'):
#'
#'   * `"max"` : Highest observed rank is assigned to tied observations.
#'   * `"min"` : Lowest observed rank is assigned to tied observations.
#'   * `"average"` : Mean rank is assigned to tied observations.
#'
#'   Two formulas can be used to determine cumulative failure probabilities
#'   *F(t)* (See 'Options'):
#'
#'   * `"benard"` : Benard's approximation for Median Ranks.
#'   * `"invbeta"` : Exact Median Ranks using the inverse beta distribution.
#'
#' * `"johnson"` : The _Johnson_ method is used to estimate the failure
#'   probabilities of failed units, taking censored units into account. Compared
#'   to complete data, correction of probabilities is done by the computation of
#'   adjusted ranks. Two formulas can be used to determine cumulative failure
#'   probabilities *F(t)* (See 'Options'):
#'
#'   * `"benard"` : Benard's approximation for Median Ranks.
#'   * `"invbeta"` : Exact Median Ranks using the inverse beta distribution.
#'
#' * `"kaplan"` : The method of _Kaplan_ and _Meier_ is used to estimate the
#'   survival function *S(t)* with respect to (multiple) right censored data.
#'   The complement of *S(t)*, i.e. *F(t)*, is returned. In contrast to the
#'   original *Kaplan-Meier* estimator, one modification is made (see 'References').
#' * `"nelson"` : The _Nelson-Aalen_ estimator models the cumulative hazard rate
#'   function in case of (multiple) right censored data. Equating the formal
#'   definition of the hazard rate with that according to *Nelson-Aalen* results
#'   in a formula for the calculation of failure probabilities.
#'
#' @md
