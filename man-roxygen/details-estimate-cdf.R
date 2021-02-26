#' @details
#' <%=header%>
#' \itemize{
#'   \item \code{"mr"} : Method \emph{Median Ranks} is used to estimate the failure
#'     probabilities of failed units without considering censored items.
#'     Tied observations can be handled in three ways (See 'Options'):
#'     \itemize{
#'       \item \code{"max"} : Highest observed rank is assigned to tied observations.
#'       \item \code{"min"} : Lowest observed rank is assigned to tied observations.
#'       \item \code{"average"} : Mean rank is assigned to tied observations.
#'     }
#'     Two formulas can be used to determine cumulative failure probabilities
#'     \emph{F(t)} (See 'Options'):
#'     \itemize{
#'       \item \code{"benard"} : Benard's approximation for Median Ranks.
#'       \item \code{"invbeta"} : Exact Median Ranks using the inverse beta distribution.
#'     }
#'   \item \code{"johnson"} : The \emph{Johnson} method is used to estimate the
#'     failure probabilities of failed units, taking censored units into account.
#'     Compared to complete data, correction of probabilities is done by the
#'     computation of adjusted ranks.
#'     Two formulas can be used to determine cumulative failure probabilities
#'     \emph{F(t)} (See 'Options'):
#'     \itemize{
#'       \item \code{"benard"} : Benard's approximation for adjusted ranks.
#'       \item \code{"invbeta"} : Exact adjusted ranks using the inverse beta distribution.
#'     }
#'   \item \code{"kaplan"} : The method of \emph{Kaplan} and \emph{Meier} is used
#'     to estimate the survival function \emph{S(t)} with respect to (multiple)
#'     right censored data. The complement of \emph{S(t)}, i.e. \emph{F(t)}, is
#'     returned. In contrast to the original \emph{Kaplan-Meier} estimator, one
#'     modification is made (see 'References').
#'   \item \code{"nelson"} : The \emph{Nelson-Aalen} estimator models the cumulative
#'     hazard rate function in case of (multiple) right censored data. Equating the
#'     formal definition of the hazard rate with that according to \emph{Nelson-Aalen}
#'     results in a formula for the calculation of failure probabilities.
#' }
