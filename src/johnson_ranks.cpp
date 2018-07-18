#include <Rcpp.h>
using namespace Rcpp;
//' Computation of Johnson Ranks
//'
//' This function calculates the Johnson ranks which are used to estimate the
//' failure probabilities in case of (multiple) right censored data.
//'
//' @param f a numeric vector indicating the number of failed units for a
//'   specific realization of the lifetime characteristic.
//' @param n_out a numeric vector indicating the number of failed and censored
//'   units that have a shorter realization of lifetime characteristic as unit
//'   \emph{i}.
//' @param n an integer value indicating the sample size.
//'
//' @return A numeric vector containing the computed Johnson ranks.
//' @export
//' @examples
//'   defectives <- c(0, 1, 2, 0, 0, 0, 3, 0, 2, 0)
//'   n_out <- c(0, 2, 4, 8, 9, 11, 12, 16, 20, 22)
//'   n <- 23
//'   johnson_ranks <- calculate_ranks(f = defectives, n_out = n_out, n = n)
// [[Rcpp::export]]
NumericVector calculate_ranks(NumericVector f, NumericVector n_out, int n) {
  int k = f.size();
  NumericVector r = f;
  for (int i = 1; i < k; i++) {
    r[i] = r[i - 1] + f[i] * ((n + 1 - r[i - 1]) / (1 + n - n_out[i]));
  }
  return r;
}
