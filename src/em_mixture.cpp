#include "RcppArmadillo.h"

using namespace Rcpp;

// [[Rcpp::export]]
double getLambda(NumericVector& x, NumericVector& weights, NumericVector& status,
                 double beta) {

  NumericVector xBeta = pow(x, beta) * weights;
  NumericVector failuresWeights = weights[status == 1];

  double lambda = sum(xBeta) / sum(failuresWeights);
  lambda = std::pow(lambda, 1.0 / beta);
  return lambda;
}

// [[Rcpp::export]]
double g(NumericVector& x,NumericVector& weights, NumericVector& status,
         double beta) {

  NumericVector failures = x[status == 1];
  NumericVector failuresWeights = weights[status == 1];

  double m = sum(failuresWeights);
  NumericVector logFailures = log(failures);
  logFailures = logFailures * failuresWeights;

  NumericVector l = log(x);
  NumericVector xBeta =pow(x, beta);
  xBeta = xBeta * weights;

  NumericVector xLog = xBeta * l;
  return 1.0 / beta + sum(logFailures) / m - ((sum(xLog)) / sum(xBeta));
}

// [[Rcpp::export]]
double gDiv(NumericVector& x, NumericVector& weights, double beta) {

  // double n = x.length();
  NumericVector l = log(x);
  NumericVector l2 = pow(l, 2);
  NumericVector xBeta =pow(x, beta);
  xBeta = xBeta * weights;

  NumericVector xBetaLog = xBeta * l;
  NumericVector xBetaLog2 = xBeta * l2;
  double w = sum(xBetaLog);
  double v = sum(xBeta);
  double s = sum(xBetaLog2);

  return (-1.0 / (beta * beta)) + (w * w) / (v * v) - (s / v);
}

// [[Rcpp::export]]
double NewtonRaphson(NumericVector& x, NumericVector& weights,
                     NumericVector& status) {

  double beta = 1;
  double alpha = 1;
  for (int i = 0; i < 10; i++) {
    beta = exp(alpha);
    alpha = alpha- g(x, weights, status, beta) / (gDiv(x, weights, beta) * beta);
    //beta = beta- g(x, weights, status, beta) / gDiv(x,weights,beta);
  }
  beta = exp(alpha);
  return beta;
}

// [[Rcpp::export]]
NumericMatrix MStepWeibull(NumericVector& x, NumericMatrix& posterior,
                           NumericVector& status) {

  int nMixures = posterior.ncol();
  //Store parameters column-wise for each cluster
  NumericMatrix parameter(2, nMixures);

  for (int i = 0; i < nMixures; i++) {
    NumericVector p_i = posterior( _, i);
    double beta_i = NewtonRaphson(x, p_i, status);
    double lambda_i = getLambda(x, p_i, status, beta_i);

    // Test lambda und beta vertauscht
    parameter(0, i) = lambda_i;
    parameter(1, i) = beta_i;
  }
  return parameter;
}

// [[Rcpp::export]]
double weibullDensity(double x, double beta, double lambda, double censored) {

  double y = x / lambda;
  double s = pow(y, beta - 1.0);
  double t = pow(y, beta);

  if(censored < 1.0) {
    return exp(-t);
  }
  return (beta / lambda) * s * exp(-t);
}

// [[Rcpp::export]]
void LikelihoodWeibull(NumericVector& x, NumericMatrix& parameter,
                       NumericVector& status, NumericVector& prior,
                       NumericMatrix& P,NumericMatrix& logL) {

  int nMixures = parameter.ncol();

  for (int i = 0; i < x.length(); i++) {
    for (int j = 0; j < nMixures; j++) {
      double p = weibullDensity(x(i), parameter(1, j), parameter(0, j), status(i));
      logL(i,j) = log(p);
      P(i,j) = p * prior(j);
    }
  }
}

// [[Rcpp::export]]
void normalize(NumericMatrix& M) {

  NumericVector norm = rowSums(M);
  for (int i = 0; i < M.rows(); ++i) {
    M(i,_) = M(i,_) / norm[i];
  }
}

double logLikelihood(arma::mat& posterior, arma::mat& logDensity,
                     arma::vec& prior) {

  double Q = arma::accu(posterior % logDensity);
  double r = arma::accu(posterior * prior);

  return Q + r;
}

//' EM-Algorithm using Newton-Raphson Method
//'
//' This method uses the EM-Algorithm to estimate the parameters of a univariate
//' mixture model. Until now, the mixture model can consist of k two-parametric
//' Weibull distributions. The Weibull distributions are parameterized with scale
//' \eqn{\eta} and shape \eqn{\beta}. In M-step these parameters are estimated using
//' Newton-Raphson. This function is implemented in c++ and is called in function
//' \code{\link{mixmod_em}}.
//'
//' @encoding UTF-8
//' @references Doganaksoy, N.; Hahn, G.; Meeker, W. Q., Reliability Analysis by
//'   Failure Mode, Quality Progress, 35(6), 47-52, 2002
//'
//' @param x a numeric vector which consists of lifetime data. Lifetime
//'  data could be every characteristic influencing the reliability of a product,
//'  e.g. operating time (days/months in service), mileage (km, miles), load
//'  cycles.
//' @param event a vector of binary data (0 or 1) indicating whether unit \emph{i}
//'   is a right censored observation (= 0) or a failure (= 1).
//' @param post a numeric matrix specifiying initial a-posteriori probabilities.
//'   The number of rows have to be in line with observations \code{x} and the
//'   number of columns must equal the mixture components \code{k}.
//' @param distribution supposed distribution of mixture model components.
//'   The value must be \code{"weibull"}. Other distributions have not been
//'   implemented yet.
//' @param k integer of mixture components, default is 2.
//' @param method default method is \code{"EM"}. Other methods have not been
//'   implemented yet.
//' @param n_iter integer defining the maximum number of iterations.
//' @param conv_limit numeric value defining the convergence limit.
//'
//' @return Returns a list with the following components:
//'   \itemize{
//'   \item \code{coefficients} : A matrix with estimated Weibull parameters. In the
//'     first row the estimated scale parameters \eqn{\eta} and in the second the
//'     estimated shape parameters \eqn{\beta} are provided. The first column belongs
//'     to the first mixture component and so forth.
//'   \item \code{posteriori} : A matrix with estimated a-posteriori probabilities.
//'   \item \code{priori} : A vector with estimated a-priori probabilities.
//'   \item \code{logL} : The value of the complete log-likelihood.}
//' @export
//' @examples
//' # Data is taken from given reference:
//' hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
//'           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
//'           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
//'           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
//'           226, 278, 314, 328, 377)
//' state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
//'          1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
//'          1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//'          0, 1, 1, 1, 1, 1, 1)
//' post_dirichlet <- LearnBayes::rdirichlet(n = length(hours),
//'                                          par = rep(.1, 2))
//' mix_mod_em <- mixture_em_cpp(x = hours,
//'                              event = state,
//'                              post = post_dirichlet,
//'                              distribution = "weibull",
//'                              k = 2,
//'                              method = "EM",
//'                              n_iter = 150)
//'
// [[Rcpp::export]]
List mixture_em_cpp(NumericVector& x, NumericVector& event, NumericMatrix post,
                    String distribution = "weibull", int k = 2,
                    String method = "EM", int n_iter = 100,
                    double conv_limit = 1e-6) {

  int n = x.length();

  NumericMatrix posterior = clone(post);
  NumericMatrix logL(n, k); // Likelihood P_ij of the data i belonging to sub-population j

  NumericVector prior(k, 1.0); // Mixing weight (relevance of cluster)
  prior = prior / k;

  NumericMatrix parameter(2, k);

  //double logLikelihood_old = -std::numeric_limits<double>::infinity();
  double logLikelihood_old = 0;
  for (int iter = 0; iter < n_iter; ++iter) {

    //######## M-Step ###########
    if (method == "EM") {
      parameter = MStepWeibull(x, posterior, event);
    }

    //######## E-Step ###########
    arma::mat posterior_old = as<arma::mat>(posterior);

    LikelihoodWeibull(x, parameter, event, prior, posterior, logL);

    normalize(posterior);
    prior = colMeans(posterior);

    arma::mat logDensity = as<arma::mat>(logL);

    NumericVector logPrior = log(prior);
    arma::vec logPrio = as<arma::vec>(logPrior);

    double logLikelihood_new =logLikelihood(posterior_old, logDensity, logPrio);

    double convCrit = (std::abs(logLikelihood_new - logLikelihood_old) /
                      (std::abs(logLikelihood_old) + 0.001 * conv_limit));

    if (convCrit < conv_limit) {
      // Rcout << "For given convergence limit, the algorithm is converged."<< std::endl;
      logLikelihood_old = logLikelihood_new;
      break;
    }

    if (iter == n_iter - 1) {
      Rcpp::warning("Maximum number of iterations was reached.");
    }

    logLikelihood_old = logLikelihood_new;

  }
  // return parameter;
  return List::create(
    _["coefficients"]  = parameter,
    _["posteriori"]  = posterior,
    _["priori"] = prior,
    _["logL"] = logLikelihood_old
  );
}
