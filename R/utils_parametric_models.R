# Function that removes the trailing '3' of three-parameter distribution:
two_parametric <- function(distribution) {
  sub("3", "", distribution)
}



# Function that checks the correctness between input distribution and parameters:
check_dist_params <- function(dist_params, distribution) {
  three_parametric <- distribution %in%
    c("weibull3", "lognormal3", "loglogistic3")

  name_arg <- as.character(substitute(dist_params))

  if (three_parametric && length(dist_params) != 3) {
    stop(
      "A three-parametric distribution needs three parameters but ",
      sQuote(name_arg), " has length ", length(dist_params), ".",
      call. = FALSE
    )
  }

  if (!three_parametric && length(dist_params) != 2) {
    stop(
      "A two-parametric distribution needs two parameters but",
      " 'dist_params' has length ", length(dist_params), ".",
      call. = FALSE
    )
  }
}



# Function that standardizes lifetime characteristic depending on distribution:
standardize <- function(x, dist_params, distribution) {

  # Three-parametric model:
  if (length(dist_params) == 3L) {
    x <- x - dist_params[[3]]
  }

  # Two-parametric model with q or q - threshold:
  distribution <- two_parametric(distribution)

  # (log-)location-scale:
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x <- log(x)
  }

  # Standardize:
  z <- (x - dist_params[[1]]) / dist_params[[2]]
  z
}



# Quantile, probability and density functions of the smallest extreme value dist:
## quantile function:
qsev <- function(p) {
  p <- ifelse(p >= 0.9999999999999999, 0.9999999999999999, p)
  p <- ifelse(p <= 1 - 0.9999999999999999, 1 - 0.9999999999999999, p)

  log(-log(1 - p))
}



## probability function (cdf):
psev <- function(q) {
  1 - exp(-exp(q))
}



## density function (pdf):
dsev <- function(x) {
  exp(x - exp(x))
}



# Funcation that simulates a sample of a 'Dirichlet' distribution:
rdirichlet <- function(n, par) {
  k <- length(par)
  z <- matrix(0, nrow = n, ncol = k)
  s <- matrix(0, nrow = n)
  for (i in 1:k) {
    z[, i] <- stats::rgamma(n, shape = par[i])
    s <- s + z[, i]
  }
  for (i in 1:k) {
    z[, i] <- z[, i]/s
  }
  return(z)
}
