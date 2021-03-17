# Helper function that performs lm():
lm_ <- function(x,
                y,
                distribution,
                direction = c("x_on_y", "y_on_x")
) {

  direction <- match.arg(direction)
  distribution <- std_parametric(distribution)

  # Prepare x and y for `lm()`:
  y <- q_std(y, distribution)

  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x <- log(x)
  }

  # Create formula to be more flexible w.r.t 'distribution' and 'direction':
  ## Replace everything between the two underscores:
  fm_chr <- sub("_[^>]+_", " ~ ", direction)

  ## If distribution is 'exponential', the intercept is 0:
  if (distribution == "exponential") {
    fm_chr <- paste(fm_chr, "- 1")
  }

  ## Define formula:
  fm <- stats::formula(fm_chr)

  ## Apply `lm()` to defined formula:
  stats::lm(fm)
}



# Approximated confidence intervals for parameters according to Mock:
conf_mock <- function(dist_params, # loc-scale-parameters
                      conf_level,
                      n, # sample size
                      direction
) {

  # Check for conf_level:
  if (!(conf_level %in% c(0.9, 0.95, 0.99))) {
    stop(
      "'conf_level' must be 0.90, 0.95 or 0.99",
      call. = FALSE
    )
  }

  # Lookup:
  conf <- as.character(conf_level)
  mock_val <- c("0.9" = 1.4, "0.95" = 2, "0.99" = 3.4)[[conf]]

  # Consider direction:
  if (direction == "y_on_x") {
    ## Back-transformed parameters:
    dist_params[1:2] <- c(
      -dist_params[[1]] / dist_params[[2]],
      1 / dist_params[[2]]
    )
  }

  # Alternative parameters for Weibull:
  estimates <- dist_params
  estimates[1:2] <- c(exp(estimates[[1]]), 1 / estimates[[2]])
  names(estimates)[1:2] <- c("eta", "beta")

  # Confidence intervals:
  p_conf <- c((1 + conf_level), (1 - conf_level)) / 2
  q_chi <- stats::qchisq(
    p = p_conf,
    df = 2 * n + 2
  )

  conf_beta <- estimates[["beta"]] *  c(
    1 / (1 + sqrt(mock_val / n)),
    (1 + sqrt(mock_val / n))
  )

  conf_eta <- estimates[["eta"]] * (2 * n / q_chi) ^ (1 / estimates[["beta"]])

  # Form confidence interval matrix:
  conf_int <- matrix(c(conf_eta, conf_beta), byrow = TRUE, ncol = 2)
  colnames(conf_int) <- paste(rev(p_conf) * 100, "%")

  if (length(estimates) > 2L) {
    conf_gamma <- estimates[["gamma"]] * (2 * n / q_chi) ^ (1 / estimates[["beta"]])
    conf_int <- rbind(conf_int, conf_gamma)
  }

  rownames(conf_int) <- names(estimates)

  # Alternative confidence interval matrix:
  conf_int_loc_sc <- conf_int
  conf_int_loc_sc[1, ] <- log(conf_int_loc_sc[1, ])
  conf_int_loc_sc[2, ] <- rev(1 / conf_int_loc_sc[2, ])

  rownames(conf_int_loc_sc) <- names(dist_params)

  # Return a list:
  list(
    coefficients = dist_params,
    confint = conf_int_loc_sc,
    shape_scale_coefficients = estimates,
    shape_scale_confint = conf_int
  )
}



# Confidence intervals for parameters using a HC standard errors:
conf_HC <- function(dist_params, # loc-scale-parameters (or scale-parameter).
                    dist_varcov, # loc-scale-var-cov (or scale-variance).
                    distribution, # Setting the parameter name for exponential.
                    conf_level,
                    n, # sample size
                    direction
) {

  n_par <- nrow(dist_varcov)
  colnames(dist_varcov) <- rownames(dist_varcov) <- names(dist_params)[1:n_par]

  # Standard errors:
  dist_se <- sqrt(diag(dist_varcov))

  # Studentized confidence intervals:
  p_conf <- c((1 - conf_level), (1 + conf_level)) / 2
  q_t <- stats::qt(
    p = p_conf,
    df = n - 2
  )

  # Compute confidence intervals using `sapply()` (matrix with named columns):
  conf_int <- sapply(
    names(dist_se),
    function(x) dist_params[[x]] + q_t * dist_se[[x]]
  )

  # Consider direction, i.e. back-transformation for parameters and intervals:
  if (direction == "y_on_x") {
    ## For scale sigma:
    dist_params[["sigma"]] <- 1 / dist_params[["sigma"]]
    conf_int[, "sigma"] <-  rev(1 / conf_int[, "sigma"])

    ## For location mu, if exists:
    if (n_par == 2L) {
      dist_params[["mu"]] <- -dist_params[["mu"]] * dist_params[["sigma"]]
      conf_int[, "mu"] <-  -conf_int[, "mu"] * conf_int[, "sigma"]
    }
  }

  # Prepare matrix with confidence intervals for output:
  ## Force sorting of interval (prevent case that conf_mu[1] > conf_mu[2])
  conf_int <- t(apply(conf_int, 2, sort))

  ## Set column names:
  colnames(conf_int) <- paste(p_conf * 100, "%")

  ## Exponential distribution:
  if (std_parametric(distribution) == "exponential") {
    names_exp <- "theta"

    ### Set name:
    names(dist_params)[n_par] <- rownames(conf_int) <- names_exp
    dimnames(dist_varcov) <- rep(list(names_exp), 2)
  }

  ## Three-parametric case:
  if (length(dist_params) > n_par) {
    conf_int <- rbind(conf_int, c(NA, NA))
    rownames(conf_int)[n_par + 1] <- names(dist_params[n_par + 1])
  }

  ## Alternative parameters and confidence intervals for Weibull:
  l_wb <- list()

  ## Weibull distribution; providing shape-scale coefficients and confint:
  if (distribution %in% c("weibull", "weibull3")) {
    estimates <- to_shape_scale_params(dist_params)
    confint <- to_shape_scale_confint(conf_int)

    l_wb <- list(
      shape_scale_coefficients = estimates,
      shape_scale_confint = confint
    )
  }

  # Return a list:
  c(
    list(
      coefficients = dist_params,
      confint = conf_int
    ),
    l_wb, # Empty, if not Weibull!
    list(varcov = dist_varcov)
  )
}
