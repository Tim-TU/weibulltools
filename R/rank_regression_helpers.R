# Helper function that performs lm():
lm_ <- function(x,
                y,
                distribution,
                direction = c("x_on_y", "y_on_x")
) {

  distribution <- two_parametric(distribution)
  direction <- match.arg(direction)

  # Switch between distributions:
  switch(distribution,
         "weibull" = {
           x <- log(x)
           y <- qsev(y)
         },
         "sev" = {
           y <- qsev(y)
         },
         "lognormal" = {
           x <- log(x)
           y <- stats::qnorm(y)
         },
         "normal" = {
           y <- stats::qnorm(y)
         },
         "loglogistic" = {
           x <- log(x)
           y <- stats::qlogis(y)
         },
         "logistic" = {
           y <- stats::qlogis(y)
         }
  )

  if (direction == "x_on_y") {
    stats::lm(x ~ y)
  } else {
    stats::lm(y ~ x)
  }
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
conf_HC <- function(dist_params, # loc-scale-parameters
                    dist_varcov, # loc-scale-var-cov
                    conf_level,
                    n, # sample size
                    direction
) {

  colnames(dist_varcov) <- rownames(dist_varcov) <- names(dist_params)[1:2]

  # Standard errors:
  dist_se <- sqrt(diag(dist_varcov))

  # Studentized confidence intervals:
  p_conf <- c((1 - conf_level), (1 + conf_level)) / 2
  q_t <- stats::qt(
    p = p_conf,
    df = n - 2
  )

  conf_mu <- dist_params[["mu"]] + q_t * dist_se[[1]]
  conf_sigma <- dist_params[["sigma"]] + q_t * dist_se[[2]]

  # Consider direction:
  if (direction == "y_on_x") {
    ## Back-transformed parameters:
    dist_params[1:2] <- c(
      -dist_params[[1]] / dist_params[[2]],
      1 / dist_params[[2]]
    )

    ## Back-transformed confidence intervals:
    conf_sigma <- rev(1 / conf_sigma)
    conf_mu <- -conf_mu * conf_sigma
  }

  # Form confidence interval matrix:
  conf_int <- matrix(c(conf_mu, conf_sigma), byrow = TRUE, ncol = 2)
  colnames(conf_int) <- paste(p_conf * 100, "%")

  if (length(dist_params) > 2L) {
    conf_int <- rbind(conf_int, c(NA, NA))
  }

  rownames(conf_int) <- names(dist_params)

  # Return a list:
  list(
    coefficients = dist_params,
    confint = conf_int,
    varcov = dist_varcov
  )
}
