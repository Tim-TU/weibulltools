
#' Title
#'
#' @param x
#' @param y
#' @param event
#' @param distribution
#' @param conf_level
#' @param details
#'
#' @return
#' @export
#'
#' @examples
rank_regression_boot <- function(x, y, event, distribution = c("weibull", "lognormal", "loglogistic"),
                            conf_level = .95, details = TRUE) {
  x_f <- x[event == 1]
  y_f <- y[event == 1]

  if (distribution == "weibull") {

    mrr <- stats::lm(log(x_f) ~ SPREDA::qsev(y_f))

    mrr_boot <- hcci::Pboot(model = mrr, significance = 1 - conf_level, J = 1000, K = 100)

    mu <- mrr_boot$beta[[1]]
    conf_mu <- c(mrr_boot$ci_lower_simple[[1]], mrr_boot$ci_upper_simple[[1]])

    sigma <- mrr_boot$beta[[2]]
    conf_sigma <- c(mrr_boot$ci_lower_simple[[2]], mrr_boot$ci_upper_simple[[2]])

    estimates_loc_sc <- c(mu, sigma)
    names(estimates_loc_sc) <- c("mu", "sigma")

    conf_eta <- c(exp(conf_mu[[1]]), exp(conf_mu[[2]]))
    conf_beta <- c(1 / conf_sigma[[2]],  1 / conf_sigma[[1]])

    conf_ints <- matrix(c(conf_eta, conf_beta), byrow = TRUE, ncol = 2)
    colnames(conf_ints) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                             paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints) <- names(estimates)

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sigma), byrow = TRUE,
                               ncol = 2)
    colnames(conf_ints_loc_sc) <- colnames(conf_ints)
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    r_sq <- summary(mrr)$r.squared


    if (details == TRUE) {
      mrr_output <- list(coefficients = estimates, confint = conf_ints,
                         loc_sc_coefficients = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         r_squared = r_sq)
    } else {
      mrr_output <- list(coefficients = estimates)
    }
  } else {
    if (distribution == "lognormal") {
      mrr <- stats::lm(log(x_f) ~ stats::qnorm(y_f))
    } else if (distribution =="loglogistic") {
      mrr <- stats::lm(log(x_f) ~ stats::qlogis(y_f))
    } else {
      stop("No valid distribution!")
    }
    estimates_loc_sc <- c(stats::coef(mrr)[[1]], stats::coef(mrr)[[2]])
    names(estimates_loc_sc) <- c("mu", "sigma")

    vcov <- sandwich::vcovHC(x = mrr, type = "HC1") #Änderungen
    se <- sqrt(diag(vcov))
    conf_mu <- c(
      estimates_loc_sc[[1]] + stats::qt((1 + conf_level) / 2, df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)),
      estimates_loc_sc[[1]] - stats::qt((1 + conf_level) / 2, df = length(x_f) - 2) * se[[1]] / sqrt(length(x_f)))

    conf_sig <- c(
      estimates_loc_sc[[2]] + stats::qt((1 + conf_level) / 2, df = length(x_f) - 2) * se[[2]] / sqrt(length(x_f)),
      estimates_loc_sc[[2]] - stats::qt((1 + conf_level) / 2, df = length(x_f) - 2) * se[[2]] / sqrt(length(x_f)))

    conf_ints_loc_sc <- matrix(c(conf_mu, conf_sig), byrow = TRUE,
                               ncol = 2)
    colnames(conf_ints_loc_sc) <- c(paste(((1 - conf_level) / 2) * 100, "%"),
                                    paste(((1 + conf_level) / 2) * 100, "%"))
    rownames(conf_ints_loc_sc) <- names(estimates_loc_sc)

    r_sq <- summary(mrr)$r.squared

    if (details == TRUE) {
      mrr_output <- list(loc_sc_coefficients = estimates_loc_sc,
                         loc_sc_confint = conf_ints_loc_sc,
                         r_squared = r_sq)
    } else {
      mrr_output <- list(loc_sc_coefficients = estimates_loc_sc)
    }
  }
  return(mrr_output)
}
