# Representative list of all kind of models
get_models <- function() {
  distribution <- "weibull"

  rel_tbl <- reliability_data(voltage, hours, status)
  cdf_tbl <- estimate_cdf(rel_tbl, "johnson")

  # Rank regression
  rr <- rank_regression(cdf_tbl, distribution)

  # ML estimation
  ml <- ml_estimation(rel_tbl, distribution)

  # Mixmod regression
  mmr <- mixmod_regression(cdf_tbl, distribution)

  # Mixmod EM
  mme <- mixmod_em(rel_tbl, distribution)

  cdf_mult_tbl <- estimate_cdf(rel_tbl, methods = c("johnson", "nelson"))

  # Rank regression list
  rr_mult <- rank_regression(cdf_mult_tbl, distribution)

  # Mixmod regression list
  mmr_mult <- mixmod_regression(cdf_mult_tbl, distribution)

  list(
    rr = rr,
    ml = ml,
    mmr = mmr,
    mme = mme,
    rr_mult = rr_mult,
    mmr_mult = mmr_mult
  )
}
