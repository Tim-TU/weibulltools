SimMixNZ <- function(n, k, anteile, beta, eta) {

  gruppen <- sample(1:k, prob = anteile, size = n, replace = TRUE)

  mus <- log(eta)

  sigmas <- 1 / beta

  datenMix <- exp(mus[gruppen] + SPREDA::rsev(n) * sigmas[gruppen])

  zens <- rep(1, length(n))

  dfGes <- data.frame(Beobachtung = datenMix, Status = zens, Unterverteilung = gruppen)

}



mix2 <- SimMixNZ(n = 100, k = 2, anteile = c(.3, .7), beta = c(1, 3), eta = c(100, 10000))

mix3 <- SimMixNZ(n = 100, k = 3, anteile = c(.3, .3, .4), beta = c(1, 3, 3), eta = c(1, 100, .01))

mix4 <- SimMixNZ(n = 100, k = 3, anteile = c(.1, .5, .4), beta = c(0.6, 1, 4), eta = c(2000, 30000, 10000))


mixmod_plot <- function(x, y, event, loc_sc_params) {
  plot_prob(x = x, y = y, event = event) %>% plot_mod(x = x, loc_sc_params = c(loc_sc_params[1:2])) %>%
    plot_mod(x = x, loc_sc_params = c(loc_sc_params[3:4])) %>% plot_mod(x = x, loc_sc_params = c(loc_sc_params[5:6]))
}
