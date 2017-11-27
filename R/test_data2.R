SimMixNZ <- function(n, k, anteile, beta, eta) {

  gruppen <- sample(1:k, prob = anteile, size = n, replace = TRUE)

  mus <- log(eta)

  sigmas <- 1 / beta

  datenMix <- exp(mus[gruppen] + rsev(n) * sigmas[gruppen])

  zens <- rep(1, length(n))

  dfGes <- data.frame(Beobachtung = datenMix, Status = zens, Unterverteilung = gruppen)

}



mix2 <- SimMixNZ(n = 100, k = 2, anteile = c(.3, .7), beta = c(1, 3), eta = c(100, 10000))



mix3 <- SimMixNZ(n = 100, k = 3, anteile = c(.3, .3, .4), beta = c(1, 3, 3), eta = c(1, 100, .01))
