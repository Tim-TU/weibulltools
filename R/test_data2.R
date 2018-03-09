# SimMixNZ <- function(n, k, anteile, beta, eta) {
#
#   gruppen <- sample(1:k, prob = anteile, size = n, replace = TRUE)
#
#   mus <- log(eta)
#
#   sigmas <- 1 / beta
#
#   datenMix <- exp(mus[gruppen] + SPREDA::rsev(n) * sigmas[gruppen])
#
#   zens <- rep(1, length(n))
#
#   dfGes <- data.frame(Beobachtung = datenMix, Status = zens, Unterverteilung = gruppen)
#
# }
#
#
#
# mix2 <- SimMixNZ(n = 100, k = 2, anteile = c(.3, .7), beta = c(1, 3), eta = c(100, 10000))
#
# mix3 <- SimMixNZ(n = 100, k = 3, anteile = c(.3, .3, .4), beta = c(1, 3, 3), eta = c(1, 100, .01))
#
# mix4 <- SimMixNZ(n = 100, k = 3, anteile = c(.2, .5, .3), beta = c(0.6, 1, 4), eta = c(20000, 30000, 10000))
#
#
#
#
# beobachtung <- c(2, 3, 5, 8, 13, 21, 28, 31, 31, 52, 53, 64, 67, 69, 76, 78, 104, 113, 119, 135, 144, 157, 160, 168, 179, 191, 203,
#                  211, 221, 226, 236, 241, 257, 261, 264, 278, 282, 284, 286, 298, 303, 314, 317, 318, 320, 327, 328, 328, 348, 348,
#                  350, 360, 369, 377, 387, 392, 412, 446)
#
# status <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#             1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
# sas_df <- data.frame(beobachtung, status)
#
# sas_john <- johnson_method(x = sas_df$beobachtung, event = sas_df$status)
#
# sas_mix <- mixmod_regression(x = sas_john$characteristic, y = sas_john$prob, event = sas_john$status)
#
# sas_plot <- plot_mixmod(x = sas_john$characteristic, y = sas_john$prob, event = sas_john$prob, mrr_output = sas_mix)
#
# sas_plot
#
#
#
# #Shock absorber failure data (see: Meeker & Escobar, Example 3.8, p. 59)
#
# ShAbs <- read.table("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat533_data/Splida_text_data/ShockAbsorber.txt", header=T)
#
#
#
# #failure=1, censored=0
#
# ShAbs$Cens <- as.numeric(ShAbs$Censor)-1
#
# head(ShAbs)
#
# obs   <- seq(10000, 100000, 10000)
# state <- c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0)
#
# df_john <- johnson_method(x = obs, event = state)
# mrr <- rank_regression(x = df_john$characteristic,
#                        y = df_john$prob,
#                        event = df_john$status,
#                        conf_level = .90)
