library(tidyverse)

binom_pmf <- function(x, size, p) dbinom(x, size, p)
betabinom_pmf <- function(x, size, p, phi) rmutil::dbetabinom(x, size, p, phi)

df <- 
  expand.grid(
    x = seq(1, 5),
    N = 2:30,
    pi = c(0.001, 0.005, 0.01, 0.05)
  )

df$px <- mapply(binom_pmf, x = df$x, size = df$N, p = df$pi)

pdf("3_results/plot4.pdf", width = 10, height = 6)
ggplot(df, aes(x = N, y = px, group = factor(x), color = factor(x))) +
  geom_line() +
  facet_grid(~factor(pi, labels = paste0("p[i] == ", c(0.001, 0.005, 0.01, 0.05))), labeller = label_parsed) +
  scale_color_brewer(name = "Number of infecteds\nin attendance", type = "qual", palette = "Set1", labels = paste0("I = ", 1:5)) +
  coord_cartesian(expand = FALSE) +
  our_theme() +
  ylab(bquote("Probability")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))
dev.off()

source("0_functions/simulation_functions.R")
source("0_functions/plot_functions.R")

# simulation globals ------------------------------------------------------

SIMS <- 100000

N_SEQ <- seq(2, 30)                    # size of gatherings
PI_SEQ <- c(0.001, 0.005, 0.01, 0.05)  # population prevalence of infection
PS_SEQ <- 1 - PI_SEQ                   # population prevalence of susceptibles


# simulation 1 - use HK parameters, vary N and pi -------------------------

sim1_params <-
  expand.grid(
    N = N_SEQ,
    pi = PI_SEQ
  )

sim1_params$ps <- 1 - sim1_params$pi  

pb <- txtProgressBar(max = nrow(sim1_params), initial = NA, style = 3)

sim1_results <-
  pmap_dfr(
    as.list(sim1_params),
    function(N, ps, pi) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      sim <- dgp(N, ps, pi, sims = SIMS, type = "probability", prior = function(x) rep(0.08, x))
      list(
        "r_eff" = mean(sim$r_eff),
        "delta" = mean(sim$delta)
      )
    }
  )

sim1 <- cbind(sim1_params, "mean" = sim1_results$r_eff)

pdf("3_results/plot5.pdf", width = 10, height = 6)
ggplot(sim1, aes(x = N, y = mean, group = factor(pi), color = factor(pi))) +
  geom_smooth(se = FALSE) +
  scale_color_brewer(name = bquote("Community Prevalence ("*p[i]*")"), type = "qual", palette = "Set1") +
  coord_cartesian(expand = FALSE) +
  gatherings_theme() +
  ylab(bquote("Expected secondary cases per index case ("*R['g']*")")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))
dev.off()
  
sim2 <- cbind(sim1_params, "mean" = sim1_results$delta)

pdf("3_results/plot6.pdf", width = 10, height = 6)
ggplot(sim2, aes(x = N, y = mean, group = factor(pi), color = factor(pi))) +
  geom_smooth(se = FALSE) +
  scale_color_brewer(name = bquote("Community Prevalence ("*p[i]*")"), type = "qual", palette = "Set1") +
  coord_cartesian(expand = FALSE) +
  gatherings_theme() +
  ylab(bquote("Expected incident cases ("*E(X[i])*")")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))
dev.off()  
  