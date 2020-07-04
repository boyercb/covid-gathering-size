library(tidyverse)
library(deSolve)

source("simulation_functions.R")


# simulation globals ------------------------------------------------------

SIMS <- 10000

N_SEQ <- seq(2, 100)               # size of gatherings
PI_SEQ <- seq(0.001, 0.09, 0.001)  # population prevalence of infection
PS_SEQ <- 1 - PI_SEQ               # population prevalence of susceptibles
                                   # (NOTE: for now assume no recovereds)  

PHI_SEQ <- seq(0.2, 1, 0.2)        # dispersion parameter 
R0_SEQ <- seq(0.5, 2.5, 0.5)       # mean number of secondary cases

  
# simulation 1 - use HK parameters, vary N and pi -------------------------

sim1_params <-
  expand.grid(
    N = N_SEQ,
    pi = PI_SEQ
  )

sim1_params$ps <- 1 - sim1_params$pi  

sim1_results <-
  pmap_dfr(
    as.list(sim1_params),
    function(N, ps, pi) {
      sim <- dgp(N, ps, pi, sims = SIMS)
      summarize_dgp(sim)
    }
  )

sim1 <- cbind(sim1_params, sim1_results)


# simulation 2 - now also vary phi and r0 ---------------------------------

sim2_params <- 
  expand.grid(
    N = N_SEQ,
    pi = PI_SEQ,
    phi = PHI_SEQ,
    r0 = R0_SEQ
  )

sim2_params$ps <- 1 - sim2_params$pi  

pb <- txtProgressBar(max = nrow(df), initial = NA, style = 3)

sim2_results <-
  pmap_dfr(
    as.list(sim2_params),
    function(N, ps, pi, phi, r0) {
      
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      
      sim <-
        dgp(
          N = N,
          ps = ps,
          pi = pi,
          sims = SIMS,
          nu_dist = function(x)
            rgamma(length(x), x * phi, phi /  r0)
        )
      
      summarize_dgp(sim)
    })

close(pb)

sim2 <- cbind(sim2_params, sim2_results)


# Plots -------------------------------------------------------------------

# nicely formatted labels for plotting
r0_labs <- c(
  "0.5" = bquote(R[0] == 0.5),
  "1" = bquote(R[0] == 1),
  "1.5" = bquote(R[0] == 1.5),
  "2" = bquote(R[0] == 2),
  "2.5" = bquote(R[0] == 2.5)
)

phi_labs <- c(
  "0.2" = bquote(phi == 0.2),
  "0.4" = bquote(phi == 0.4),
  "0.6" = bquote(phi == 0.6),
  "0.8" = bquote(phi == 0.8),
  "1" = bquote(phi == 1)
)

# Plot 1: using HK parameters what is the expected number of secondary cases
# when varying N and pi
ggplot(sim1, aes(x = N, y = pi, fill = mean)) +
  geom_tile() +
  scale_fill_viridis_b(
    name = bquote("Expected\nnew cases E["*Delta*I*"]"),
    breaks = seq(1, 35, 1)
  ) +
  coord_cartesian(expand = F) +
  labs(
    x = "Size of gathering (N)",
    y = bquote("Prevalence of infection ("*p[i]*")")
  ) +
  guides(fill=guide_legend(ncol = 2))


# Plot 2: additionally vary dispersion and mean
ggplot(sim2, aes(x = N, y = pi, fill = mean)) +
  facet_grid(factor(r0, labels = r0_labs) ~ factor(phi, labels = phi_labs),
             labeller = label_parsed) +
  geom_tile() +
  scale_fill_viridis_c(
    name = bquote("Expected\nnew cases E[" * Delta * I * "]")
  ) +
  coord_cartesian(expand = F) +
  labs(
    x = "Size of gathering (N)",
    y = bquote("Prevalence of infection (" * p[i] * ")")
  ) +
  guides(fill=guide_legend(ncol = 2))


# Plot 3: check distributional effects in secondary cases
ggplot(sim2, aes(x = N, y = pi, fill = sd)) +
  facet_grid(factor(r0, labels = r0_labs) ~ factor(phi, labels = phi_labs),
             labeller = label_parsed) +
  geom_tile() +
  scale_fill_viridis_b(
    name = bquote("SD new\ncases SD[" * Delta * I * "]"),
    breaks = seq(1, 35, 1)
  ) +
  coord_cartesian(expand = F) +
  labs(
    x = "Size of gathering (N)",
    y = bquote("Prevalence of infection (" * p[i] * ")")
  ) +
  guides(fill=guide_legend(ncol = 2))


