# simulation --------------------------------------------------------------

# simulation parameters
N_SEQ <- c(20000)                       # size of population
PI_SEQ <- c(0.01, 0.025, 0.05)          # population prevalence of infection
PR_SEQ <- seq(0, 0.20, 0.10)            # population prevalence of immunity
PHI_SEQ <- c(0.1, 1, 10, 100)           # range of dispersion parameter phi
C_SEQ <- c(5, 10, 20, 50)               # range of limitations in gathering size 
OPT_SEQ <- c("0", "1", "2", "3")
SIMS <- 1000

# initialize parameter grid
sim_params <-
  expand_grid(
    N = N_SEQ,
    pi = PI_SEQ,
    pr = PR_SEQ,
    phi = PHI_SEQ,
    c = C_SEQ,
    option = OPT_SEQ,
    SIM = 1:SIMS
  )

if (rerun_simulation) {
  # plan for parallel multiprocess
  future::plan(future::multiprocess, workers = 4)
  
  # run simulations for full parameter grid
  tictoc::tic()
  sim_results <-
    furrr::future_pmap_dfr(as.list(sim_params),
             function(N, pi, pr, phi, c, option, SIM) {
               sim <-
                 simulate_gatherings(
                   N,
                   pi,
                   pr,
                   c = c,
                   option = option,
                   dist = dist_m$M,
                   wt = dist_m$prob,
                   prior = function (x)
                     rbeta(x, 0.08 * phi, phi * (1 - 0.08))
                 )
               list(R_g = mean(sim$X_eff),
                    total_cases = sum(sim$delta))
             }, .progress = TRUE)
  tictoc::toc()
  
  # bind with simulation parameters
  sim_results <- cbind(sim_params, sim_results)
  
  # save a copy
  write_rds(sim_results, "1_data/sim_results.rds")
} else {
  
  # read in saved results
  sim_results <- read_rds(sim_results, "1_data/sim_results.rds")
} 



# plots: expected secondary cases per index case (Rg) ----------------------

ggplot(sim_results, aes(x = pi, y = X_eff, color = pr)) +
  geom_point() +
  facet_grid(N~phi)

sim_results %>%
  filter(phi == 100) %>%
  group_by(c, pi, pr, option) %>%
  summarise(X_eff = quantile(X_eff, prob = 0.9)) %>%
ggplot(., aes(x = factor(pi), y = factor(pr), fill = X_eff)) +
  geom_tile() +
  geom_label(aes(label = round(X_eff, 2)), label.size = NA) +
  facet_grid(c~option) + 
  scale_fill_distiller() +
  gatherings_theme() +
  coord_cartesian(expand = F) +
  theme(legend.position = "none") +
  labs(
    x = "Infectious (%)",
    y = "Immune (%)"
  )

sim_results %>%
  filter(pi == 0.01 & pr == 0) %>%
  ggplot(., aes(x = factor(option), y = X_eff, fill = factor(phi))) +
    geom_violin(draw_quantiles = c(0.5)) +
    facet_grid(c~phi) + 
    gatherings_theme() +
    scale_fill_brewer() +
    theme(legend.position = "none") +
    labs(
      x = "Restriction type",
      y = bquote(R[g])
    )

sim_results %>%
  filter(pi == 0.01 & pr == 0) %>%
  ggplot(., aes(x = factor(option), y = delta, fill = factor(phi))) +
  geom_violin(draw_quantiles = c(0.5)) +
  facet_grid(c~phi) + 
  gatherings_theme() +
  scale_fill_brewer() +
  theme(legend.position = "none") +
  labs(
    x = "Restriction type",
    y = bquote(R[g])
  )

sim_results %>%
  filter(phi == 0.1) %>%
  group_by(c, pi, pr, option) %>%
  summarise(delta = mean(delta)) %>%
  ggplot(., aes(x = factor(pi), y = factor(pr), fill = delta)) +
  geom_tile() +
  geom_label(aes(label = round(delta, 3)), label.size = NA) +
  facet_grid(c~option) + 
  scale_fill_distiller() +
  gatherings_theme() +
  coord_cartesian(expand = F) +
  theme(legend.position = "none") +
  labs(
    x = "Infectious (%)",
    y = "Immune (%)"
  )


head(sim_results)
