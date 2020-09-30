# simulation --------------------------------------------------------------

# simulation parameters
N_SEQ <- c(1000, 5000, 10000)           # size of population
PI_SEQ <- seq(0.005, 0.015, 0.005)      # population prevalence of infection
PR_SEQ <- seq(0, 0.20, 0.05)            # population prevalence of immunity
PHI_SEQ <- c(0.1, 1, 10, 100)           # range of dispersion parameter phi
L_SEQ <- seq(10, 50, 10)                # range of limitations in gathering size 
SIMS <- 1000

# initialize parameter grid
sim_params <-
  expand_grid(
    N = N_SEQ,
    pi = PI_SEQ,
    pr = PR_SEQ,
    phi = PHI_SEQ,
    SIM = 1:SIMS
  )

if (rerun_simulation) {
  # plan for parallel multiprocess
  future::plan(future::multiprocess, workers = 4)
  
  # run simulations for full parameter grid
  tictoc::tic()
  sim_results <-
    furrr::future_pmap_dfr(as.list(sim_params),
             function(N, pi, pr, phi, SIM) {
               sim <-
                 simulate_gatherings(
                   N,
                   pi,
                   pr,
                   dist = dist$M,
                   wt = dist$prob,
                   prior = function (x)
                     rbeta(x, 0.08 * phi, phi * (1 - 0.08))
                 )
               list(X_eff = mean(sim$X_eff),
                    delta = mean(sim$delta))
             }, .progress = TRUE)
  tictoc::toc()
}

sim_results <- cbind(sim_params, sim_results)

# plots: expected secondary cases per index case (Rg) ----------------------



