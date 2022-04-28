N_SEQ <- seq(2, 100)               # size of gatherings
PHI_SEQ <- seq(0.1, 1, 0.2)        # dispersion parameter 
R0_SEQ <- seq(0.5, 2.5, 0.5)       # mean number of secondary cases

sim1_params <-
  expand.grid(
    N = N_SEQ, 
    phi = PHI_SEQ,
    r0 = R0_SEQ
  )

pb <- txtProgressBar(max = nrow(sim1_params), initial = NA, style = 3)

sim1_results <-
  pmap_dfr(
    as.list(sim1_params),
    function(N, phi, r0) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      
      sim <- dgp_one_infected(
        N,
        sims = SIMS,
        nu_dist = function(x)
          rgamma(x, phi, phi /  r0)
      )
      summarize_dgp(sim$r_eff)
    }
  )

sim1 <- cbind(sim1_params, sim1_results)