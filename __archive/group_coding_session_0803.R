library(ggplot2)

df <- 
  data.frame(
  x = rbeta(10000, 80, 920)
)  

ggplot(df, aes(x = x)) + geom_histogram()

mean(df$x)



dgp(10, 0.9, 0.1, sims = 10, type = "probability")




library(tidyverse)
library(deSolve)

source("simulation_functions.R")


# simulation globals ------------------------------------------------------

SIMS <- 10000

N_SEQ <- seq(2, 30)                  # size of gatherings
pi <- c(0.005, 0.01, 0.05, 0.10) 
ps <- 1 - pi

disp <- c(1, 10, 100)
# PI_SEQ <- seq(0.001, 0.09, 0.001)  # population prevalence of infection
# PS_SEQ <- 1 - PI_SEQ               # population prevalence of susceptibles
# (NOTE: for now assume no recovereds)  

# PHI_SEQ <- seq(0.2, 1, 0.2)        # dispersion parameter 
# R0_SEQ <- seq(0.5, 2.5, 0.5)       # mean number of secondary cases


# simulation 1 - use HK parameters, vary N and pi -------------------------

sim1_params <-
  expand.grid(
    N = N_SEQ,
    pi = pi,
    disp = disp
  )

sim1_params$ps <- 1 - sim1_params$pi

pb <- txtProgressBar(max = nrow(sim1_params), initial = NA, style = 3)

sim1_results <-
  pmap_dfr(
    as.list(sim1_params),
    function(N, ps, pi, disp) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      
      sim <-
        dgp(
          N,
          ps,
          pi,
          sims = SIMS,
          type = "probability",
          prior = function(x)
            rbeta(x, 0.08 * disp, 0.92 * disp)
        )
      
      return(
        list("delta" = sim$delta)
      )
      #summarize_dgp(sim$r_eff)
      # list(
      #   "r_eff" = mean(sim$r_eff),
      #   "delta" = mean(sim$delta)
      # )
    }
  )

close(pb)


#r_eff <- sapply(sim1_results, get, x = "r_eff")
# delta <- sapply(sim1_results, get, x = "delta")
# 
# colnames(delta) <- paste0("delta_", 2:30)

# delta <-
#   pivot_longer(as_tibble(delta), everything(), values_to = "delta") %>%
#   separate(name, c("variable", "N"), sep = "_") %>%
#   select(-variable) %>%
#   mutate(N = as.numeric(N))



sim1_plot <- sim1_params[rep(seq_len(nrow(sim1_params)), each = SIMS), ]

sim1_plot$delta <- sim1_results$delta


ggplot(sim1_plot, aes(
  x = N,
  y = delta,
  fill = factor(disp),
  color = factor(disp)
)) + geom_smooth() + facet_grid(~factor(pi))


sim1_plot %>% 
  filter(N == 10 | N == 20) %>%
  group_by(N, pi) %>%
  summarise(across(everything(), mean))
  

ggplot(filter(sim1_plot, N == 10 | N == 20), aes(
  x = factor(N),
  y = delta,
  fill = factor(disp),
  color = factor(disp)
)) + geom_violin() + facet_grid(~factor(pi))

  