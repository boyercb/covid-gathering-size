# how long do we have to hold for? ----------------------------------------

how_long_params <- 
  expand_grid(
    pi_0 = c(0.001, 0.005, 0.01, 0.02),
    pr_0 = c(0, 0.10, 0.20),
    c_work = c(NA, 1)
  )

if (rerun_simulation) {
  # plan for parallel multiprocess
  future::plan(future::multiprocess, workers = 4)
  
  # run simulations for full parameter grid
  tictoc::tic()
  
  how_long <- 
    pmap_dfr(as.list(how_long_params), function(pi_0, pr_0, c_work) {
      map_dfr(1:10000, function(x) {
        
        pop <- 100000L
        N <- pi_0 * pop
        Nvec <- vector()
        
        pi <- pi_0
        pr <- pr_0
        i <- 1
        
        while(i <= 75) {
          N <-
            offspring_model(
              N = N,
              k_work = k_work,
              k_gather = k_gather,
              k_home = k_home,
              pi = pi,
              pr = pr,
              mu_home = 0.17,
              c_gather = 4,
              c_work = if(!is.na(c_work)) c_work else NULL,
              option = "3"
            ) %>% colSums() %>% sum()
          Nvec[i] <- N
          pi <- N / pop
          if (i > 1) {
            pr <- sum(Nvec[1:(i-1)]) / pop + pr_0
          }
          i <- i + 1
        }
        return(data.frame(
          pi_0 = pi_0,
          pr_0 = pr_0,
          c_work = c_work,
          sim = x,
          stop_time = min(which(Nvec == 0))
        ))
      })
    })
  tictoc::toc()
  
  # bind with simulation parameters
  how_long <- cbind(how_long_params, how_long)
  
  # save a copy
  write_rds(sim_results, "1_data/how_long.rds")
} else {
  
  # read in saved results
  how_long <- read_rds(sim_results, "1_data/how_long.rds")
}

how_long_plot <- 
  how_long %>%
  mutate(
    c_work = factor(
      ifelse(is.na(c_work), 0, 1),
      levels = c(0, 1),
      labels = c(
        "Gatherings restricted\nto <= 4",
        "Gatherings restricted +\nWork from home"
      )
    ),
    pi_0 = factor(
      pi_0,
      labels = c(
        bquote(p[i] == 0.001),
        bquote(p[i] == 0.005),
        bquote(p[i] == 0.01),
        bquote(p[i] == 0.02)
      )
    )
  ) %>%
  ggplot(., aes(x = stop_time * 5, color = factor(pr_0))) +
  facet_grid(c_work ~ pi_0) +
  stat_ecdf() +
  scale_y_reverse(labels = rev(c(0, 0.25, 0.5, 0.75, 1))) +
  ggtitle(label = "", subtitle = "Proportion infected") +
  scale_color_brewer(name = bquote(p[r])) +
  labs(
    x = "Time intervention is in place (days)",
    y = "Probability that epidemic survives to time t"
  ) +
  gatherings_theme() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    legend.position = c(0.95, 0.2)
  )


pdf("3_results/how_long_plot.pdf", width = 9.6, height = 5.8)
print(how_long_plot)
dev.off()