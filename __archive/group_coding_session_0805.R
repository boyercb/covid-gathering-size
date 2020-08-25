source("simulation_functions.R")

# Simulation 1 ------------------------------------------------------------

# let's start really simple and consider the case in which we have a single
# infected attending a gathering of size N with NO heterogeneity in
# infectiousness

SIMS <- 10000
N <- seq(2, 50, 1)  # size of gatherings
pi <- 0.1           # population prevalence of infection
ps <- 1 - pi        # population prevalence of susceptibles
                    # (NOTE: for now assume no recovereds)  

simple_params <- 
  expand.grid(
    N = N,
    pi = pi,
    ps = ps
  )

pb <- txtProgressBar(max = nrow(simple_params), initial = NA, style = 3)

simple_results <-
  pmap_dfr(
    as.list(simple_params),
    function(N, ps, pi) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      
      sim <-
        dgp(
          N,
          ps,
          pi,
          sims = SIMS,
          type = "probability",
          prior = function(x) rbeta(x, 0.8, 9.2)
        )
      
      return(list(
        "N" = N,
        "pi" = pi,
        "ps" = ps,
        "S" = sim$SIR[,1],
        "I" = sim$SIR[,2],
        "R" = sim$SIR[,3],
        "delta" = sim$delta
      ))
    }
  )

close(pb)

# simple_params <- simple_params[rep(seq_len(nrow(simple_params)), times = simple_results$num.gt.1), ]
# 
# simple_results <- cbind(simple_params, simple_results)

ggplot() + 
  stat_summary(
    aes(x = N, y = delta),
    data = filter(simple_results, I > 0),
    geom = "line",
    fun = mean,
    color = "green"
  ) +
  stat_summary(
    aes(x = N, y = delta),
    data = filter(simple_results, I == 1),
    geom = "line",
    fun = mean,
    color = "blue"
  ) + 
  stat_summary(
    aes(x = N, y = delta),
    data = filter(simple_results, I == 2),
    geom = "line",
    fun = mean,
    color = "red"
  ) 

ggplot(simple_results, aes(x = N, y = factor(delta))) + 
  geom_bin2d(binwidth = 1) + 
  theme_bw() +
  scale_fill_viridis_c()

ggplot(simple_results, aes(x =, y = ))


# plot 


ggplot(simple_results, aes(x = N, y = r_eff)) + geom_smooth()
