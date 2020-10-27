# Eva's crazy new idea ----------------------------------------------------

# compare two scenarios
# 1. implement restriction at much lower threshold, lift them at low level
# 2. implement restriction at higher pi threshold, lift them at same low level

# can show that earlier is better
# is total time different between scenarion 1 and 2?

N_sims_per_timestep <- 1000

simulate_scenarios <- function(x,               # blank argument for map function
                               N = 20,          # number infected at start 
                               N_r = 0,         # number immune at start
                               pop = 100000L,   # population size
                               timesteps = 100, # number of time steps 
                               max_pi,          # when to start restriction
                               min_pi           # when to lift restriction
) {
  
  # starting values
  pi <- N / pop     # initial prevalence of infection
  pr <- N_r / pop   # initial prevalence of immunity
  
  restrict <- FALSE   # flag to start or stop restriction
  
  # vectors for collecting number infected and Rt value at each time step
  Nvec <- vector()
  Rtvec <- vector()
  pivec <- vector()
  restrictvec <- vector()
  i <- 1
  
  while(i <= timesteps) {
    
    # if above the limit and no restriction implemented yet
    # implement restriction
    if (pi >= max_pi & !restrict) {
      restrict <- TRUE
    }
    
    # if below the limit and restriction is still in place
    # lift it
    if (pi <= min_pi & restrict) {
      restrict <- FALSE
    }
    
    
    if (restrict) { # if restriction in place 
      X <-
        offspring_model(
          N = N,
          k_work = k_work,
          k_gather = k_gather,
          k_home = k_home,
          mu_home = 0.17,
          pi = pi,
          pr = pr,
          c_gather = 4,
          option = "3"
        )
    } else { # if restriction not in place 
      X <-
        offspring_model(
          N = N,
          k_work = k_work,
          k_gather = k_gather,
          k_home = k_home,
          mu_home = 0.17,
          pi = pi,
          pr = pr
        ) 
    } 
    
    N <- X %>% colSums() %>% sum()
    Rt <- X %>% colMeans() %>% sum()
    pi <- N / pop
    
    Nvec[i] <- N
    Rtvec[i] <- Rt
    pivec[i] <- pi
    restrictvec[i] <- as.numeric(restrict)
    
    if (i > 1) {
      pr <- sum(Nvec[1:(i-1)]) / pop
    }
    i <- i + 1
  }
  return(data.frame(
    sim = rep(x, timesteps),
    period = 1:timesteps,
    infected = Nvec, 
    cumulative = cumsum(Nvec),
    pi = pivec,
    restrict = restrictvec,
    Rt = Rtvec
  ))
}

# plan for parallel multiprocess
future::plan(future::multiprocess, workers = 4)

# run scenario 1
scenario1 <-
  furrr::future_map_dfr(
    1:N_sims_per_timestep,
    simulate_scenarios,
    min_pi = 0.001,
    max_pi = 0.005,
    .progress = TRUE
  )

# Rt over time
ggplot(scenario1, aes(x = period, y = Rt)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 

# Incident cases over time
ggplot(scenario1, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")

# Possible: cumulative cases over time
ggplot(scenario1, aes(x = period, y = cumulative)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")


# plan for parallel multiprocess
future::plan(future::multiprocess, workers = 4)

# run scenario 2
scenario2 <-
  furrr::future_map_dfr(
    1:N_sims_per_timestep,
    simulate_scenarios,
    min_pi = 0.001,
    max_pi = 0.02,
    .progress = TRUE
  )


ggplot(scenario2, aes(x = period, y = Rt)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


ggplot(scenario2, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")

# Possible: cumulative cases over time
ggplot(scenario2, aes(x = period, y = cumulative)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")


# alternative plots
ggplot(filter(scenario2, sim != 1), aes(x = period, y = infected)) +
  geom_step(aes(group = sim), alpha = 0.05, color = "#3182BD") +
  geom_step(aes(group = sim), data = filter(scenario2, sim == 1), size = 1.2) +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")

ggplot(filter(scenario2, sim != 1), aes(x = period, y = Rt)) +
  geom_step(aes(group = sim), alpha = 0.05, color = "#FC9272") +
  geom_step(aes(group = sim), data = filter(scenario2, sim == 1), size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


ggplot(filter(scenario1, sim != 1), aes(x = period, y = infected)) +
  geom_step(aes(group = sim), alpha = 0.05, color = "#3182BD") +
  geom_step(aes(group = sim), data = filter(scenario1, sim == 1), size = 1.2) +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")

ggplot(filter(scenario1, sim != 1), aes(x = period, y = Rt)) +
  geom_step(aes(group = sim), alpha = 0.05, color = "#FC9272") +
  geom_step(aes(group = sim), data = filter(scenario1, sim == 1), size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 

ggplot(filter(scenario1, sim != 1), aes(x = period, y = cumulative)) +
  geom_step(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  geom_step(aes(group = sim), data = filter(scenario1, sim == 1), size = 1.2) +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")


df <- 
  bind_rows(scenario1, scenario2, .id = "scenario") %>%
  mutate(Rt = ifelse(Rt > 5, NA, Rt)) %>%
  pivot_longer(c("Rt", "infected", "cumulative"))

ggplot(filter(df, sim != 2), aes(x = period, y = value)) +
  facet_grid(name ~ factor(scenario, labels = c("scenario 1", "scenario 2")), scales = "free_y") +
  geom_step(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  geom_step(aes(group = sim), data = filter(df, sim == 2), size = 1.1) +
  coord_cartesian(expand = F) +
  gatherings_theme() 
