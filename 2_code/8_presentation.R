# load the BBC pandemic data ----------------------------------------------

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18_v1.csv")

k_home <- k_bbc %>%
  rename(M = e_home) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))

k_work <- k_bbc %>%
  rename(M = e_work) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))

k_gather <- k_bbc %>%
  rename(M = e_other) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))


# exploring heterogeneity via Lorenz curve --------------------------------

sim1_params <- 
  expand_grid(
    phi = c(0.1, 1, 10, 100)
  )

sim1 <- 
  map_dfr(sim1_params$phi, function(phi) {
    X <- offspring_model(
      N = 100000,
      k_work = k_work,
      k_gather = k_gather,
      k_home = k_home,
      phi_work = phi,
      phi_gather = phi,
      phi_home = phi
    )
    return(tibble(phi = phi, as_tibble(X)))
  }) 

plot_sim1 <- sim1 %>% 
  pivot_longer(cols = starts_with("X_")) %>%
  mutate(
    name = factor(
      x = name,
      levels = c("X_home", "X_gather", "X_work"),
      labels = c("Home", "Gatherings", "Work")
    ),
    selected = case_when(
      phi == 1 & name == "Gatherings" ~ 1,
      phi == 10 & name %in% c("Work", "Home") ~ 1,
      TRUE ~ 5
    )
  )  

ggplot(plot_sim1, aes(x = value, color = factor(phi))) +
  facet_grid( ~ name) +
  gglorenz::stat_lorenz(desc = TRUE, geom = "line") +
  scale_color_brewer(name = bquote("Dispersion\nparameter"*phi), palette = "Reds") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  xlab("\nProportion of infectious cases") +
  ylab("Proportion of secondary cases\n") +
  gatherings_theme() +
  theme(
    legend.position = c(0.92, 0.15)
  )

ggplot(plot_sim1, aes(x = value, color = factor(selected), group = factor(phi))) +
  facet_grid( ~ name) +
  gglorenz::stat_lorenz(desc = TRUE) +
  geom_text(
    aes(x = x, y = y, label = label),
    data = tibble(
      name = factor(
        c("X_home", "X_gather", "X_work"),
        levels = c("X_home", "X_gather", "X_work"),
        labels = c("Home", "Gatherings", "Work")
      ),
      phi = c(10, 1, 10),
      selected = c(1, 1, 1),
      label = c(bquote(phi == 10), bquote(phi == 1), bquote(phi == 10)),
      x = c(0.4, 0.22, 0.25),
      y = c(0.7, 0.9, 0.85)
    ),
    family = "Palatino",
    parse = TRUE
  ) + 
  scale_color_manual(values = c("red", alpha("black", 0.4))) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  xlab("\nProportion of infectious cases") +
  ylab("Proportion of secondary cases\n") +
  gatherings_theme() +
  theme(
    legend.position = "none"
  )


# values of R_g, R_h, R_w -------------------------------------------------

plot_sim1 %>%
  filter(selected == 1) %>%
  group_by(name) %>%
  summarise(
    value = mean(value)
  ) %>%
  mutate(
    letter = c("h", "g", "w")
  ) %>%
  ggplot(., aes(x = name, y = value, fill = name)) +
  geom_col() +
  geom_text(
    aes(label = paste0("R[", letter, "] == ", round(value, 2))),
    nudge_y = 0.05,
    family = "Palatino",
    parse = T
  ) +
  scale_fill_manual(values = c("#a6cee3", "#fdbf6f", "#fb9a99")) +
  gatherings_theme() +
  coord_cartesian(expand = F) +
  xlab("") +
  ylab("R value") +
  ylim(c(0, 1.5)) +
  theme(legend.position = "none")


# restriction types  ------------------------------------------------------

# REDO THIS:
# Add :
# At the top : Overdispersion
# "Phi ="
# Mention which is most overdispersed and least overdispersed 
# Axis on the right that says gathering size limitation
# Restriction type 0 = no restriction, 1 = blabla, 2 =, 3 = blablabla

# sim_results %>%
#   filter(pi == 0.01 & pr == 0) %>%
#   ggplot(., aes(x = factor(c), y = R_g, fill = factor(phi))) +
#   geom_violin(draw_quantiles = c(0.5), trim = FALSE) +
#   facet_grid(option~phi) + 
#   gatherings_theme() +
#   scale_fill_brewer() +
#   theme(legend.position = "none") +
#   labs(
#     x = "Restriction type",
#     y = bquote(R[g])
#   )

# how low do we have to go? -----------------------------------------------

sim2_params <- 
  expand_grid(
    c_gather = seq(2, 50),
    freq_gather = seq(1, 5),
    option = c("1", "2", "3") 
  )

sim2 <- 
  pmap_dfr(as.list(sim2_params), function(c_gather, freq_gather, option) {
    X <- offspring_model(
      N = 500000,
      k_work = k_work,
      k_gather = k_gather,
      k_home = k_home,
      mu_home = 0.17,
      c_gather = c_gather,
      freq_gather = freq_gather,
      option = option
    )
    return(tibble(
      c_gather = c_gather,
      freq_gather = freq_gather,
      option = option,
      name = c("R_gather", "R_t"),
      value = c(colMeans(X)[2], sum(colMeans(X)))
    ))
  }) 

# with frequency
ggplot(sim2, aes(x = c_gather, y = value, color = factor(freq_gather))) +
  facet_grid(factor(
    option,
    levels = c("1", "3", "2"),
    labels = c(
      "(1)\nSet gathering size to limit",
      "(2)\nRedraw",
      "(3)\nSet gathering size to 1"
    )
  ) ~ factor(name, labels = c(bquote(R[g]), bquote(R[t])))) +
  geom_hline(
    yintercept = 1,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  geom_step(size = 1.1) +
  scale_color_brewer(name = "frequency per\ninfectious period", palette = "Purples") +
  ylab("") +
  xlab("Gathering size limit (c)") +
  gatherings_theme() +
  theme(
    legend.position = 'right'
  )

# frequency set to 5
ggplot(filter(sim2, freq_gather == 5), aes(x = c_gather, y = value, color = factor(
  option,
  levels = c("1", "3", "2"),
  labels = c(
    "(1)\nSet gathering size to limit",
    "(2)\nRedraw",
    "(3)\nSet gathering size to 1"
  )))) +
  facet_grid(~ factor(name, labels = c(bquote(R[g]), bquote(R[t])))) +
  geom_hline(
    yintercept = 1,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  geom_step(size = 1.1) +
  scale_color_brewer(name = "Restriction type", palette = "Greens") +
  ylab("") +
  xlab("Gathering size limit (c)") +
  gatherings_theme() +
  theme(
    legend.position = 'right'
  )


# how long do we have to hold for? ----------------------------------------

sim3_params <- 
  expand_grid(
    pi_0 = c(0.001, 0.005, 0.01, 0.02),
    pr_0 = c(0, 0.10, 0.20),
    c_work = c(NA, 1)
  )

sims3 <- 
  pmap_dfr(as.list(sim3_params), function(pi_0, pr_0, c_work) {
    map_dfr(1:10000, function(x) {
      
      pop <- 100000L
      N <- pi_0 * pop
      Nvec <- vector()
      
      pi <- pi_0
      pr <- pr_0
      i <- 1
      
      while(i <= 50) {
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
            option = "2"
          ) %>% colSums() %>% sum()
        Nvec[i] <- N
        pi <- N / pop
        if (i > 1) {
          pr <- sum(Nvec[1:(i-1)]) / pop
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

sims3 %>%
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
  ggtitle(subtitle = "Proportion infected") +
  scale_color_brewer(name = bquote(p[r])) +
  labs(
    x = "Days intervention is in place",
    y = "Probability that epidemic continues"
  ) +
  gatherings_theme() +
  theme(
    legend.position = c(0.95, 0.2)
  )


# Eva's crazy new idea ----------------------------------------------------

# compare two scenarios
# 1. implement restriction at much lower threshold, lift them at low level
# 2. implement restriction at higher pi threshold, lift them at same low level

# can show that earlier is better
# is total time different between scenarion 1 and 2?

N_sims_per_timestep <- 1000

simulate_scenarios <- function(x,              # blank argument for map function
                               N = 20,         # number infected at start 
                               N_r = 0,        # number immune at start
                               pop = 1000000L, # population size
                               timesteps = 50, # number of time steps 
                               max_pi,         # when to start restriction
                               min_pi          # when to lift restriction
                               ) {
  
  # starting values
  pi <- N / pop     # initial prevalence of infection
  pr <- N_r / pop   # initial prevalence of immunity
  
  restrict <- FALSE   # flag to start or stop restriction
  
  # vectors for collecting number infected and Rt value at each time step
  Nvec <- vector()
  Rtvec <- vector()
  
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
          pi = pi,
          pr = pr,
          c_gather = 5,
          option = "3"
        )
    } else { # if restriction not in place 
      X <-
        offspring_model(
          N = N,
          k_work = k_work,
          k_gather = k_gather,
          k_home = k_home,
          pi = pi,
          pr = pr
        ) 
    } 
    
    N <- X %>% colSums() %>% sum()
    Rt <- X %>% colMeans() %>% sum()
    
    Nvec[i] <- N
    Rtvec[i] <- Rt
    pi <- N / pop
    if (i > 1) {
      pr <- sum(Nvec[1:(i-1)]) / pop
    }
    i <- i + 1
  }
  return(data.frame(
    sim = rep(x, timesteps),
    period = 1:timesteps,
    infected = Nvec, 
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
    max_pi = 0.01,
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

# run scenario 2
scenario2 <-
  furrr::future_map_dfr(
    1:N_sims_per_timestep,
    simulate_scenarios,
    min_pi = 0.001,
    max_pi = 0.10,
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

