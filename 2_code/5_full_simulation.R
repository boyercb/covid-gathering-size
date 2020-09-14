N_SEQ <- seq(2, 30)                    # size of gatherings
PI_SEQ <- seq(0.005, 0.02, 0.005)      # population prevalence of infection
PR_SEQ <- seq(0, 0.20, 0.01)           # population prevalence of immunity
PHI_SEQ <- c(0.1, 1, 10, 100)          # range of dispersion parameter phi
L_SEQ <- seq(10, 100, 10)              # range of limitations in gathering size 


sim_params <-
  expand.grid(
    N = N_SEQ,
    pi = PI_SEQ,
    pr = PR_SEQ,
    phi = PHI_SEQ
  )

sim_params$ps <- 1 - sim_params$pi - sim_params$pr

pb <- txtProgressBar(max = nrow(sim_params), initial = NA, style = 3)

sim_results <-
  pmap(
    as.list(sim_params[names(sim_params) != "pr"]),
    function(N, ps, pi, phi) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      sim <- dgp(N, ps, pi, sims = SIMS, type = "probability", prior = function(x) rbeta(x, 0.086 * phi, phi * 0.914))
      list(
        "X_eff_table" = table(sim$X_eff),
        "X_eff_mean" = mean(sim$X_eff),
        "delta_table" = table(sim$delta),
        "delta_mean" = mean(sim$delta)
      )
    }
  )

close(pb)


# plot: expected secondary cases per index case (Rg) ----------------------

sim_X_eff_mean <- 
  cbind(sim_params, "mean" = sapply(sim_results, get, x = "X_eff_mean"))

# Eva, I think what you'd want to do here is:
#   1. load empirical distribution data of gathering sizes from BBC Pandemic  
#   2. left join sim_X_eff_mean with empirical distribution 
#   3. calculate weighted mean by multiplying probability of gathering size by X_eff at that size

# I think this will give us an estimate of R_g or gathering's contribution to R_t
# then we can just plot before and after intervention and use simulation to give us 
# uncertainty range in R_g


# load and modify the BBC pandemic data as we need to for the draw_gatherings() function below
bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")
bbc <- select(bbc, e_other)
# gathering sizes are contacts + 1 (index person)
bbc$M <- bbc$e_other + 1
# plotting it
bbc_data <- ggplot(data = bbc, mapping = aes(x = M)) + 
  geom_point(stat = "count") + 
  scale_y_log10() + scale_x_log10()
pdf("3_results/bbc_data.pdf", width = 5, height = 6)
print(bbc_data)
dev.off()  
## On GitHub there was this _v1 version. It seems that the e_other column is the same in both files:
# o18_v1 <- read.csv("/Users/carolinemerdinger/Desktop/Other_dist/contact_distributions_o18_v1.csv", header = TRUE)
# #head(o18_v1) #count(o18_v1)
# o18_v1$e_other2 <- o18$e_other
# o18_v1$test <- o18_v1$e_other - o18_v1$e_other2
# o18_v1$test; sum(o18_v1$test) # 0

#
sim_X_eff_mean <-
  sim_X_eff_mean %>%
  filter(pr %in% c(0, 0.05, 0.10, 0.15, 0.20)) %>%
  filter(phi == 1) %>%
  mutate(
    pi = factor(pi, labels = c(
        bquote(p[i] == 0.005),
        bquote(p[i] == 0.01),
        bquote(p[i] == 0.015),
        bquote(p[i] == 0.02)
      )),
    pr = factor(pr, labels = c(
      bquote(p[r] == 0),
      bquote(p[r] == 0.05),
      bquote(p[r] == 0.10),
      bquote(p[r] == 0.15),
      bquote(p[r] == 0.20)
    )),
  )

p_sim_X_eff_mean <- 
  ggplot(sim_X_eff_mean, aes(
  x = N,
  y = mean,
  fill = pr,
  color = pr
)) +
  facet_grid(
    ~ pi,
    labeller = label_parsed
  ) +
  geom_smooth(se = F, method = "lm") +
  scale_fill_viridis_d(
    name = "Immune (%)",
    option = "C",
    labels = label_parse()
  ) +
  scale_color_viridis_d(
    name = "Immune (%)",
    option = "C",
    labels = label_parse()
  ) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  coord_cartesian(expand = FALSE) +
  labs(y = bquote("Expected secondary cases per index case, "*E(X[i])),
       x = "Gathering size (N)") +
  gatherings_theme() 

pdf("3_results/sim_X_eff_mean.pdf", width = 10, height = 6)
print(p_sim_X_eff_mean)
dev.off()  


# plot: probability of Rg >= 1 --------------------------------------------

sim_X_eff_table <- 
  cbind(sim_params, lapply(sim_results, get, x = "X_eff_table") %>% bind_rows())

sim_X_eff_table <- 
  sim_X_eff_table %>%
  mutate(
    across(`0`:`29`, function (x) x / rowSums(select(sim_X_eff_table, `0`:`29`), na.rm = T))
  ) %>%
  pivot_longer(
    `0`:`29`,
    names_to = "x",
    values_drop_na = TRUE
  ) %>%
  mutate(x = as.integer(x)) %>%
  filter(x == 0) %>%
  mutate(
    phi = factor(phi, labels = c(
      bquote(phi == 0.1),
      bquote(phi == 1),
      bquote(phi == 10),
      bquote(phi == 100)
    )),
    pi = factor(pi, labels = c(
      bquote(p[i] == 0.005),
      bquote(p[i] == 0.01),
      bquote(p[i] == 0.015),
      bquote(p[i] == 0.02)
    ))
  )


p_sim_X_eff_table <- 
  ggplot(sim_X_eff_table, aes(
  x = N,
  y = pr,
  fill = 1 - value,
  color = 1 - value
)) +
  facet_grid(
    phi ~ pi,
    labeller = label_parsed
  ) +
  geom_tile() +
  scale_fill_viridis_c(
    name = bquote(P(X[i] > 0)),
    option = "C",
  ) +
  scale_color_viridis_c(
    name = bquote(P(X[i] > 0)),
    option = "C",
  ) +
  coord_cartesian(expand = FALSE) +
  labs(y = bquote("Prevalence of immunity ("*p[r]*")"),
       x = "Gathering size (N)") +
  gatherings_theme() + 
  theme(legend.key.width = unit(1, "cm"))

pdf("3_results/sim_X_eff_table.pdf", width = 10, height = 6)
print(p_sim_X_eff_table)
dev.off()  


# plot: expected secondary cases per gathering ----------------------------

sim_delta_mean <- 
  cbind(sim_params, "mean" = sapply(sim_results, get, x = "delta_mean"))

sim_delta_mean <-
  sim_delta_mean %>%
  filter(pr %in% c(0, 0.05, 0.10, 0.15, 0.20)) %>%
  filter(phi == 1) %>%
  mutate(
    pi = factor(pi, labels = c(
      bquote(p[i] == 0.005),
      bquote(p[i] == 0.01),
      bquote(p[i] == 0.015),
      bquote(p[i] == 0.02)
    )),
    pr = factor(pr, labels = c(
      bquote(p[r] == 0),
      bquote(p[r] == 0.05),
      bquote(p[r] == 0.10),
      bquote(p[r] == 0.15),
      bquote(p[r] == 0.20)
    )),
  )

p_sim_delta_mean <- 
  ggplot(sim_delta_mean, aes(
    x = N,
    y = mean,
    fill = pr,
    color = pr
  )) +
  facet_grid(
    ~ pi,
    labeller = label_parsed
  ) +
  geom_smooth(se = F) +
  scale_fill_viridis_d(
    name = "Immune (%)",
    option = "C",
    labels = label_parse()
  ) +
  scale_color_viridis_d(
    name = "Immune (%)",
    option = "C",
    labels = label_parse()
  ) +
  coord_cartesian(expand = FALSE) +
  labs(y = bquote("Expected secondary cases per gathering"),
       x = "Gathering size (N)") +
  gatherings_theme() 

pdf("3_results/sim_delta_mean.pdf", width = 10, height = 6)
print(p_sim_delta_mean)
dev.off()  
