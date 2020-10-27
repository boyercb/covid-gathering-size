# plot 4: probability of I infecteds attending by prevalence --------------

p4_df <- 
  expand.grid(
    x = seq(1, 5),
    N = 2:30,
    pi = c(0.001, 0.005, 0.01, 0.05)
  )

p4_df$px <- mapply(binom_pmf, x = p4_df$x, size = p4_df$N, p = p4_df$pi)

p4 <-
  ggplot(p4_df, aes(
    x = N,
    y = px,
    group = factor(x),
    color = factor(x)
  )) +
  geom_line() +
  facet_grid( ~ factor(pi, labels = paste0("p[i] == ", c(
    0.001, 0.005, 0.01, 0.05
  ))), labeller = label_parsed) +
  scale_color_brewer(
    name = "Number of infecteds\nin attendance",
    type = "qual",
    palette = "Set1",
    labels = paste0("I = ", 1:5)
  ) +
  coord_cartesian(expand = FALSE) +
  gatherings_theme() +
  ylab(bquote("Probability")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))

pdf("3_results/plot4.pdf", width = 10, height = 6)
print(p4)
dev.off()


# simulation 1 - use HK parameters, vary N and pi -------------------------

N_SEQ <- seq(2, 30)                    # size of gatherings
PI_SEQ <- c(0.001, 0.005, 0.01, 0.05)  # population prevalence of infection
PS_SEQ <- 1 - PI_SEQ                   # population prevalence of susceptibles

p5_params <-
  expand.grid(
    N = N_SEQ,
    pi = PI_SEQ
  )

p5_params$ps <- 1 - p5_params$pi  

pb <- txtProgressBar(max = nrow(p5_params), initial = NA, style = 3)

p5_results <-
  pmap(
    as.list(p5_params),
    function(N, ps, pi) {
      i <- getTxtProgressBar(pb)
      setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
      sim <- dgp(N, ps, pi, sims = SIMS, type = "probability", prior =function(x) rbeta(x, 0.20, 0.80))
      list(
        "r_eff" = table(sim$r_eff),
        "delta" = table(sim$delta)
      )
    }
  )

close(pb)

p5_df <- 
  cbind(p5_params, lapply(p5_results, get, x = "r_eff") %>% bind_rows())

p5_df <- 
  p5_df %>%
  mutate(
    across(`0`:`29`, function (x) x / rowSums(select(p5_df, `0`:`29`), na.rm = T))
  ) %>%
  pivot_longer(
    `0`:`29`,
    names_to = "x",
    values_drop_na = TRUE
  ) %>%
  mutate(x = as.integer(x))

p5 <- ggplot(p5_df, aes(
  x = N,
  y = x,
  fill = value,
  color = value
)) +
  facet_wrap(
    ~ factor(pi, labels = c(
      bquote(p[i] == 0.001),
      bquote(p[i] == 0.005),
      bquote(p[i] == 0.01),
      bquote(p[i] == 0.05)
    )),
    labeller = label_parsed,
    nrow = 2,
    ncol = 2
  ) +
  geom_tile() +
  scale_fill_viridis_c(
    name = bquote(P(X == x)),
    trans = scales::sqrt_trans(),
    breaks = c(0.05, 0.25, 0.6),
    option = "C",
  ) +
  scale_color_viridis_c(
    name = bquote(P(X == x)),
    trans = scales::sqrt_trans(),
    breaks = c(0.05, 0.25, 0.6),
    option = "C",
  ) +
  coord_cartesian(expand = FALSE) +
  labs(y = "Incident cases (X)",
       x = "Gathering size (N)") +
  gatherings_theme() +
  theme(legend.position = c(0.075, 0.83),
        legend.key.size = unit(0.4, "cm"))



pdf("3_results/plot5.pdf", width = 10, height = 6)
print(p5)
dev.off()

p6_df <- 
  cbind(p5_params, lapply(p5_results, get, x = "delta") %>% bind_rows())

p6_df <- 
  p6_df %>%
  mutate(
    across(`0`:`29`, function (x) x / rowSums(select(p6_df, `0`:`29`), na.rm = T))
  ) %>%
  pivot_longer(
    `0`:`29`,
    names_to = "x",
    values_drop_na = TRUE
  ) %>%
  mutate(x = as.integer(x))

p6 <- ggplot(p6_df, aes(
  x = N,
  y = x,
  fill = value,
  color = value
)) +
  facet_wrap(
    ~ factor(pi, labels = c(
      bquote(p[i] == 0.001),
      bquote(p[i] == 0.005),
      bquote(p[i] == 0.01),
      bquote(p[i] == 0.05)
    )),
    labeller = label_parsed,
    nrow = 2,
    ncol = 2
  ) +
  geom_tile() +
  scale_fill_viridis_c(
    name = bquote(P(X == x)),
    trans = scales::sqrt_trans(),
    breaks = c(0.05, 0.25, 0.6),
    option = "C",
  ) +
  scale_color_viridis_c(
    name = bquote(P(X == x)),
    trans = scales::sqrt_trans(),
    breaks = c(0.05, 0.25, 0.6),
    option = "C",
  ) +
  coord_cartesian(expand = FALSE) +
  labs(y = "Incident cases (X)",
       x = "Gathering size (N)") +
  gatherings_theme() +
  theme(legend.position = c(0.075, 0.83),
        legend.key.size = unit(0.4, "cm"))


pdf("3_results/plot6.pdf", width = 10, height = 6)
print(p6)
dev.off()  
