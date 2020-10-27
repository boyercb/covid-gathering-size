# plot 2: distribution of heterogenous q with dispersion ------------------

p2_grid <- expand.grid(
  q = 0.2,
  phi = c(0.1, 1, 10, 100)
)

p2_df <- pmap_df(
  list(q = p2_grid$q,
       phi = p2_grid$phi),
  function(q, phi) {
    data.frame(
      q = q,
      phi = phi,
      x = rbeta(100000, q * phi, phi * (1 - q))
    )
  }
)
p2 <- 
  ggplot(p2_df, aes(
    x = x,
  )) +
  facet_wrap( ~ factor(phi, labels = c(
    bquote(phi == 0.1),
    bquote(phi == 1),
    bquote(phi == 10),
    bquote(phi == 100)
  )), labeller = label_parsed, nrow = 2, ncol = 2, scales = "free_y") +
  geom_histogram(aes(y = ..density../100), bins = 100, color = "#004c99", fill = "#004c99") +
  coord_cartesian(expand = FALSE) +
  labs(
    y = "Density",
    x = "Transmission probability (q)"
  ) +
  gatherings_theme()

pdf("3_results/plot2.pdf", width = 8, height = 5)
print(p2)
dev.off()


# plot 3: probability of x incidenct cases by values of dispersion --------

p3_df <- expand.grid(
  x = 0:30,
  N = 2:30,
  q = 0.2,
  phi = c(0.1, 1, 10, 100)
)

p3_df <- filter(p3_df, x <= N)

p3_df$px_bb <-
  mapply(
    binom_pmf,
    x = p3_df$x,
    size = p3_df$N,
    p = p3_df$q
  )

p3_df$px_bb <-
  mapply(
    betabinom_pmf,
    x = p3_df$x,
    size = p3_df$N,
    p = p3_df$q,
    phi = p3_df$phi
  )

p3_df$px_bb <- ifelse(p3_df$phi == 100, p3_df$px, p3_df$px_bb)

p3 <- ggplot(p3_df, aes(
  x = N,
  y = x,
  fill = px_bb,
  color = px_bb
)) +
  facet_wrap(
    ~ factor(phi, labels = c(
      bquote(phi == 0.1),
      bquote(phi == 1),
      bquote(phi == 10),
      bquote(phi == infinity)
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

pdf("3_results/plot3.pdf", width = 8, height = 5)
print(p3)
dev.off()


