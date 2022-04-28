# plot: distribution of heterogenous q with dispersion ---------------------

hetq_grid <- expand.grid(
  q = 0.2,
  phi = c(0.1, 1, 10, 100)
)

hetq_df <- pmap_df(
  as.list(hetq_grid),
  function(q, phi) {
    data.frame(
      q = q,
      phi = phi,
      x = rbeta(100000, q * phi, phi * (1 - q))
    )
  }
)

p2 <- 
  ggplot(hetq_df, aes(
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

pdf("3_results/hetq.pdf", width = 8, height = 5)
print(p2)
dev.off()


