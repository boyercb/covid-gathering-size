p5_df <- expand_grid(
  N = 0:100,
  h = c(2, 2.5, 5, 10, Inf)
) 

p5_df$k <- (100 - (p5_df$N / p5_df$h)) / 100 * p5_df$N

p5 <- 
  ggplot(p5_df, aes(x = N, y = k, color = factor(h))) +
  geom_line() +
  coord_cartesian(expand = FALSE) +
  labs(y = "Contacts (k)",
       x = "Gathering size (N)") +
  scale_color_brewer(
    name = "h",
    type = "qual",
    palette = "Set1",
    labels = c(bquote(2), bquote(2.5), bquote(5), bquote(10), bquote(infinity))
  ) +
  gatherings_theme() 

pdf("3_results/plot5.pdf", width = 6, height = 6)
print(p5)
dev.off()