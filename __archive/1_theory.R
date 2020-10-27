# plot 1a: R versus gathering size ----------------------------------------
p1a_df <-
  expand.grid(N = 2:30,
              q = seq(0.05, 0.2, 0.05))

p1a_df$R_eff <- p1a_df$N * p1a_df$q

p1a <- ggplot(p1a_df, aes(x = N, y = R_eff, color = factor(q))) +
  geom_line() +
  scale_color_brewer(name = "q",
                     type = "qual",
                     palette = "Set1") +
  coord_cartesian(expand = FALSE) +
  gatherings_theme() +
  ylab(bquote("Expected incident cases (" * R['g'] * ")")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))

pdf("3_results/plot1a.pdf", width = 5, height = 5)
print(p1a)
dev.off()


# plot 1b: probability of X incidence cases by gathering size -------------

p1b_df <-
  expand.grid(x = 0:30,
              N = 2:30,
              q = 0.2)

p1b_df <- filter(p1b_df, x <= N)

p1b_df$px <- mapply(
  FUN = binom_pmf,
  x = p1b_df$x,
  size = p1b_df$N,
  p = p1b_df$q
)

p1b <- ggplot(p1b_df, aes(
  x = N,
  y = x,
  fill = px,
  color = px
)) +
  geom_tile() +
  scale_fill_viridis_c(name = bquote(P(X == x)),
                       option = "C") +
  scale_color_viridis_c(name = bquote(P(X == x)),
                        option = "C") +
  coord_cartesian(expand = FALSE) +
  ylab("Incidence cases (X)") +
  xlab("Gathering size (N)") +
  gatherings_theme() +
  theme(legend.position = c(0.1, 0.8))

pdf("3_results/plot1b.pdf", width = 5, height = 5)
print(p1b)
dev.off()