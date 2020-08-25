binom_cdf <- function(x, size, p) pbinom(x, size, p)
betabinom_cdf <- function(x, size, p, phi) rmutil::pbetabinom(x, size, p, phi)

binom_pmf <- function(x, size, p) dbinom(x, size, p)
betabinom_pmf <- function(x, size, p, phi) rmutil::dbetabinom(x, size, p, phi)

df <- 
  expand.grid(
  N = 2:30,
  q = seq(0.05, 0.2, 0.05)
)
rmutil::rbetazbinom()
df$R_eff <- df$N * df$q

pdf("plot1.pdf", width = 5, height = 5)
ggplot(df, aes(x = N, y = R_eff, color = factor(q))) +
  geom_line() +
  scale_color_brewer(name = "q", type = "qual", palette = "Set1") +
  coord_cartesian(expand = FALSE) +
  our_theme() +
  ylab(bquote("Expected incident cases ("*R['g']*")")) +
  xlab("Gathering size (N)") +
  theme(legend.position = c(0.1, 0.8))
dev.off()

df <- 
  expand.grid(
    x = 0:30,
    N = 2:30,
    q = 0.2
  )

df <- filter(df, x <= N)

df$px <- mapply(binom_pmf, x = df$x, size = df$N, p = df$q)

pdf("plot2.pdf", width = 5, height = 5)
ggplot(df, aes(
  x = N,
  y = x,
  fill = px,
  color = px
)) +
  geom_tile() +
  #geom_text(aes(label = round(px, 2)), color = "black") +
  scale_fill_viridis_c(
    name = bquote(P(X == x)),
    option = "C",
    # trans = scales::sqrt_trans(),
    # breaks = c(0.05, 0.25, 0.6)
  ) +
  scale_color_viridis_c(
    name = bquote(P(X == x)),
    option = "C",
    # trans = scales::sqrt_trans(),
    # breaks = c(0.05, 0.25, 0.6)
  ) +
  coord_cartesian(expand = FALSE) +
  ylab("Incidence cases (X)") +
  xlab("Gathering size (N)") +
  our_theme() +
  theme(legend.position = c(0.1, 0.8))
dev.off()


df <- expand.grid(
  x = 1,
  N = 2:30,
  q = seq(0.05, 0.20, 0.05),
  phi = c(0.1, 1, 10, 100)
)
df$gt1 <- 1 - mapply(binom_cdf, x = df$x, size = df$N, p = df$q)

df$gt1_bb <- 1 - mapply(betabinom_cdf, x = df$x, size = df$N, p = df$q, phi = df$phi)

df$gt1_bb <- ifelse(df$phi == 100, df$gt1, df$gt1_bb)


ggplot(df, aes(
  x = N,
  y = gt1_bb,
  color = factor(phi, labels = c("0.1", "1", "10", bquote("\U221E")))
)) +
  facet_grid( ~ factor(q, labels = c("q = 0.05", "q = 0.10", "q = 0.15", "q = 0.20"))) +
  geom_line() +
  scale_color_brewer(name = bquote(phi),
                     type = "qual",
                     palette = "Set1") +
  coord_cartesian(expand = FALSE) +
  ylab(bquote(P*"("*X >= 1*")")) +
  our_theme()


df <- expand.grid(
  x = 0:30,
  N = 2:30,
  q = seq(0.05, 0.20, 0.05),
  phi = c(0.1, 1, 10, 100)
)

df <- filter(df, x <= N)

df$px <- mapply(betabinom_pmf, x = df$x, size = df$N, p = df$q, phi = df$phi)
df$px_bb <- mapply(betabinom_pmf, x = df$x, size = df$N, p = df$q, phi = df$phi)

df$px_bb <- ifelse(df$phi == 100, df$px, df$px_bb)


ggplot(df, aes(
  x = N,
  y = x,
  fill = px_bb
)) +
  facet_grid(
    factor(phi, labels = c("0.1", "1", "10", bquote("\U221E"))) ~ 
      factor(q, labels = c("q = 0.05", "q = 0.10", "q = 0.15", "q = 0.20"))) +
  geom_tile() +
  scale_fill_viridis_c(name = bquote(P(X == x)), trans = scales::sqrt_trans(), breaks = c(0.05, 0.25, 0.6)) +
  coord_cartesian(expand = FALSE) +
  ylab("X") +
  our_theme()

pdf("plot3.pdf", width = 8, height = 5)
ggplot(filter(df, q == 0.2), aes(
  x = N,
  y = x,
  fill = px_bb,
  color = px_bb
)) +
  facet_wrap( ~ factor(phi, labels = c(
    bquote(phi == 0.1),
    bquote(phi == 1),
    bquote(phi == 10),
    bquote(phi == infinity)
  )), labeller = label_parsed, nrow = 2, ncol = 2) +
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
  labs(
    y = "Incident cases (X)",
    x = "Gathering size (N)"
  ) +
  our_theme() +
  theme(legend.position = c(0.075, 0.83),
        legend.key.size = unit(0.4, "cm"))
dev.off()


df <- expand.grid(
  q = 0.2,
  phi = c(0.1, 1, 10, 100)
)

plot_df <- pmap_df(
  list(q = df$q,
       phi = df$phi),
  function(q, phi) {
    data.frame(
      q = q,
      phi = phi,
      x = rbeta(100000, q * phi, phi * (1 - q))
    )
  }
)

pdf("plot3b.pdf", width = 8, height = 5)
ggplot(plot_df, aes(
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
dev.off()




