# Expected number of cases using exact solution ---------------------------

N <- seq(2, 20, 1)           # size of gatherings
p <- seq(0.001, 0.1, 0.001)  # prevalence of infection
q <- seq(0.2, 0.8, 0.2)      # probability of infection given contact

df <- expand.grid(N = N, p = p, q = q)

df$dI <- with(df, N * (1 - p) * (1 - (1 - q)^(N * p)))

pdf("exact.pdf", height = 7.5, width = 13.33)
ggplot(df, aes(x = N, y = p, z = dI)) +
  facet_grid( ~ q, labeller = labeller(q = q_labs)) +
  geom_contour_filled(binwidth = 1) +
  xlab("size of gathering (N)") + 
  scale_fill_viridis_d(name = "expected\nnew cases", option = "plasma") +
  coord_cartesian(expand = F)
dev.off()


# Number needed to keep expected new cases below 1 ------------------------

p <- seq(0.0001, 0.1, 0.0001)  # prevalence of infection
q <- seq(0.2, 0.8, 0.2)        # probability of infection given contact

f <- function(x, p, q) x * (1 - p) * (1 - (1 - q)^(x * p)) - 1

df <- expand.grid(p = p, q = q)
df$N <- mapply(function(p, q) uniroot(f, interval = c(0, 1000), p = p, q = q)$root, df$p, df$q)

pdf("size_limit.pdf", height = 7.5, width = 13.33)
ggplot(df, aes(x = p, y = N, group = factor(q), color = factor(q))) +
  geom_line() + 
  coord_cartesian(expand = F) +
  scale_color_brewer(name = "q") +
  scale_x_log10(labels = function(n) format(n, scientific = FALSE)) +
  scale_y_log10() +
  labs(
    x = "p",
    y = "size of gathering (N)",
    title = "Gathering size limit required to keep expected new cases below 1 (log-log scale)"
  ) +
  theme_classic()
dev.off()

