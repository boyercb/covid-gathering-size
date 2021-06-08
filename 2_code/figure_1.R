library(tidyverse)

tau <- 0.08
pi <- 0.05
ps <- 0.80

df <- tibble(
  k = seq(1, 100),
  x = ps * (k) * (1 - (1 - tau) ^ (pi * (k)))
)
pdf("3_results/expected_infections_example.pdf", width = 4.5, height = 3.5)
ggplot(df, aes(x = k, y = x)) + 
  geom_line() +
  annotate(
    "text",
    x = 45,
    y = 5,
    label = "k * p[s] * (1 - (1 - tau)^{k*p[i]})",
    family = "Palatino",
    parse = TRUE,
    hjust = "left"
  ) +
  # annotate(
  #   "text",
  #   x = 12.5,
  #   y = 16.5,
  #   label = "p[s] == 0.80",
  #   family = "Palatino",
  #   parse = TRUE,
  #   hjust = "left"
  # ) +
  # annotate(
  #   "text",
  #   x = 12.5,
  #   y = 15.5,
  #   label = "tau == 0.08",
  #   family = "Palatino",
  #   parse = TRUE,
  #   hjust = "left"
  # ) +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  #theme_minimal(base_size = 14, base_family = "Palatino") +
  #gatherings_theme() +
  labs(
    x = "Gathering size (k)",
    y = bquote(E(X[t] * "|" * K == k))
  )
dev.off()


pdf("3_results/powerlaw_example.pdf", width = 4.5, height = 3.5)
df <- tibble(
  X = rpldis(10000, 1, 2.5)
) 
df <- df %>% 
  count(X) %>%
  mutate(
    n_sum = sum(n) - lag(cumsum(n), default = 0),
    p = n_sum / sum(n)
    )
  
#%>% count(X)

ggplot(df, aes(x = X, y = p)) + 
  geom_point() +
  scale_y_continuous(trans = "log10", labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_continuous(trans = "log10", labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks() +
  coord_cartesian() +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  #gatherings_theme() +
  labs(
    x = "Gathering size (k)",
    y = bquote("Pr(K > k)")
  )
dev.off()

tau <- 0.10
pi <- 0.05

df <- tibble(
  k = seq(1, 100),
  x = 0.5 * (k + 1) * (1 - (1 - tau) ^ (pi * (k + 1)))
)

df$R_g_1 <- df$k * tau * df$k^{-2}
df$R_g_2 <- df$k * tau * 2*df$k^{-3}
df$R_g_3 <- df$k * tau * 3*df$k^{-4}

df %>% pivot_longer(starts_with("R_g")) %>%
ggplot(., aes(x = k, y = value, color = name)) + 
  geom_step() +
  #geom_line(color = "red") +
  # scale_y_log10() +
  # scale_x_log10() +
  gatherings_theme() +
  labs(
    x = "Gathering size (M)",
    y = bquote(R[g])
  )
