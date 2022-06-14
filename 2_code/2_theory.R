source('2_code/0_packages.R')

# Figure 1: panel a -------------------------------------------------------

# create figure showing relationship between gathering size and expected 
# incident cases at a given size
tau <- 0.08
pi <- 0.05
ps <- 0.80

df <- tibble(
  k = seq(1, 244),
  x = ps * (k) * (1 - (1 - tau) ^ (pi * (k)))
)

pdf("3_results/expected_infections_example.pdf", width = 4.5, height = 3.5)
ggplot(df, aes(x = k, y = x)) + 
  geom_line() +
  annotate(
    "text",
    x = 15,
    y = 15,
    label = "k * p[s] * (1 - (1 - tau)^{k*p[i]})",
    family = "Palatino",
    parse = TRUE,
    hjust = "center"
  ) +
  scale_x_continuous(trans = "log10",
                     breaks = c(seq(1, 10, by = 1), seq(20, 100, by = 10), 200),
                     labels = c(1, rep("", 8), 10, rep("", 3), 50, rep("", 4), 100, "")) +   #annotation_logticks(sides ="b") +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  labs(
    x = "Gathering size (k)",
    y = bquote(E(X * "|" * K == k))
  )
dev.off()


# Figure 1: panel b -------------------------------------------------------

# Create figure showing example "heavy-tailed" gathering size distributions 
N <- 1:1000

df <- tibble(
  N = N,
  X_pl = dpldis(N, 1, 1.5),
  X_ln = dlnorm(N, 0, 1),
  X_yu = VGAM::dyules(N, 1.25),
  X_exp = dexp(N, 1),
) 

df <- df %>% 
  pivot_longer(cols = -c(N)) %>%
  separate(name, c("variable", "distribution")) %>%
  mutate(
    distribution = case_when(
      distribution == "pl" ~ "Power law",
      distribution == "ln" ~ "Log-normal",
      distribution == "yu" ~ "Yule",
      distribution == "exp" ~ "Exponential"    
    ),
    distribution = factor(distribution,
                          levels = c("Power law", "Yule", "Log-normal", "Exponential"))
  )

df <- filter(df, value > 10^(-4))
pdf("3_results/powerlaw_example.pdf", width = 4.5, height = 3.5)
ggplot(df, aes(x = N, y = value, color = distribution, linetype = distribution)) + 
  geom_line() +
  scale_color_grey(name = "") +
  scale_linetype(name = "") +
  scale_y_continuous(trans = "log10", labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_x_continuous(trans = "log10",
                     breaks= c(seq(1,10, by=1), seq(20, 100, by=10), 200),
                     labels = c(1, rep("",8), 10, rep("",3), 50, rep("", 4), 100, "")) + 
  # scale_x_continuous(trans = "log10", labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  # annotation_logticks() +
  coord_cartesian() +
  theme_pubr(base_size = 10, base_family = "Palatino") +
  theme(
    legend.position = c(0.85, 0.85)
  ) +
  labs(
    x = "Gathering size (k)",
    y = bquote("Pr(K > k)")
  )
dev.off()

