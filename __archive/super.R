# Expected number of cases using exact solution ---------------------------

N <- seq(2, 20, 1)           # size of gatherings
p <- seq(0.001, 0.1, 0.001)  # prevalence of infection
c <- seq(0.2, 0.8, 0.2)      # proportion of contacts that are qhigh 
qlow <- seq(0.5, 0.1, -0.1)   # probability of infection given contact

df <- expand.grid(N = N, p = p, qlow = qlow, c = c)
df$qhigh <- 1 - df$qlow

df$dI <- with(df, N * (1 - c) * (1 - p) * (1 - (1 - qlow)^(N * (1 - c) * p)) + 
                N * c * (1 - p) * (1 - (1 - qhigh)^(N * c * p)))

q_labs <- c(
  "0.5" = "low = 0.5,\n high = 0.5",
  "0.4" = "low = 0.4,\n high = 0.6",
  "0.3" = "low = 0.3,\n high = 0.7",
  "0.2" = "low = 0.2,\n high = 0.8",
  "0.1" = "low = 0.1,\n high = 0.9"
)

c_labs <- c(
  "0.2" = "c = 0.2",
  "0.4" = "c = 0.4",
  "0.6" = "c = 0.6",
  "0.8" = "c = 0.8"
)

pdf("super.pdf", height = 7.5, width = 13.33)
ggplot(df, aes(x = N, y = p, z = dI)) +
  facet_grid(qlow ~ c, labeller = labeller(qlow = q_labs, c = c_labs )) +
  geom_contour_filled(binwidth = 1) +
  xlab("size of gathering (N)") + 
  scale_fill_viridis_d(name = "expected\nnew cases", option = "plasma") +
  coord_cartesian(expand = F)
dev.off()



