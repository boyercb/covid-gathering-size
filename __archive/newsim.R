library(ggplot2)

# What is relationship between gathering size and infection risk? ---------

# TODO: add immunity (i.e. recovered people can attend)

# Set up event simulation -------------------------------------------------

gathering <- function(N, p, q) {
  I <- rbinom(1, N, p)
  b <- rbinom(N - I, I, q)
  
  delta <- sum(I(b > 0))
  return(delta)
}


# Define simulation parameters --------------------------------------------

sims <- 10000              # number of simulations per parameter combo
N <- seq(2, 20, 1)         # size of gatherings
p <- seq(0.025, 1, 0.025)  # prevalence of infection
q <- seq(0.2, 0.8, 0.2)    # probability of infection given contact

params <- expand.grid(N = N, p = p, q = q)


# Run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = sims, initial = NA, style = 3)

raw <- replicate(sims, {
  i <- getTxtProgressBar(pb)
  setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
  mapply(gathering, params$N, params$p, params$q)
})

close(pb)

results <- params
results$cases <- rowMeans(raw)


# Plot results ------------------------------------------------------------

q_labs <- paste0("q = ", q)
names(q_labs) <- q

pdf("newsim1.pdf", height = 7.5, width = 13.33)
  ggplot(results, aes(x = N, y = p, z = cases)) +
    facet_grid( ~ q, labeller = labeller(q = q_labs)) +
    geom_contour_filled(binwidth = 1) +
    xlab("size of gathering (N)") + 
    scale_fill_viridis_d(name = "expected\nnew cases", option = "plasma")
dev.off()

pdf("newsim2.pdf", height = 7.5, width = 13.33)
ggplot(results, aes(x = N, y = p, z = cases)) +
  facet_grid( ~ q, labeller = labeller(q = q_labs)) +
  geom_contour_filled(binwidth = 1) +
  annotate("text", x = 12, y = 0.45, label = "R > 1", size = 5) +
  xlab("size of gathering (N)") + 
  scale_fill_manual(
    name = "expected\nnew cases", 
    values = c("#0D0887B3", rep("#F0F921B3", 15))
  ) + 
  theme(legend.position = "none")
dev.off()