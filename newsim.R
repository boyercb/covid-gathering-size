library(ggplot2)

# What is relationship between gathering size and infection risk? ---------


# Set up event simulation -------------------------------------------------

simulate_event <- function(size, p, q) {
  event <- rbinom(size, 1, p)
  inf <- which(event == 1)
  sus <- which(event == 0)
  
  if (length(inf) != 0) {
    contacts <- expand.grid(inf = inf, sus = sus)
    contacts$spread <- rbinom(nrow(contacts), 1, q)
  } else {
    contacts <- NULL
  }
  
  cases <-  contacts$sus[contacts$spread == 1]
  
  post <- event + as.numeric(1:size %in% cases)
  
  return(sum(post - event))
}


# Define simulation parameters --------------------------------------------

sims <- 10000              # number of simulations per parameter combo
size <- seq(2, 20, 1)      # size of gatherings
p <- seq(0.025, 1, 0.025)  # prevalence of infection
q <- seq(0.2, 0.8, 0.2)    # probability of infection given contact

params <- expand.grid(size = size, p = p, q = q)


# Run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = sims, initial = NA, style = 3)

raw <- replicate(sims, {
  i <- getTxtProgressBar(pb)
  setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
  mapply(simulate_event, params$size, params$p, params$q)
})

close(pb)

results <- params
results$cases <- rowMeans(raw)
results$max <- apply(raw, 1, max)


# Plot results ------------------------------------------------------------

q_labs <- paste0("q = ", q)
names(q_labs) <- q

pdf("newsim1.pdf", height = 7.5, width = 13.33)
  ggplot(results, aes(x = size, y = p, z = cases)) +
    facet_grid( ~ q, labeller = labeller(q = q_labs)) +
    geom_contour_filled(binwidth = 1) +
    xlab("size of gathering") + 
    scale_fill_viridis_d(name = "expected\nnew cases", option = "plasma")
dev.off()

pdf("newsim2.pdf", height = 7.5, width = 13.33)
ggplot(results, aes(x = size, y = p, z = cases)) +
  facet_grid( ~ q, labeller = labeller(q = q_labs)) +
  geom_contour_filled(binwidth = 1) +
  annotate("text", x = 12, y = 0.45, label = "R > 1", size = 5) +
  xlab("size of gathering") + 
  scale_fill_manual(
    name = "expected\nnew cases", 
    values = c("#0D0887B3", rep("#F0F921B3", 15))
  ) + 
  theme(legend.position = "none")
dev.off()