# exploring heterogeneity via Lorenz curve --------------------------------

lorenz_params <- 
  expand_grid(
    phi = c(0.1, 1, 10, 100)
  )

lorenz_results <- 
  map_dfr(lorenz_params$phi, function(phi) {
    X <- offspring_model(
      N = 100000,
      k_work = k_work,
      k_gather = k_gather,
      k_home = k_home,
      phi_work = phi,
      phi_gather = phi,
      phi_home = phi
    )
    return(tibble(phi = phi, as_tibble(X)))
  }) 

lorenz_plot <- lorenz_results %>% 
  pivot_longer(cols = starts_with("X_")) %>%
  mutate(
    name = factor(
      x = name,
      levels = c("X_home", "X_gather", "X_work"),
      labels = c("Home", "Gatherings", "Work")
    ),
    selected = case_when(
      phi == 1 & name == "Gatherings" ~ 1,
      phi == 10 & name %in% c("Work", "Home") ~ 1,
      TRUE ~ 5
    )
  )  

lorenz_plot1 <- 
  ggplot(lorenz_plot, aes(x = value, color = factor(phi))) +
  facet_grid( ~ name) +
  gglorenz::stat_lorenz(desc = TRUE, geom = "line") +
  scale_color_brewer(name = bquote("Dispersion\nparameter"*phi), palette = "Reds") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  xlab("\nProportion of infectious cases") +
  ylab("Proportion of secondary cases\n") +
  gatherings_theme() +
  theme(
    legend.position = c(0.92, 0.15)
  )

pdf("3_results/lorenz_plot1.pdf", width = 9.6, height = 5.8)
print(lorenz_plot1)
dev.off()

lorenz_plot2 <-
  ggplot(lorenz_plot, aes(x = value, color = factor(selected), group = factor(phi))) +
  facet_grid( ~ name) +
  gglorenz::stat_lorenz(desc = TRUE) +
  geom_text(
    aes(x = x, y = y, label = label),
    data = tibble(
      name = factor(
        c("X_home", "X_gather", "X_work"),
        levels = c("X_home", "X_gather", "X_work"),
        labels = c("Home", "Gatherings", "Work")
      ),
      phi = c(10, 1, 10),
      selected = c(1, 1, 1),
      label = c(bquote(phi == 10), bquote(phi == 1), bquote(phi == 10)),
      x = c(0.4, 0.22, 0.25),
      y = c(0.7, 0.9, 0.85)
    ),
    family = "Palatino",
    parse = TRUE
  ) + 
  scale_color_manual(values = c("red", alpha("black", 0.4))) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  xlab("\nProportion of infectious cases") +
  ylab("Proportion of secondary cases\n") +
  gatherings_theme() +
  theme(
    legend.position = "none"
  )

pdf("3_results/lorenz_plot2.pdf", width = 9.6, height = 5.8)
print(lorenz_plot2)
dev.off()