# how low do we have to go? -----------------------------------------------

how_low_params <- 
  expand_grid(
    c_gather = seq(2, 50),
    freq_gather = seq(1, 5),
    option = c("1", "2", "3") 
  )

how_low <- 
  pmap_dfr(as.list(how_low_params), function(c_gather, freq_gather, option) {
    X <- offspring_model(
      N = 500000,
      k_work = k_work,
      k_gather = k_gather,
      k_home = k_home,
      mu_home = 0.17,
      c_gather = c_gather,
      freq_gather = freq_gather,
      option = option
    )
    return(tibble(
      c_gather = c_gather,
      freq_gather = freq_gather,
      option = option,
      name = c("R_gather", "R_t"),
      value = c(colMeans(X)[2], sum(colMeans(X)))
    ))
  }) 

# frequency set to 5
how_low_plot1 <- 
  ggplot(filter(how_low, freq_gather == 5), aes(x = c_gather, y = value, color = factor(
    option,
    levels = c("1", "3", "2"),
    labels = c(
      "(1) Set gathering size to limit",
      "(2) Redraw",
      "(3) Set gathering size to 1"
    )))) +
  facet_grid(~ factor(name, labels = c(bquote(R[g]), bquote(R[t])))) +
  geom_hline(
    yintercept = 1,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  geom_step(size = 1.1) +
  scale_color_brewer(name = "Restriction type", palette = "Greens") +
  ylab("") +
  xlab("Gathering size limit (c)") +
  gatherings_theme() +
  theme(
    legend.position = c(0.15, 0.85)
  )

pdf("3_results/how_low_plot1.pdf", width = 9.6, height = 5.8)
print(how_low_plot1)
dev.off()

# with frequency
how_low_plot2 <- 
  ggplot(how_low, aes(x = c_gather, y = value, color = factor(freq_gather))) +
  facet_grid(factor(
    option,
    levels = c("1", "3", "2"),
    labels = c(
      "(1)\nSet gathering size to limit",
      "(2)\nRedraw",
      "(3)\nSet gathering size to 1"
    )
  ) ~ factor(name, labels = c(bquote(R[g]), bquote(R[t])))) +
  geom_hline(
    yintercept = 1,
    linetype = 'dashed',
    color = 'grey70'
  ) +
  geom_step(size = 1.1) +
  scale_color_brewer(name = "frequency per\ninfectious period", palette = "Purples") +
  ylab("") +
  xlab("Gathering size limit (c)") +
  gatherings_theme() +
  theme(
    legend.position = 'right'
  )

pdf("3_results/how_low_plot2.pdf", width = 9.6, height = 5.8)
print(how_low_plot2)
dev.off()