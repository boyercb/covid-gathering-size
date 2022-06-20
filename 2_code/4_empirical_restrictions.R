source('2_code/0_packages.R')
source('2_code/1_functions.R')
source('2_code/3_empirical_distributions.R')

rerun_simulation <- FALSE

# run simulation ----------------------------------------------------------

# define simulation parameters
params <- expand_grid(
  tau = 0.08,
  pi = c(0.001, 0.01, 0.1),
  pr = c(0, 0.25, 0.75),
  alpha = seq(1.5, 4, 0.5),
  xmin = 1,
  xmax = seq(2, 500)
)

if (rerun_simulation) {
  plan(multisession, workers = 10)
  
  with_progress({
    p <- progressor(steps = nrow(params))
    expected_rate <- future_pmap(as.list(params),
                                 function(tau, pi, pr, xmin, xmax, alpha) {
                                   p()
                                   exact_solution(tau, pi, pr, xmin, xmax, alpha, .details = TRUE)
                                 })
  })
  
  plan(sequential)
  
  write_rds(expected_rate, "1_data/expected_rate.rds")
  
} else {
  expected_rate <- read_rds("1_data/expected_rate.rds")
}

expected_rate <- bind_rows(expected_rate)

results <- bind_cols(
  params,
  RR = expected_rate$RR,
  X_0 = expected_rate$X_0,
  X_1 = expected_rate$X_1
)
results$alpha <- factor(results$alpha)

# Figure 2 ----------------------------------------------------------------


pdf("3_results/rr_kmax.pdf", width = 4.5, height = 3.5)
ggplot(
  filter(results, pr == 0 &  pi == 0.01 & tau == 0.08),
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(alpha),
    color = fct_rev(alpha),
    group = fct_rev(alpha)
  )
) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_pipr_sensitivity.pdf", width = 10, height = 5)
ggplot(
  filter(results, pr %in% c(0, 0.25, 0.75) & tau == 0.08),
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(alpha),
    color = fct_rev(alpha),
    group = fct_cross(alpha, factor(pi)),
    linetype = factor(pi)
  )
) +
  facet_grid(
    factor(
      x = pr,
      labels = c(
        "p[r] == 0",
        "p[r] == 0.25",
        "p[r] == 0.75"
      ))
           ~ factor(
    alpha,
    labels = c(
      "alpha == 1.5",
      "alpha == 2",
      "alpha == 2.5",
      "alpha == 3",
      "alpha == 3.5",
      "alpha == 4"
    )
  ), labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = "none") +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = "none") +
  scale_linetype_discrete(name = bquote(p[i])) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "bottom") 
dev.off()

pdf("3_results/rr_kmax_pipr_sensitivity_absolute.pdf", width = 9.5, height = 5)
ggplot(
  filter(results, pr %in% c(0, 0.25, 0.75) & tau == 0.08 & pi == 0.01),
  aes(
    x = xmax,
    y = X_0,
    fill = fct_rev(alpha),
    color = fct_rev(alpha),
    # group = fct_cross(alpha, factor(pi)),
    # linetype = factor(pi)
  )
) +
  facet_grid(
    factor(
      x = pr,
      labels = c(
        "p[r] == 0",
        "p[r] == 0.25",
        "p[r] == 0.75"
      ))
    ~ factor(
      alpha,
      labels = c(
        "alpha == 1.5",
        "alpha == 2",
        "alpha == 2.5",
        "alpha == 3",
        "alpha == 3.5",
        "alpha == 4"
      )
    ), labeller = label_parsed) +
  geom_line(size = 1.05) +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = "none") +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = "none") +
  scale_linetype_discrete(name = bquote(p[i])) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]}) - E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "bottom") 
dev.off()


pdf("3_results/rr_kmax_log10.pdf", width = 4.5, height = 3.5)
ggplot(
  filter(results, pr == 0 &
           pi == 0.01 &
           tau == 0.08),
  aes(
    x = xmax,
    y = log(X_1) - log(X_0),
    fill = fct_rev(alpha),
    color = fct_rev(alpha),
    group = fct_rev(alpha)
  )
) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  scale_fill_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino")
dev.off()


ggplot(
  filter(results, pi == 0.01 &
           pr %in% seq(0, 1, 0.2) & tau == 0.08),
  aes(
    x = xmax,
    y = 1 - RR,
    fill = factor(pr),
    color = factor(pr),
    group = factor(pr)
  )
) +
  facet_wrap(~alpha, scales = "free_y") + 
  geom_line(size = 1.05) +
  # geom_text(aes(x = x, y = y, label = label),
  #           data = tibble(
  #             alpha = factor(unique(params$alpha)),
  #             x = c(150, 130, 100, 67, 30, 17),
  #             y = c(0.19, 0.28, 0.44, 0.67, 0.84, 0.93),
  #             label = paste0("alpha == ", unique(params$alpha))
  #           ),
  #           parse = TRUE,
  #           nudge_x = 5,
  #           nudge_y = -0.025, hjust = "left") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  scale_fill_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_minimal(base_size = 14, base_family = "Palatino") 


# empirical restriction estimates -----------------------------------------

empirical_pdfs <- 
  DFcontactsprop %>% 
  select(-COMIX_physicalprop, -COMIX_workprop, -COMIX_schoolprop) %>%
  select(-COMIX_homeprop,
         -COMIX_otherprop,
         -COMIX_totprop, 
         -COMIX_workschoolprop) %>%
  pivot_longer(cols = ends_with("prop")) %>%
  select(n_gatherings, name, value)

calc_empirical_RR <- function(tau, pi, pr, D, xmax, pdfs) {
  pdfs <- mutate(pdfs, restricted = n_gatherings <= xmax)
  
  pdfs %>%
    group_by(name) %>%
    summarise(
      X_1 = sum(expected_rate_per_gathering(n_gatherings[restricted], tau, pi, pr) * value[restricted] , na.rm = TRUE),
      X_0 = sum(expected_rate_per_gathering(n_gatherings, tau, pi, pr) * value, na.rm = TRUE),
      RR = X_1 / X_0,
      k_max = xmax,
      .groups = 'drop'
    )
}

params <- expand_grid(
  tau = c(0.08),
  pi = c(0.001, 0.01, 0.1),
  pr = c(0, 0.25, 0.75),
  xmax = seq(2, 500)
)

empirical_results <- 
  pmap_dfr(as.list(params), calc_empirical_RR, pdfs = empirical_pdfs)

empirical_results <- 
  bind_cols(
    params %>% slice(rep(1:n(), each = 5)),
    empirical_results
  )

empirical_results %>%
  filter(pi==0.01)

pdf("3_results/rr_kmax_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results %>% filter(pi==0.01),
       aes(x = k_max, y = RR, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "", 
    values = my_col5,
    labels = c("CNS university students",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.78, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_log10_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results, aes(x = k_max, y = RR, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    guide = FALSE,
    labels = c("CNS university students",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_empirical_pipr_sensitivity.pdf", width = 8.4, height = 5)
ggplot(
  filter(empirical_results, pr %in% c(0, 0.25, 0.75)),
  aes(
    x = k_max,
    y = RR,
    fill = fct_rev(name),
    color = fct_rev(name),
    group = fct_cross(name, factor(pi)),
    linetype = factor(pi)
  )
) +
  facet_grid(
    factor(
      x = pr,
      labels = c(
        "p[r] == 0",
        "p[r] == 0.25",
        "p[r] == 0.75"
      ))
    ~ factor(fct_rev(name),
             labels = c("CNS~university~students",
                        "BBC~work/school",
                        "BBC~total",
                        "BBC~other",
                        "BBC~home")), labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    guide = "none",
    labels = c("CNS university students",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  scale_linetype_discrete(name = bquote(p[i])) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = "bottom") 
dev.off()

