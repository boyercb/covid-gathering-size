library(tidyverse)
library(poweRlaw)
library(progressr)
library(furrr)

# define helper functions -------------------------------------------------

# calculates the expected rate per gathering of size k (derived in paper)
expected_rate_per_gathering <- function(k, tau, pi, pr) {
  I(k > 1) * k * (1 - pi - pr) * (1 - (1 - tau)^(k * pi))
}

# simulates draws from discrete power law under gathering size restriction by
# re-drawing until size is lower than restriction threshold
restricted_rpldis <- function(n, xmin, xmax, alpha) {
  k <- rpldis(n = n, xmin = xmin, alpha = alpha)
  
  if (any(k > xmax)) {
    k[k > xmax] <- sapply(
      X = k[k > xmax],
      function(x) {
        while(x > xmax) { 
          x <- rpldis(n = 1, xmin = xmin, alpha = alpha) 
        }
        return(x)
      })
  } 
  return(k)
}

# runs the full monte carlo simulation to estimate the expected reduction in
# cases versus the unrestricted scenario
monte_carlo_simulation <- function(sims, tau, pi, pr, xmin, xmax, alpha, .details = FALSE) {
  # simulate gatherings under discrete power law (no restrictions)
  k_0 <- rpldis(n = sims, xmin = xmin, alpha = alpha)
  
  # simulate gatherings under discrete power law (restricted)
  k_1 <- restricted_rpldis(n = sims, xmin = xmin, xmax = xmax, alpha = alpha)
  
  # calculate expected rate per gathering
  X_0 <- expected_rate_per_gathering(k_0, tau, pi, pr)
  X_1 <- expected_rate_per_gathering(k_1, tau, pi, pr)
  
  p()
  
  if (.details) {
    return(
      list(
        RR = (mean(X_0) - mean(X_1)) / mean(X_0),
        k_0 = k_0,
        k_1 = k_1,
        X_0 = X_0,
        X_1 = X_1
      )
    )
  } else {
    # percent reduction in cases due to gatherings
    return((mean(X_0) - mean(X_1)) / mean(X_0))
  }
} 

# exact solution (up to a max size)
exact_solution <- function(tau, pi, pr, xmin, xmax, alpha, .details = FALSE) {
  k_0 <- 1:500
  k_1 <- 1:xmax
  
  X_0 <- sum(
    expected_rate_per_gathering(k_0, tau, pi, pr) * dpldis(k_0, xmin, alpha) / ppldis(500, xmin, alpha)
  )
  
  X_1 <- sum(
    expected_rate_per_gathering(k_1, tau, pi, pr) * dpldis(k_1, xmin, alpha) / ppldis(xmax, xmin, alpha)
  )

  if (.details) {
    return(
      list(
        RR = (X_0 - X_1) / X_0,
        # k_0 = k_0,
        # k_1 = k_1,
        X_0 = X_0,
        X_1 = X_1
      )
    )
  } else {
    return((X_0 - X_1) / X_0)
  }
}

# exact solution (up to a max size)
exact_Rg <- function(tau, pi, pr, D, xmin, xmax, alpha, .details = FALSE) {
  k_0 <- 1:500
  k_1 <- 1:xmax
  
  Rg_0 <- sum(
    I(k_0 > 1) * k_0 * tau * (1 - pi - pr) * D * dpldis(k_0, xmin, alpha) / ppldis(500, xmin, alpha)
  )
  
  Rg_1 <- sum(
    I(k_1 > 1) * k_1 * tau * (1 - pi - pr) * D * dpldis(k_1, xmin, alpha) / ppldis(xmax, xmin, alpha)
  )
  
  if (.details) {
    return(
      list(
        RR = (Rg_0 - Rg_1) / Rg_0,
        # k_0 = k_0,
        # k_1 = k_1,
        Rg_0 = Rg_0,
        Rg_1 = Rg_1
      )
    )
  } else {
    return((Rg_0 - Rg_1) / Rg_0)
  }
}

# run simulation ----------------------------------------------------------

# define simulation parameters
params <- expand_grid(
  tau = c(0.08),
  pi = c(0.001, 0.01, 0.1),
  pr = seq(0, 1, 0.1),
  alpha = seq(1.5, 4, 0.5),
  xmin = 1,
  xmax = seq(2, 500),
  #sims = 1000
)


# run
plan(multisession, workers = 10)

# with_progress({
#   p <- progressor(steps = nrow(params))
#   
#   RR <-
#     future_pmap_dbl(as.list(params),
#                 monte_carlo_simulation,
#                 .options = furrr_options(seed = TRUE))
# })
#   

with_progress({
  p <- progressor(steps = nrow(params))
  expected_rate <- future_pmap(as.list(params),
                               function(tau, pi, pr, xmin, xmax, alpha) {
                                 p()
                                 exact_solution(tau, pi, pr, xmin, xmax, alpha, .details = TRUE)
                               })
})

with_progress({
  p <- progressor(steps = nrow(params))
  Rg <- future_pmap(as.list(params),
                    function(tau, pi, pr, D, xmin, xmax, alpha) {
                      p()
                      exact_Rg(tau, pi, pr, D, xmin, xmax, alpha, .details = TRUE)
                    }, 
                    D = 5)
})

plan(sequential)

expected_rate <- bind_rows(expected_rate)
Rg <- bind_rows(Rg)
results <- bind_cols(
  params,
  RR = expected_rate$RR,
  X_0 = expected_rate$X_0,
  X_1 = expected_rate$X_1,
  Rg_0 = Rg$Rg_0,
  Rg_1 = Rg$Rg_1,
  RR_Rg = Rg$RR
)


# empirical restriction estimates -----------------------------------------

empirical_pdfs <- DFcontactsprop %>% 
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
      Rg_1 = sum(tau * (1 - pi - pr) * D * n_gatherings[restricted] * value[restricted], na.rm = TRUE),
      Rg_0 = sum(tau * (1 - pi - pr) * D * n_gatherings * value, na.rm = TRUE),
      RR = X_1 / X_0,
      RR_Rg = Rg_1 / Rg_0,
      k_max = xmax,
      .groups = 'drop'
    )
}

params <- expand_grid(
  tau = c(0.08),
  pi = 0.01,
  pr = 0,
  D = 5,
  xmax = seq(2, 500)
)

empirical_results <- 
  pmap_dfr(as.list(params), calc_empirical_RR, pdfs = empirical_pdfs)

pdf("3_results/rr_kmax_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results, aes(x = k_max, y = RR, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    labels = c("Sekara et al.",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X[t]^{k[max]})/E(X[t]))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_log10_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results, aes(x = k_max, y = RR, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    guide = FALSE,
    labels = c("Sekara et al.",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X[t]^{k[max]})/E(X[t]))
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_Rg_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results, aes(x = k_max, y = RR_Rg, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    labels = c("Sekara et al.",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(R[g]^{k[max]}/R[g])
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_Rg_log10_empirical.pdf", width = 4.5, height = 3.5)
ggplot(empirical_results, aes(x = k_max, y = RR_Rg, color = fct_rev(name), group = fct_rev(name))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(
    name = "Data source", 
    values = my_col5,
    guide = FALSE,
    labels = c("Sekara et al.",
               "BBC work/school",
               "BBC total",
               "BBC other",
               "BBC home")) +
  labs(
    x = bquote(k[max]),
    y = bquote(R[g]^{k[max]}/R[g])
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()


# plot results ------------------------------------------------------------

results$alpha <- factor(results$alpha)

pdf("3_results/rr_kmax.pdf", width = 4.5, height = 3.5)
ggplot(filter(results, pr == 0, pi == 0.01), aes(x = xmax, y = log(X_1) - log(X_0), fill = fct_rev(alpha), color = fct_rev(alpha), group = fct_rev(alpha))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X[t]^{k[max]})/E(X[t]))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_log10.pdf", width = 4.5, height = 3.5)
ggplot(filter(results, pr == 0, pi == 0.01), aes(x = xmax, y = log(X_1) - log(X_0), fill = fct_rev(alpha), color = fct_rev(alpha), group = fct_rev(alpha))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  scale_fill_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X[t]^{k[max]})/E(X[t]))
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino")
dev.off()

pdf("3_results/rr_kmax_Rg.pdf", width = 4.5, height = 3.5)
ggplot(filter(results, pr == 0, pi == 0.01), aes(x = xmax, y = 1 - RR_Rg, fill = fct_rev(alpha), color = fct_rev(alpha), group = fct_rev(alpha))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(R[g]^{k[max]}/R[g])
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.85, 0.35)) 
dev.off()

pdf("3_results/rr_kmax_Rg_log10.pdf", width = 4.5, height = 3.5)
ggplot(filter(results, pr == 0, pi == 0.01), aes(x = xmax, y = 1 - RR_Rg, fill = fct_rev(alpha), color = fct_rev(alpha), group = fct_rev(alpha))) + 
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  scale_fill_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(R[g]^{k[max]}/R[g])
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  theme_pubr(base_size = 11, base_family = "Palatino") 
dev.off()


ggplot(filter(results, pi == 0.01 & pr %in% seq(0, 1, 0.2)), aes(x = xmax, y = 1 - RR, fill = factor(pr), color = factor(pr), group = factor(pr))) + 
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
    y = bquote(E(X[t]^{k[max]})/E(X[t]))
  ) +
  theme_minimal(base_size = 14, base_family = "Palatino") 




calc_moments <- function(pdfs) {
  pdfs %>%
    group_by(name) %>%
    summarise(
      M_1 = sum(n_gatherings * value, na.rm = TRUE),
      M_2 = sum(n_gatherings^2 * value, na.rm = TRUE),
      var = M_2 - M_1^2,
      sd = sqrt(var),
      q90 = first(n_gatherings[cumsum(value[!is.na(value)]) >= 0.90]), 
      q99 = first(n_gatherings[cumsum(value[!is.na(value)]) >= 0.99]), 
      .groups = 'drop'
    )
}

calc_moments(empirical_pdfs)

DFcontactsprop %>% summarise(Sekara_S9 = sum(Sekara_S9, na.rm = TRUE),
                             BBC_home = sum(BBC_home, na.rm = TRUE),
                             BBC_work = sum(BBC_work, na.rm = TRUE),
                             BBC_other = sum(BBC_other, na.rm = TRUE),
                             BBC_total = sum(BBC_total, na.rm = TRUE)
                             )
ggplot(filter(results, pr == 0, pi == 0.01) %>% pivot_longer(c("Rg_1", "Rg_0")), aes(x = xmax, y = value, color = name)) + 
  facet_wrap(~alpha, scales = "free_y") + 
  geom_line(size = 1.05) +
  #geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  scale_fill_brewer(palette = "Reds", guide = FALSE, direction = -1) +
  labs(
    x = bquote(k[max]),
    y = bquote(R[g]^{k[max]}/R[g])
  ) +
  theme_minimal(base_size = 14)

ggplot(filter(results, pi == 0.01), aes(x = xmax, y = pr, fill = Rg_0)) + 
  facet_grid(~alpha) +
  geom_tile()
  #geom_contour_filled()
  