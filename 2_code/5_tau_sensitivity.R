# sensitivity analysis - varying tau

source('2_code/0_packages.R')
source('2_code/1_functions.R')

rerun_simulation <- TRUE

# vary dispersion using beta distribution ---------------------------------

# first plot our distribution choices

beta_df <- tibble(
  scenario1 = rbeta(10000, 0.08, 0.92),
  scenario2 = rbeta(10000, 0.8, 9.2),
  scenario3 = rbeta(10000, 800, 9200)
)

beta_df <-
  pivot_longer(beta_df, cols = everything())

pdf("3_results/beta_distribution.pdf", width = 6.5, height = 5)
ggplot(beta_df, aes(x = value)) +
  facet_grid(~name, scales = "free_x") +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.08, linetype = "dashed") +
  theme_minimal(base_size = 10, base_family = "Palatino") +
  labs(
    x = NULL,
    y = NULL
  )
dev.off()


# we want to allow tau to vary according to a distribution rather than being a fixed value

N_tau_sims <- 10000

sens1_params <- expand_grid(
  tau_params = list(
    c(0.08, 0.92), # 1 / (1 + 1) = 0.5
    c(0.8, 9.2),      # 1 / (8 + 92 + 1) = 1/11
    c(800, 9200)   # 1 / (800 + 9200 + 1) = 1/10001
  ),
  pi = c(0.01),
  pr = 0, 
  alpha = seq(1.5, 4, 0.5),
  xmin = 1,
  xmax = seq(2, 500)
)

if (rerun_simulation) {
  plan(multisession, workers = 8)
  
  with_progress({
    p <- progressor(steps = nrow(sens1_params))
    sens1_expected_rate <- future_pmap(
      .l = as.list(sens1_params),
      .f = function(tau_params, pi, pr, xmin, xmax, alpha) {
        p()
        observed_tau <-
          rbeta(N_tau_sims, tau_params[1], tau_params[2])
        
        RR <- map_dbl(observed_tau,
                      function(x)
                        exact_solution(x, pi, pr, xmin, xmax, alpha, .details = FALSE))
        
        list(RR = mean(RR),
             RR_sd = sd(RR))
      },
      .options = furrr_options(seed = TRUE)
    )
  })
  
  
  plan(sequential)
  
  write_rds(sens1_expected_rate, "1_data/sens1_expected_rate.rds")

} else {
  sens1_expected_rate <- read_rds("1_data/sens1_expected_rate.rds")
}

sens1_expected_rate <- bind_rows(sens1_expected_rate)

sens1_results <- 
  sens1_params %>%
  mutate(
    dispersion = map_chr(tau_params, function (x) case_when(
      x[1] == 0.08 ~ "0.5",
      x[1] == 0.8 ~ "0.1",
      x[1] == 800 ~ "0.0001"
    )) 
  ) %>%
  bind_cols(
    RR = sens1_expected_rate$RR,
    RR_sd = sens1_expected_rate$RR_sd
  )


# vary tau mean -----------------------------------------------------------

# define simulation parameters
sens2_params <- expand_grid(
  tau = c(0.01, 0.08, 0.25),
  pi = 0.01,
  pr = 0,
  alpha = seq(1.5, 4, 0.5),
  xmin = 1,
  xmax = seq(2, 500)
)

if (rerun_simulation) {

  plan(multisession, workers = 8)

  with_progress({
    p <- progressor(steps = nrow(sens2_params))
    sens2_expected_rate <- future_pmap(
      .l = as.list(sens2_params),
      .f = function(tau, pi, pr, xmin, xmax, alpha) {
        p()
        exact_solution(tau, pi, pr, xmin, xmax, alpha, .details = TRUE)
      }
    )
  })
  
  plan(sequential)
  
  write_rds(sens2_expected_rate, "1_data/sens2_expected_rate.rds")
  
} else {
  sens2_expected_rate <- read_rds("1_data/sens2_expected_rate.rds")
}
sens2_expected_rate <- bind_rows(sens2_expected_rate)

sens2_results <- 
  sens2_params %>%
  mutate(
    tau = factor(tau, levels = c(0.08, 0.01, 0.25))
  ) %>%
  bind_cols(
    RR = sens2_expected_rate$RR,
    X_0 = sens2_expected_rate$X_0,
    X_1 = sens2_expected_rate$X_1
  )


# vary tau with gathering size --------------------------------------------

# three gathering size bins 
# 0 - 10 
# 10 - 50
# 50 +

# three tau values
# 0.01
# 0.08
# 0.25

# 3 scenarios 
# low to high, 0 - 10 = 0.01; 11-50 = 0.08; 51+ = 0.25;
# all = 0.08
# high to low, 0-10 = 0.25; 11-50 = 0.08; 51+ = 0.01;

# define simulation parameters
sens3_params <- expand_grid(
  scenario = c(1, 2, 3),
  pi = c(0.01),
  pr = seq(0),
  alpha = seq(1.5, 4, 0.5),
  xmin = 1,
  xmax = seq(2, 500)
)

if (rerun_simulation) {
  plan(multisession, workers = 8)
  
  with_progress({
    p <- progressor(steps = nrow(sens3_params))
    sens3_expected_rate <- future_pmap(
      .l = as.list(sens3_params),
      .f = function(scenario, pi, pr, xmin, xmax, alpha) {
        p()
        exact_solution(0.08, pi, pr, xmin, xmax, alpha, .details = TRUE, scenario)
      }
    )
  })
  
  plan(sequential)
  
  write_rds(sens3_expected_rate, "1_data/sens3_expected_rate.rds")
  
} else {
  sens3_expected_rate <- read_rds("1_data/sens3_expected_rate.rds")
}

sens3_expected_rate <- bind_rows(sens3_expected_rate)

sens3_results <- 
  sens3_params %>%
  mutate(
    scenario = factor(scenario, 
                      levels = c(2, 1, 3))
  ) %>%
  bind_cols(
    RR = sens3_expected_rate$RR,
    X_0 = sens3_expected_rate$X_0,
    X_1 = sens3_expected_rate$X_1
  )


# plots -------------------------------------------------------------------

pdf("3_results/rr_kmax_tau_sens1_mean.pdf", width = 6.5, height = 5)
ggplot(
  sens1_results,
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(factor(alpha)),
    color = fct_rev(factor(alpha)),
    linetype = fct_rev(dispersion)
  )
) +
  facet_grid(~ factor(alpha, labels = c(
    "alpha == 1.5",
    "alpha == 2",
    "alpha == 2.5",
    "alpha == 3",
    "alpha == 3.5",
    "alpha == 4"
  )), labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_ribbon(aes(ymin = (1-RR) - 2 * RR_sd, ymax = (1 - RR) + 2 * RR_sd, alpha = fct_rev(dispersion))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_linetype_discrete(name = "dispersion") +
  scale_alpha_discrete(name = "dispersion") +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.93, 0.20)) 
dev.off()

pdf("3_results/rr_kmax_tau_sens1_w_sd.pdf", width = 6.5, height = 5)
ggplot(
  sens1_results,
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(factor(alpha)),
    color = fct_rev(factor(alpha)),
    linetype = fct_rev(dispersion)
  )
) +
  facet_grid(~ factor(alpha, labels = c(
    "alpha == 1.5",
    "alpha == 2",
    "alpha == 2.5",
    "alpha == 3",
    "alpha == 3.5",
    "alpha == 4"
  )), labeller = label_parsed) +
  geom_line(size = 1.05) +
  # geom_ribbon(aes(ymin = (1-RR) - 2 * RR_sd, ymax = (1 - RR) + 2 * RR_sd, alpha = fct_rev(dispersion))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_linetype_discrete(name = "dispersion") +
  scale_alpha_discrete(name = "dispersion") +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.93, 0.20)) 
dev.off()

pdf("3_results/rr_kmax_tau_sens2.pdf", width = 6.5, height = 5)
ggplot(
  sens2_results,
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(factor(alpha)),
    color = fct_rev(factor(alpha)), 
    linetype = tau
  )
) +
  facet_grid(~ factor(alpha, labels = c(
    "alpha == 1.5",
    "alpha == 2",
    "alpha == 2.5",
    "alpha == 3",
    "alpha == 3.5",
    "alpha == 4"
  )), labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_linetype_discrete(name = bquote(tau), breaks = c(0.25, 0.08, 0.01)) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.93, 0.20)) 
dev.off()

pdf("3_results/rr_kmax_tau_sens3.pdf", width = 6.5, height = 5)
ggplot(
  sens3_results,
  aes(
    x = xmax,
    y = 1-RR,
    fill = fct_rev(factor(alpha)),
    color = fct_rev(factor(alpha)),
    linetype = scenario
  )
) +
  facet_grid(~ factor(alpha, labels = c(
    "alpha == 1.5",
    "alpha == 2",
    "alpha == 2.5",
    "alpha == 3",
    "alpha == 3.5",
    "alpha == 4"
  )), labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1, guide = 'none') +
  scale_linetype_discrete(name = "scenario", breaks = c(3, 2, 1), labels = c(
    bquote(tau ~ decreasing ~ with ~ k),
    bquote(tau ~ independent ~ of ~ k),
    bquote(tau ~ increasing ~ with ~ k)
  )) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.93, 0.20)) 
dev.off()


ggplot(
  mutate(sens3_results, diff = X_0 - X_1),
  aes(
    x = xmax,
    y = diff,
    fill = factor(alpha),
    color = factor(alpha)
  )
) +
  facet_grid(~ scenario, labeller = label_parsed) +
  geom_line(size = 1.05) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  scale_fill_brewer(name = parse(text="alpha"), palette = "Reds", direction = -1) +
  scale_linetype_discrete(name = bquote(p[i]), breaks = c(1, 2, 3)) +
  labs(
    x = bquote(k[max]),
    y = bquote(E(X^{k[max]})/E(X))
  ) +
  theme_pubr(base_size = 11, base_family = "Palatino") +
  theme(legend.position = c(0.95, 0.20)) 

