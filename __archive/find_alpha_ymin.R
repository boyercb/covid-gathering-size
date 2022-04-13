library(cmdstanr)
# library(posterior)
# library(bayesplot)

stan_data <- list(
  N = nrow(k_bbc),
  y = k_bbc$e_other + 1
)

mod <- cmdstan_model("2_code/find_alpha_ymin.stan")

fit <- mod$optimize(data = stan_data, init = list(list(alpha = 2, ymin = 1)))

fit$mle()