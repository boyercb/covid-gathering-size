library(cmdstanr)
library(posterior)
library(bayesplot)

df <- filter(lorenz_results, phi == 10)

stan_data <- list(
  N = nrow(df),
  y = df$X_work
)

mod <- cmdstan_model("2_code/find_k.stan", compile = FALSE)
mod$compile(cpp_options = list(STAN_THREADS = TRUE, STAN_OPENCL = TRUE))
mod$exe_file()

fit <- mod$optimize(data = stan_data)
fit$mle()
# fit <-
#   mod$sample(
#     data = stan_data,
#     iter_warmup = 1000,
#     iter_sampling = 2000,
#     parallel_chains = 4,
#     seed = 58798235
#   )
