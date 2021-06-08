library(cmdstanr)
library(posterior)
library(bayesplot)
df <-
  simulate_gatherings(
    N = 100000,
    pi = 0.02,
    pr = 0,
    dist = dist_m$M,
    wt = dist_m$prob,
    return_SIR = TRUE
  )

df <- df$SIR[df$SIR[, "I"] == 1, ]
df <- df[rowSums(df[, c("S", "I", "R")]) > 1, ]
stan_data <- list(
  obs = nrow(df),
  p_i = 0.02,
  p_r = 0,
  x = df[, "delta"],
  N = rowSums(df[, c("S", "I", "R")]) - 1
)

mod <- cmdstan_model("2_code/gatherings_model.stan", compile = FALSE)
mod$compile(cpp_options = list(STAN_THREADS = TRUE, STAN_OPENCL = TRUE))
mod$exe_file()

fit <-
  mod$sample(
    data = stan_data,
    iter_warmup = 1000,
    iter_sampling = 2000,
    parallel_chains = 4,
    seed = 58798235
  )
