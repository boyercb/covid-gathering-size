# User-defined functions --------------------------------------------------

# This function simulates a gathering of size N drawn from a source population
# in which the prevalence of susceptibility and infection are given by ps and pi
# respectively. The function also allows us to specify the distribution for nu.
dgp <- function(N,
                ps,
                pi,
                T = 1,
                D = 10,
                sims = 1,
                return_SIR = FALSE,
                nu_dist = function(x)
                  rgamma(length(x), x * 0.45, 0.45 / 0.58)) {  # <-- This can be changed to whatever distribution we want!
  # draw random sample of attendees from population of
  # 1. susceptibles 
  # 2. infecteds
  # 3. recovereds
  if (ps + pi == 1) {
    y <- rmultinom(sims, N, c(ps, pi))
    y <- rbind(y, 0)
  } else {
    y <- rmultinom(sims, N, c(ps, pi, 1 - ps - pi))
  }
  
  # transpose so rows are sims
  y <- t(y)
  
  # rename to reflect compartments
  colnames(y) <- c("S", "I", "R")
  
  # draw infectiousness (nu) for each infected
  sum_nu <- nu_dist(y[, "I"])
  
  # calculate probability of infection assuming homogeneous mixing
  q <- 1 - exp(-sum_nu * (T/D))
  
  # flip S q-probability coins to determine number who get infected
  delta <- rbinom(sims, y[, "S"], q)
  
  # return results
  if (return_SIR) {
    return(list(
      "y" = y,
      "q" = q,
      "delta" = delta
    ))
  } else {
    return(delta)
  }
}

# This function caluclates summary stats about secondary cases
summarize_dgp <- function(x) {
  c(
    "mean" = mean(x),
    "median" = median(x),
    "q1" = quantile(x, 0.25, names = F),
    "q3" = quantile(x, 0.75, names = F),
    "sd" = sd(x)
  )
}