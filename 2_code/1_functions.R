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
exact_solution <- function(tau, pi, pr, xmin, xmax, alpha, .details = FALSE, scenario = 2) {
  k_0 <- 1:500
  k_1 <- 1:xmax
  
  if (tau < 1e-12) {
    tau <- 1e-12
  }
  
  if (scenario == 1) {
    tau_0 <- case_when(
      k_0 <= 10 ~ 0.01,
      k_0 > 10 & k_0 <= 50 ~ 0.08,
      k_0 > 50 ~ 0.25
    )
    tau_1 <- case_when(
      k_1 <= 10 ~ 0.01,
      k_1 > 10 & k_1 <= 50 ~ 0.08,
      k_1 > 50 ~ 0.25
    )
  } else if (scenario == 2) {
    tau_0 <- tau
    tau_1 <- tau
  } else if (scenario == 3) {
    tau_0 <- case_when(
      k_0 <= 10 ~ 0.25,
      k_0 > 10 & k_0 <= 50 ~ 0.08,
      k_0 > 50 ~ 0.01
    )
    tau_1 <- case_when(
      k_1 <= 10 ~ 0.25,
      k_1 > 10 & k_1 <= 50 ~ 0.08,
      k_1 > 50 ~ 0.01
    )
  }
  
  X_0 <- sum(
    expected_rate_per_gathering(k_0, tau_0, pi, pr) * dpldis(k_0, xmin, alpha) / ppldis(500, xmin, alpha)
  )
  
  X_1 <- sum(
    expected_rate_per_gathering(k_1, tau_1, pi, pr) * dpldis(k_1, xmin, alpha) / ppldis(xmax, xmin, alpha)
  )
  
  if (.details) {
    return(
      list(
        RR = (X_0 - X_1) / X_0,
        X_0 = X_0,
        X_1 = X_1
      )
    )
  } else {
    return((X_0 - X_1) / X_0)
  }
}
