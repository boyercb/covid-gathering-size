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
                  rgamma(x, 0.16, 0.16 / 2.5)) {  # <-- This can be changed to whatever distribution we want!
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
  
  # draw infectiousness (nu) for each infected and use poisson to get expected r
  r <- mapply(sim_infections, N = N, S = y[, "S"], I = y[, "I"], R = y[, "R"])
  
  # flip S q-probability coins to determine number who get infected
  delta <- sapply(r, sum)

  # return results
  if (return_SIR) {
    return(list(
      "y" = y,
      "r" = unlist(r),
      "delta" = delta
    ))
  } else {
    return(delta)
  }
}

# This function simulates a gathering of size N drawn from a source population
# in which the prevalence of susceptibility is given by ps. The function also
# allows us to specify the distribution for nu.
dgp_r <- function(N,
                  ps,
                  T = 1,
                  D = 10,
                  sims = 1,
                  return_SIR = FALSE,
                  nu_dist = function(x)
                    rgamma(length(x), x * 0.45, 0.45 / 0.58)) {

  if (ps < 1) { # if there are immune types
    # draw random sample of attendees from population of
    # 1. susceptibles 
    # 2. recovereds
    y <- rmultinom(sims, N, c(ps, 1 - ps))
    y <- rbind(y[1, ], 0, y[2, ])
    y <- t(y)
  } else { # otherwise fix 1 infected and N - 1 recored
    y <- matrix(rep(c(N - 1, 1, 0), each = sims), nrow = sims, ncol = 3)
  }
  
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

sim_infections <- function(N, S, I, R) {
  if (I != 0) {
    r_i <- rpois(I, rgamma(I, 0.16, 0.16 / 2.5))
    r_i <- ifelse(r_i > N-1, N-1, r_i)
    
    susceptible <- S
    exposed <- 0
    infected <- I
    recovered <- R
    r_new <- vector()
    
    for (i in 1:length(r_i)) {
      r_new[i] <- rhyper(1, susceptible, exposed + infected + recovered - 1, r_i[i])
      susceptible <- susceptible - r_new[i]
      exposed <- exposed + r_new[i]
    }
  } else {
    r_new <- 0
  }
  return(r_new)
}




