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
                nu_dist = function(x)
                  rgamma(x, 0.16, 0.16 / 2.5)) {  # <-- This can be changed to whatever distribution we want!
  # draw random sample of attendees from population of
  # 1. susceptibles 
  # 2. infecteds
  # 3. recovereds
  if (ps + pi == 1) {
    mat <- rmultinom(sims, N, c(ps, pi))
    mat <- rbind(mat, 0)
  } else {
    mat <- rmultinom(sims, N, c(ps, pi, 1 - ps - pi))
  }
  
  # transpose so rows are sims
  mat <- t(mat)
  
  # rename to reflect compartments
  colnames(mat) <- c("S", "I", "R")
  
  # draw infectiousness (nu) for each infected and simulate sequence of
  # infections to get effective r for each infected]
  r <-
    mapply(
      sim_infections,
      N = N,
      S = mat[, "S"],
      I = mat[, "I"],
      R = mat[, "R"],
      MoreArgs = list(
        T = T,
        D = D,
        nu_dist = nu_dist
      )
    )
  
  # calculate total number infected at gathering
  delta <- sapply(r, sum)
  mat <- cbind(mat, delta)

  # return results
  res <- list(
    "SIR" = mat,
    "r" = unlist(r),
    "r_eff" = unlist(r[which(mat[, "I"] > 0)]), 
    "delta" = delta
  )
  
  return(res)
}

# This is a helper function that draws individual infectiousness and simulates
# infection process at a gathering based on number in each initial disease state
# and the right prior distribution for infectiveness
sim_infections <- function(N,
                           S,
                           I,
                           R,
                           T = 1,
                           D = 10,
                           nu_dist = function(x)
                             rgamma(x, 0.16, 0.16 / 2.5)) {
  
  if (I != 0) {
    # draw number to infect using hierarchical poisson approach
    r_i <- rpois(I, nu_dist(I) * T / D)
    
    # cap maximum possible secondary infections at size of gathering minus 1
    r_i <- ifelse(r_i > N-1, N-1, r_i)
    
    # initialize counters
    n_s <- S
    n_e <- 0
    n_i <- I
    n_r <- R
    r_eff <- vector()
    
    # loop over infecteds and determine number of successful secondary cases
    # using draw from hypergeometric distribution where m = # of remaining
    # susceptible (success state) and n = # of recovered, exposed or already
    # infected (failure state)
    for (i in 1:I) {
      r_eff[i] <- rhyper(1, n_s, n_e + n_i + n_r - 1, r_i[i])
      n_s <- n_s - r_eff[i]
      n_e <- n_e + r_eff[i]
    }
  } else {
    r_eff <- 0
  }
  return(r_eff)
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




