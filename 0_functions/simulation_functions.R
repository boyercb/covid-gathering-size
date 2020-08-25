# User-defined functions --------------------------------------------------

# This function simulates a gathering of size N drawn from a source population
# in which the prevalence of susceptibility and infection are given by ps and pi
# respectively. The function also allows us to specify the distribution for nu.
dgp <- function(N,
                ps,
                pi,
                T = 5,
                D = 5,
                sims = 1,
                type = "rate",
                prior = NULL) {  # <-- This can be changed to whatever distribution we want!
  
  if (ps == 1 & pi == 0) {
    # assume 1 infected attends each gathering
    mat <- matrix(
      rep(c(N-1, 1, 0), each = sims),
      nrow = sims, 
      ncol = 3
    )
    
    # rename to reflect compartments
    colnames(mat) <- c("S", "I", "R")
  } else {
    # sample attendees from source population with given prevalence of
    # susceptibility and infection
    mat <- draw_SIR_attendees(N, ps, pi, sims)
  }
  
  if (type == "rate") {
    # if not defined set default gamma prior
    if (is.null(prior)) {
      prior <- function(x) rgamma(x, 0.16, 0.16 / 2.5)
    }
    
    # draw infectiousness rate (nu) for each infected and simulate sequence of
    # infections to get effective r for each infected]
    r <-
      mapply(
        simulate_rates,
        N = N,
        S = mat[, "S"],
        I = mat[, "I"],
        R = mat[, "R"],
        MoreArgs = list(
          T = T,
          D = D,
          nu_dist = prior
        ),
        SIMPLIFY = FALSE
      )
  } else if (type == "probability") {
    # if not defined set default beta prior
    if (is.null(prior)) {
      prior <- function(x) rbeta(x, 0.08, 0.92)
    }
    
    # draw infectiousness probability (q) for each infected and simulate
    # sequence of infections to get effective r for each infected]
    r <-
      mapply(
        simulate_probabilities,
        N = N,
        S = mat[, "S"],
        I = mat[, "I"],
        R = mat[, "R"],
        MoreArgs = list(
          q_dist = prior
        ),
        SIMPLIFY = FALSE
      )
  }
  
  # subset among gatherings in which an infected attended
  r_eff <- unlist(r[which(mat[, "I"] > 0)])
  
  if (length(r_eff) == 0) {
    r_eff <- 0
  }
  
  # calculate total number infected at gathering
  delta <- sapply(r, sum)
  mat <- cbind(mat, delta)

  # return results
  res <- list(
    "SIR" = mat,
    "r" = unlist(r),
    "r_eff" = r_eff, 
    "delta" = delta
  )
  
  return(res)
}

# draw random sample of attendees from population of
# 1. susceptibles 
# 2. infecteds
# 3. recovereds
# using the multinomial distribution
draw_SIR_attendees <- function(N, ps, pi, sims) {
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
  
  return(mat)
}


# This is a helper function that draws individual infectiousness and simulates
# infection process at a gathering based on number in each initial disease state
# and the right prior distribution for infectiveness
simulate_rates <- function(N,
                           S,
                           I,
                           R,
                           T = 5,
                           D = 5,
                           nu_dist = function(x)
                             rgamma(x, 0.16, 0.16 / 2.5)) {
  
  
  if (I != 0) {
    # draw number to infect using hierarchical poisson approach
    r_i <- rpois(I, nu_dist(I) * T / D)
    
    # cap maximum possible secondary infections at size of gathering minus 1
    r_i <- ifelse(r_i > N-1, N-1, r_i)
    
    # initialize counters
    E <- 0
    r_eff <- vector()
    
    # loop over infecteds and determine number of successful secondary cases
    # using draw from hypergeometric distribution where m = # of remaining
    # susceptible (success state) and n = # of recovered, exposed or already
    # infected (failure state)
    for (i in 1:I) {
      r_eff[i] <- rhyper(1, S - E, E + I + R - 1, r_i[i])
      E <- E + r_eff[i]
    }
  } else {
    r_eff <- 0
  }
  return(r_eff)
}


# This is a helper function that draws individual infectiousness and simulates
# infection process at a gathering based on number in each initial disease state
# and the right prior distribution for infectiveness
simulate_probabilities <- function(N,
                                   S,
                                   I,
                                   R,
                                   q_dist = function(x)
                                     rbeta(x, 0.5, 0.95)) {
  
  if (I != 0) {
    # draw individual probability of infectiousness using hierarchical
    # beta-binomial approach
    q_i <- q_dist(I)
    
    # initialize counters
    E <- 0
    r_eff <- vector()
    
    # loop over infecteds and determine number of successful secondary cases
    # using draw from binomial distribution where N = # of remaining
    # susceptible and p = probability of infection given contact (q)
    for (k in 1:I) {
      r_eff[k] <- rbinom(1, S - E, q_i[k])
      if (is.na(r_eff[k])) {
        print(q_i[k])
        print(paste0("N: ", N, " S: ", S, " I: ", I, " R: ", R))
        print(paste0("E: ", E))
        
      }
      E <- E + r_eff[k]
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


dgp_one_infected <- function(N,
                             T = 5,
                             D = 5,
                             sims = 1,
                             nu_dist = function(x)
                               rgamma(x, 0.16, 0.16 / 2.5)) {
  mat <- matrix(
    rep(c(N-1, 1, 0), each = sims),
    nrow = sims, 
    ncol = 3
  )
  
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
  
  # subset among gatherings in which an infected attended
  r_eff <- unlist(r[which(mat[, "I"] > 0)])
  
  if (length(r_eff) == 0) {
    r_eff <- 0
  }
  
  # calculate total number infected at gathering
  delta <- sapply(r, sum)
  mat <- cbind(mat, delta)
  
  # return results
  res <- list(
    "SIR" = mat,
    "r" = unlist(r),
    "r_eff" = r_eff, 
    "delta" = delta
  )
  
  return(res)
}

# distribution functions for binomial and beta-binomial distributions
binom_cdf <- function(x, size, p) pbinom(x, size, p)
betabinom_cdf <- function(x, size, p, phi) rmutil::pbetabinom(x, size, p, phi)

binom_pmf <- function(x, size, p) dbinom(x, size, p)
betabinom_pmf <- function(x, size, p, phi) rmutil::dbetabinom(x, size, p, phi)

