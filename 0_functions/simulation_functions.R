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
    X <-
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
    X <-
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
  X_eff <- unlist(X[which(mat[, "I"] > 0)])
  
  if (length(X_eff) == 0) {
    X_eff <- 0
  }
  
  # calculate total number infected at gathering
  delta <- sapply(X, sum)
  mat <- cbind(mat, delta)

  # return results
  res <- list(
    "SIR" = mat,
    "X" = unlist(X),
    "X_eff" = X_eff, 
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
    X <- vector()
    
    # loop over infecteds and determine number of successful secondary cases
    # using draw from hypergeometric distribution where m = # of remaining
    # susceptible (success state) and n = # of recovered, exposed or already
    # infected (failure state)
    for (i in 1:I) {
      X[i] <- rhyper(1, S - E, E + I + R - 1, r_i[i])
      E <- E + X[i]
    }
  } else {
    X <- 0
  }
  return(X)
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
    X <- vector()
    
    # loop over infecteds and determine number of successful secondary cases
    # using draw from binomial distribution where N = # of remaining
    # susceptible and p = probability of infection given contact (q)
    for (k in 1:I) {
      X[k] <- rbinom(1, S - E, q_i[k])
      E <- E + X[k]
    }
  } else {
    X <- 0
  }
  return(X)
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
  # infections to get effective X for each infected]
  X <-
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
  X_eff <- unlist(X[which(mat[, "I"] > 0)])
  
  if (length(r_eff) == 0) {
    X_eff <- 0
  }
  
  # calculate total number infected at gathering
  delta <- sapply(X, sum)
  mat <- cbind(mat, delta)
  
  # return results
  res <- list(
    "SIR" = mat,
    "X" = unlist(X),
    "X_eff" = X_eff, 
    "delta" = delta
  )
  
  return(res)
}

# distribution functions for binomial and beta-binomial distributions
binom_cdf <- function(x, size, p) pbinom(x, size, p)
betabinom_cdf <- function(x, size, p, phi) rmutil::pbetabinom(x, size, p, phi)

binom_pmf <- function(x, size, p) dbinom(x, size, p)
betabinom_pmf <- function(x, size, p, phi) rmutil::dbetabinom(x, size, p, phi)



# load and modify the BBC pandemic data as we need to for the draw_gatherings() function below
bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")
bbc <- select(bbc, e_other)
# gathering sizes are contacts + 1 (index person)
bbc$M <- bbc$e_other + 1
# plotting it
bbc_data <- ggplot(data = bbc, mapping = aes(x = M)) + 
  geom_point(stat = "count") + 
  scale_y_log10() + scale_x_log10()
pdf("3_results/bbc_data.pdf", width = 5, height = 6)
print(bbc_data)
dev.off()  

# draw_gatherings() function generates M, a vector of gatherings drawn from the
# BBC pandemic distribution so that in total N people attend,
# under 4 options for restriction in gathering size up to c : 0 for no restriction, 
# 1 for replacement by c, 2 for replacement by 0 and 3 for replacement by another draw smaller to c

draw_gatherings <- function(N = 10000, #10000 as default, can change
                            c = 10, # cut-off as default, can change
                            option) { # define options for restriction (1, 2, or 3)
# initialize loop variables
M <- vector()
i <- 1
# create subset of bbc distribution <= c (for option 3)
smallM <- sample(bbc$M[bbc$M <= c])
# loop until sum of gatherings is equal to pop size
while(sum(M) != N) {
  # draw gathering of size m from BBC pandemic distribution
  M[i] <- sample(bbc$M, 1) 
  if (M[i] > c) {
    M[i] <- switch(
      option,
      # option 0 : no restriction to gathering size 
      "0" = M[i],
      # option 1: set to c (most conservative)
      "1" = c,
      # option 2: set to 1 (least conservative)
      "2" = 1,
      # option 3: redraw until less than c (realistic?) 
      #"3" = while (M[i] > c) # Chris I'm not sure what you were trying to do here (and it didn't seem to work?)
      #  sample(bbc$M, 1)
      "3" = sample(smallM, 1) #So I did that instead
    )
  }
  # if sum(m) is less than pop size, then advance to next gathering
  # otherwise redraw existing gathering
  if (sum(M) <= N) { 
    i <- i + 1
  } 
} 
return(M)
} 

# apply the draw_SIR_attendees() function to each of the gatherings from draw_gatherings()
# get distribution of S, I and R in each gathering 

gath <- as.matrix(draw_gatherings(option = "0" ))
head(gath)
test1 <- lapply(gath,draw_SIR_attendees,ps= 0.95, pi = 0.05, sims = 1)
head(test1)
test2 <- matrix(unlist(test1), ncol = 3, byrow = TRUE)
head(test2)
test3 <- data.frame(gath, test2)
colnames(test3) <- c("N", "S", "I", "R")
head(test3)
#sum(test3$N) # check yes

#draw_SIR_gatherings <- function() #Need to make a function out of this

#


