# This function simulates a gathering of size N drawn from a source population
# in which the prevalence of susceptibility and infection are given by ps and pi
# respectively. The function also allows us to specify the distribution for q.
dgp <- function(N,
                ps,
                pi,
                q_dist = function(x)
                  rbeta(x, 1, 1)) {  # <-- This can be changed to whatever distribution we want!
  # draw random sample of attendees from population of
  # 1. susceptibles 
  # 2. infecteds
  # 3. recovereds
  y <- rmultinom(N, 1, c(ps, pi, 1 - ps - pi))
  
  # sum up the number attending from each category and rename
  v <- rowSums(y)
  names(v) <- c("S", "I", "R")
  
  # also keep track of original sample 
  y <- colSums(y * 1:3)
  
  # loop over all the susceptibles
  dI <- vector()
  for (i in 1:v["S"]) {
    # each has I contacts, draw a q for each of these contacts
    q <- q_dist(v["I"]) 
    
    # flip q-probability coin to decide if contact leads to infection
    k <- rbinom(v["I"], 1, q)
    
    # if at least one contact leads to infection the susceptible is infected
    dI[i] <- I(sum(k) > 0)
  }
  
  # calculate total number of new infections caused by the gathering
  delta <- sum(dI)
  
  # return results
  return(list(
    "y" = y,
    "dI" = as.numeric(dI),
    "delta" = delta
  ))
}

# Example - gathering of size 10 from population in which 90% are susceptible,
# 5% are infected, and 5% are recovered
dgp(10, 0.9, 0.05)

# Example - different prior distribution for q
dgp(10, 0.9, 0.05, function (x) rbeta(x, 0.5, 0.5))


# TODO: run this a bunch of times under different values of N, ps, pi and
# different distributions of q and use results to calculate expected number of
# secondary cases, variance/spread of number of secondary cases created, etc.
