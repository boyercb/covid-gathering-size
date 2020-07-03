# This function simulates a gathering of size N drawn from a source population
# in which the prevalence of susceptibility and infection are given by ps and pi
# respectively. The function also allows us to specify the distribution for nu.
dgp <- function(N,
                ps,
                pi,
                T = 1,
                D = 10,
                nu_dist = function(x)
                  rgamma(x, 0.45, 0.58)) {  # <-- This can be changed to whatever distribution we want!
  # draw random sample of attendees from population of
  # 1. susceptibles 
  # 2. infecteds
  # 3. recovereds
  y <- rmultinom(1, N, c(ps, pi, 1 - ps - pi))
  y <- as.vector(y)
  
  # rename to reflect compartments
  names(y) <- c("S", "I", "R")
  
  # draw infectiousness (nu) for each infected
  nu <- nu_dist(y["I"]) 
  
  # calculate probability of infection assuming homogeneous mixing
  q <- 1 - exp(-sum(nu) * (T/D))
  
  # flip S q-probability coins to determine number who get infected
  delta <- rbinom(1, y["S"], q)
  
  # return results
  return(list(
    "y" = y,
    "q" = q,
    "delta" = delta
  ))
}


library(ggplot2)

df <- data.frame(
  nu = rgamma(1000, 0.45, 0.58)
)

ggplot(df, aes(x = nu)) + 
  geom_histogram()

df <- 
expand.grid(
  N = seq(2, 100),
  pi = seq(0.001, 0.09, 0.001)
)

df$delta <-
  map2_dbl(df$N,
       df$pi,
       function(x, y) {
         replicate(1000, {
           sim <- dgp(x, 0.9, y)
           sim$delta
         }) %>% mean()
       })
  
ggplot(filter(df, N <= 20), aes(x = N, y = pi, z = delta)) +
  geom_contour_filled(binwidth = 1)
  

df <- 
  expand.grid(
    N = seq(2, 20),
    pi = seq(0.001, 0.09, 0.001),
    phi = seq(0.1, 0.5, 0.1),
    r0 = seq(0.5, 2.5, 0.5)
  )

pb <- txtProgressBar(max = nrow(df), initial = NA, style = 3)

df$delta <-
  pmap_dbl(
  list(
    N = df$N,
    pi = df$pi,
    phi = df$phi,
    r0 = df$r0
  ),
  function(N, pi, phi, r0) {
    i <- getTxtProgressBar(pb)
    setTxtProgressBar(pb, ifelse(is.na(i), 1, i + 1))
    replicate(1000, {
      sim <- dgp(N, 0.9, pi, nu_dist = function(x) rgamma(x, phi, r0))
      sim$delta
    }) %>% mean()
  })

ggplot(df, aes(x = N, y = pi, z = delta)) +
  facet_grid(r0 ~ phi) +
  geom_contour_filled(binwidth = 1)



