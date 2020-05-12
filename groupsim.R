# =============================================================================
# Import and define key parameters
# =============================================================================

library(tidyverse) 

R0 <- 2.5 # Basic reproduction number
nuscale <- 1 # Overdispersion scale for individual infectiousness

dinf <- 5*24 # Duration of infectiousness in hours
dgroup <- 2 # Duration of gathering in hours

groupsize <- 16 # Group size 
popprev <- c(S=90, I=5, R=5) # S/I/R prevalence in population

# =============================================================================
# Calculate new infections
# =============================================================================

popprev <- popprev/sum(popprev) # Normalize population prevalence

# Draw infection states for the group from the population: 
groupchar <- t(rmultinom(1, groupsize, popprev)) %>% as.data.frame %>% unlist

# Draw nu values (individual R0) for each infectious person:
nuvec <- rgamma(groupchar["I"], shape=R0/nuscale, scale=nuscale)

# Calculate infection rates for each infectious person: 
betavec <- nuvec/dinf

# Calculate the probability that a susceptible person gets infected: 
pinf <- 1-exp(-sum(betavec)*dgroup)

# Calculate the number of new infections 
newinf <- rbinom(1, groupchar["S"], pinf)
