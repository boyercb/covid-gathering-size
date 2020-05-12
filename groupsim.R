# =============================================================================
# Import and define key parameters
# =============================================================================

library(tidyverse) 

reps <- 1000 # Number of simulations to run 

parms <- c(
	R0=2.5,       # Basic reproduction number
	nuscale=1,    # Overdispersion scale for individual infectiousness 
	dinf=5*24,    # Duration of infectiousness in hours
	dgroup=2,     # Duration of gathering in hours
	groupsize=16, # Group size 
	popS=90,      # Susceptible prevalence in the population (unnormalized ok)
	popI=5,       # Infectious prevalence in the population (unnormalized ok)
	popR=5        # Recovered prevalence in the population (unnormalized ok)
	)

# =============================================================================
# Calculate new infections
# =============================================================================

getnewinf <- function(parms){

	with(as.list(parms), {

		# Collect population prevalence into a vector 
		popprev <- c(S=popS, I=popI, R=popR)

		 # Normalize population prevalence
		popprev <- popprev/sum(popprev)

		# Draw infection states for the group from the population: 
		groupstates <- t(rmultinom(1, groupsize, popprev)) %>% as.data.frame %>% unlist

		# Draw nu values (individual R0) for each infectious person:
		nuvec <- rgamma(groupstates["I"], shape=R0/nuscale, scale=nuscale)

		# Calculate infection rates for each infectious person: 
		betavec <- nuvec/dinf

		# Calculate the probability that a susceptible person gets infected: 
		pinf <- 1-exp(-sum(betavec)*dgroup)

		# Calculate the number of new infections 
		newinf <- rbinom(1, groupstates["S"], pinf)

		return(newinf)

	})

}


newinfvec <- rep(NA,reps)
for(indexA in 1:reps){
	newinfvec[indexA] <- getnewinf(parms)

}


