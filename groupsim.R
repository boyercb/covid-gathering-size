# =============================================================================
# Import and define key parameters
# =============================================================================

# Import packages
library(tidyverse) 

# Define number of sims to run per parameter set 
reps <- 500 

# Define parameter values/ranges: 
R0rng <- 2.5       # Basic reproduction number
nuscalerng <- 1    # Overdispersion for nu
dinfrng <- 5*24    # Duration of infectiousness (hrs)
dgrouprng <- 2     # Duration of gathering (hrs)
groupsizerng <- seq(2,100,2) # Group size 
popSrng <- 90      # S prevalence in the population (unnormalized ok)
popIrng <- 5       # I prevalence in the population (unnormalized ok)
popRrng <- 5       # R prevalence in the population (unnormalized ok)

# Collect parameter values into a data frame: 
parmsdf <- expand.grid(
	R0=R0rng,
	nuscale=nuscalerng,
	dinf=dinfrng,
	dgroup=dgrouprng,
	groupsize=groupsizerng,
	popS=popSrng,
	popI=popIrng,
	popR=popRrng)

# =============================================================================
# Define functions to simulate transmission
# =============================================================================

# Define function to generate the number of new infections generated at a gathering:

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

# Define a function to repeatedly calculate the number of new infections generated at a gathering for the same parameter set: 

getnewinfvec <- function(parms,reps){
	outvec <- rep(NA, reps) # Initialize an output vector
	for(indexA in 1:reps){
		outvec[indexA] <- getnewinf(parms) # Simulate new infections
	}
	return(outvec)
}


# =============================================================================
# Run simulation
# =============================================================================

# Simulate the number of new infections for each parameter set: 

ninfdf <- parmsdf %>%
	mutate(parmset=1:n()) %>% # Assign an index to each parameter set (row)
	split(.$parmset) %>% # Split each parameter set (row) into its own list
	map(~ cbind( 
		map_dfr(seq_len(reps), function(x).),
		data.frame(ninf=getnewinfvec(as.vector(.), reps))
	)) %>% # Repeat the parameter set 'reps' times and bind the simulated infections as a new column 
	map(~ mutate(., rep=1:n())) %>% # Assign an index to each simulation
	bind_rows() # Bind everything back into a single data frarme 

# =============================================================================
# Visualize output
# =============================================================================

# Visualize the output (mean and 95% prediction interval): 
figninfdf <- ninfdf %>% 
	group_by(groupsize) %>%
	summarise(ninf_mean=mean(ninf), ninf_lwr=quantile(ninf,0.025), ninf_upr=quantile(ninf,0.975)) %>%
	ggplot(aes(x=groupsize, y=ninf_mean)) + 
		geom_errorbar(aes(ymin=ninf_lwr, ymax=ninf_upr), width=0.4, alpha=0.3) + 
		geom_point() + 
		geom_line(stat="smooth", method="loess", span=1) + 
		theme_minimal() 

