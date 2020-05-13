# =============================================================================
# Group size 2 
# =============================================================================

# group types 
# 00
# 01
# 10
# 11

p <- 0.1 # Prior prevalence of infectiousness:
q <- 0.2 # Probability of transmission

# Prior probability of each group type
priorprob <- c((1-p)^2, 2*p*(1-p), 2*p*(1-p), p^2)

# Initialize a probability transition matrix
transmat <- t(matrix(c(
c(1,0,0,0),
c(0,1-q,0,q),
c(0,0,1-q,q),
c(0,0,0,1)
),ncol=4))


# =============================================================================
# Group size 3 
# =============================================================================


# Now keep track of number of infections:

# 0
# 1
# 2
# 3

p <- 0.1 # Prior prevalence of infectiousness:
q <- 0.2 # Probability of transmission

priorprob <- c(
	(1-p)^3,
	3*p*(1-p)^2,
	3*p^2*(1-p),
	p^3
	)

transmat <- t(matrix(c(
	c(1,0,0,0),
	c(0,(1-q)^2,q*(1-q),q^2),
	c(0,0,(1-q)^2,1-(1-q)^2),
	c(0,0,0,1)
),nrow=4))











