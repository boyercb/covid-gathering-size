# =============================================================================
# Group size 2 
# =============================================================================

# 2 people
# 0 for uninfected, 1 for infected
# 4 possibilities

from <- list(c(0,0),c(0,1),c(1,0),c(1,1))
to <- list(list(c(0,0)), list(c(0,1),c(1,1)), list(c(1,1),c(1,0)), list(c(1,1))) 

# group types 
# 00
# 01
# 10
# 11

# Prior prevalence of infectiousness:
p <- 0.1 
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

