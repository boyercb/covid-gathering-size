# 2 people
# 0 for uninfected, 1 for infected
# 4 possibilities

from <- list(c(0,0),c(0,1),c(1,0),c(1,1))
to <- list(list(c(0,0)), list(c(0,1),c(1,1)), list(c(1,1),c(1,0)), list(c(1,1))) 

# 00
# 01
# 10
# 11

# Prior prevalence of infectiousness:
prev <- 0.1 
q <- 0.2 # Probability of transmission


# Initialize a probability transition matrix
transmat <- matrix(rep(0,16), nrow=4)


t(matrix(c(
c(1,0,0,0),
c(0,1-q,0,q),
c(0,0,1-q,q),
c(0,0,0,1)
),ncol=4)

