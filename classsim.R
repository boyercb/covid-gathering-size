library(tidyverse)

# =============================================================================
# Group size 2 
# =============================================================================

# Group types:
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

priorprob*colSums(transmat)

# =============================================================================
# Group size 3 
# =============================================================================

# Now keep track of number of infections, rather than group type:

# Possibilities:
# 0
# 1
# 2
# 3

p <- 0.1 # Prior prevalence of infectiousness:
q <- 0.2 # Probability of transmission

prng <- seq(0.1, 0.9, 0.1)

outcomevec0 <- rep(NA,length(prng))
outcomevec1 <- rep(NA,length(prng))
outcomevec2 <- rep(NA,length(prng))
outcomevec3 <- rep(NA,length(prng))

for(indexA in 1:length(prng)){
	p <- prng[indexA]
priorprob <- c(
	(1-p)^3,
	3*p*(1-p)^2,
	3*p^2*(1-p),
	p^3
	)

transmat <- t(matrix(c(
	c(1,0,0,0),
	c(0,(1-q)^2,2*q*(1-q),q^2),
	c(0,0,(1-q)^2,1-(1-q)^2),
	c(0,0,0,1)
),nrow=4))


outcome <- priorprob*colSums(transmat)
outcomevec0[indexA] <- outcome[1]
outcomevec1[indexA] <- outcome[2]
outcomevec2[indexA] <- outcome[3]
outcomevec3[indexA] <- outcome[4]
}

df <- data.frame(p=prng, out0=outcomevec0,out1=outcomevec1,out2=outcomevec2,out3=outcomevec3)

fig3 <- df %>%
	pivot_longer(cols=c("out0","out1","out2","out3"), names_to="nout", values_to="values") %>%
	ggplot(aes(x=p, y=values, col=nout))+ 
		geom_point() + 
		geom_line()

ggsave(df, file="~/Desktop/")









