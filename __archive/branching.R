N <- 100
tau <- 6

gathering <- matrix(rep(NA, tau * N), nrow = N, ncol = tau)
status <- matrix(rep(NA, tau * N), nrow = N, ncol = tau)

# assign infectiousness
gathering[gathering == 1, iter] <- rgamma(gathering[gathering == 1, iter], 0.45, 0.45/0.58)

# assign people to gatherings
shuffle <- sample(1:N, N).

# calculate q for each gathering based on attendees
q <- aggregate(shuffle[, iter], by = rep(1:(N/4), each = 4), sum)

# determine new infections

# pass on to next iteration

randomizr::conduct_ra
