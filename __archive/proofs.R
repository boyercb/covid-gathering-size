library(tidyverse)


# find correct gamma-poisson parameterization in R ------------------------

N <- 10000000

# simulate negative bionomial from Hong Kong paper 
y_nb <- rnbinom(N, 0.16, mu = 2.5)

# simulate equivalent gamma-poisson representation
mu <- rgamma(N, 0.16, 0.16 / 2.5)
y_gamma <- rpois(N, mu)

# collect in data.frame to plot
df <- data.frame(
  y_gamma = y_gamma,
  y_nb = y_nb
)

# plot overlapping distributions to verify they are equivalent
ggplot(pivot_longer(df, everything()), aes(x = value, fill = name)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 1) + 
  coord_cartesian(expand = F) 


# sum of gammas proof -----------------------------------------------------


N <- 10000000

# sum of 2 gammas
y_gamma_sum <- rgamma(N, 0.45, 0.45 / 0.58) + rgamma(N, 0.45, 0.45 / 0.58)

# gamma with 2 times shape parameter
y_gamma <- rgamma(N, 2 * 0.45, (2 * 0.45) / 0.58)

# collect in data.frame to plot
df <- data.frame(
  y_gamma = y_gamma,
  y_gamma_sum = y_gamma_sum
)

# plot overlapping distributions to verify they are equivalent
ggplot(pivot_longer(df, everything()), aes(x = value, fill = name)) +
  geom_histogram(position = "identity", alpha = 0.3, bins = 100) + 
  coord_cartesian(expand = F) 
