nu <- rgamma(10000, 0.1, 0.1 / 2.5)
x <- rpois(10000, nu)
x <- ifelse(x > 10, 10, x)
y <- rnbinom(10000, 0.1, mu = 2.5)
y <- ifelse(y > 10, 10, y)

z <- rpois(10000, 2.5)
z <- ifelse(z > 10, 10, z)

mean(x)
mean(y)
mean(z)


df <- 
  expand.grid(
  N = 2:100
)

df$r0 <- sapply(df$N, function(x) {
  nu <- rgamma(100000, 0.1, 0.1 / 2.5)
  y <- rpois(100000, nu)
  y <- ifelse(y > x, x, y)
  return(mean(y))
})

df$r0_log <- sapply(df$N, function(x) {
  nu <- rlnorm(100000, log(2.5^2/(sqrt(2.5^2 + 2.5^2/0.1))), sqrt(log(1 + 1/0.1)))
  y <- rpois(100000, nu)
  y <- ifelse(y > x, x, y)
  return(mean(y))
})

df$r0_d <- sapply(df$N, function(x) {
  nu <- rgamma(100000, 0.01, 0.01 / 2.5)
  y <- rpois(100000, nu)
  y <- ifelse(y > x, x, y)
  return(mean(y))
})

df$r0_log_d <- sapply(df$N, function(x) {
  nu <- rlnorm(100000, log(2.5^2/(sqrt(2.5^2 + 2.5^2/0.01))), sqrt(log(1 + 1/0.01)))
  y <- rpois(100000, nu)
  y <- ifelse(y > x, x, y)
  return(mean(y))
})

df$r0_i <- sapply(df$N, function(x) {
  nu <- rgamma(100000, 0.1, 0.1 / 2.5)
  y <- rpois(100000, nu)
  y <- ifelse(y > 0.7 * x, 0.7 * x, y)
  return(mean(y))
})

df$r0_2p5 <- sapply(df$N, function(x) {
  nu <- rgamma(100000, 0.1, 0.1 / 2.5)
  y <- rpois(100000, nu)
  y <- ifelse(y > x, 2.5, y)
  return(mean(y))
})

ggplot(pivot_longer(df, -N), aes(x = N, y = value, color = name)) +
  geom_line()


