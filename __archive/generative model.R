generative_model <- function(N) {
  k <- sample(1:50, N, replace = TRUE)
  q <- rbeta(N, 8, 92)
  
  e <- rnorm(sum(k), 0, 0.05)
  
  q <- rep(q, times = k) + e
  
  q <- ifelse(q > 1, 1, ifelse(q < 0, 0, q))
  
  y <- rbinom(sum(k), 1, q)
  
  y <- tapply(y, rep(1:N, times = k), sum)
  
  data.frame(
    k = k,
    y = y
  )
}

df <- generative_model(100)

rstan