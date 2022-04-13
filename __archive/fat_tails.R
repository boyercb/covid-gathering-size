fat_tails_params <-
  expand.grid(
    N = 1e7,
    alpha = seq(1.2, 3, 0.2),
    x_min = 8,
    phi = c(0.1, 1, 10, 100),
    c = 50
  )

fat_tails <- 
  pmap_dfr(as.list(fat_tails_params), function(N, alpha, x_min, phi, c) {
    q <- rbeta(N, 0.08 * phi, (1 - 0.08) * phi)
    k_tail <- rpareto(N, x_min, alpha)
    k <- draw_contacts(N, k_gather, days = 1)
    k[k >= 8] <- floor(k_tail[k >= 8])

    X <- rbinom(N, k, q)
    
    k_restrict <- k
    k_restrict[k_restrict >= c] <- c
    
    X_restrict <- rbinom(N, k_restrict, q)
    
    return(tibble(
      N = N,
      alpha = alpha,
      x_min = x_min,
      phi = phi,
      R = mean(X),
      R_99 = quantile(X, 0.99),
      R_max = max(X),
      R_restrict = mean(X_restrict),
      R_restrict_99 = quantile(X_restrict, 0.99),
      R_restrict_max = max(X_restrict)
    ))
  }) 





# experiment: generate data from a power law distribution that is scale free and
# a homogeneous SAR and see what the Lorenz curve looks like; then vary phi and
# plot again

x_min <- 2
alpha <- 2
u <- runif(1000000)

h <- hist(x_min * (1 - u)^(-1 / (alpha - 1)), breaks=40, plot=F)
plot(h$count, log="xy", type='l',
     xlab="", ylab="", main="Density in logarithmic scale")

# distribution, cdf, quantile and random functions for Pareto distributions
dpareto <- function(x, xm, alpha) ifelse(x > xm , alpha*xm**alpha/(x**(alpha+1)), 0)
ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm/q)**alpha, 0 )
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm*(1-p)**(-1/alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)
pareto.mle <- function(x)
{
  xm <- min(x)
  alpha <- length(x)/(sum(log(x))-length(x)*log(xm))
  return( list(xm = xm, alpha = alpha))
}

pareto.test <- function(x, B = 1e3)
{
  a <- pareto.mle(x)
  
  # KS statistic
  D <- ks.test(x, function(q) ppareto(q, a$xm, a$alpha))$statistic
  
  # estimating p value with parametric bootstrap
  B <- 1e5
  n <- length(x)
  emp.D <- numeric(B)
  for(b in 1:B)
  {
    xx <- rpareto(n, a$xm, a$alpha);
    aa <- pareto.mle(xx)
    emp.D[b] <- ks.test(xx, function(q) ppareto(q, aa$xm, aa$alpha))$statistic
  }
  
  return(list(xm = a$xm, alpha = a$alpha, D = D, p = sum(emp.D > D)/B))
}

