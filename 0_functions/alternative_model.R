offspring_model <- function(N = 100000L,      # number of iterations
                            k_work,           # contact distribution for work
                            k_gather,         # contact distribution for gatherings
                            k_home,           # contact distribution for home
                            mu_work = 0.01,   # SAR at work
                            mu_gather = 0.08, # SAR at gatherings
                            mu_home = 0.36,   # SAR at home
                            phi_work = 10,    # SAR dispersion at work
                            phi_gather = 1,   # SAR dispersion at gatherings
                            phi_home = 10,    # SAR dispersion at home
                            c_work = NULL,    # restriction cut-off work
                            c_gather = NULL,  # restructione cut-off gatherings
                            option = "0",     # type of restriction ("0", "1", "2", or "3")
                            pi = 0,           # prevalence of infection
                            pr = 0,           # prevalence of immunity
                            days = 5          # infectious period
                            ) {
  
  # draw contacts 
  n_work <- draw_contacts(N, k_work, c_work, option, days)
  n_gather <- draw_contacts(N, k_gather, c_gather, option, days)
  n_home <- draw_contacts(N, k_home, NULL, "0", 1) # everyone sees same household members throughout infectious period
  
  # draw probability of infection given contact
  q_work <- rbeta(N, phi_work * mu_work, phi_work * (1 - mu_work))
  q_gather <- rbeta(N, phi_gather * mu_gather, phi_gather * (1 - mu_gather))
  q_home <- rbeta(N, phi_home * mu_home, phi_home * (1 - mu_home))
  
  # draw number of susceptible 
  S_work <- rbinom(N, n_work, 1 - pi - pr)
  S_gather <- rbinom(N, n_gather, 1 - pi - pr)
  S_home <- rbinom(N, n_home, 1 - pi - pr)
  
  # simulate infections
  X_work <- rbinom(N, S_work, q_work)
  X_gather <- rbinom(N, S_gather, q_gather)
  X_home <- rbinom(N, S_home, q_home)
  
  return(cbind(X_work, X_gather, X_home))
}

draw_contacts <- function(N, k, c = NULL, option = "0", days) {
  if (!is.null(c)) {
    # rebalance contact distribution based on cut-off (c)
    k <- switch(
      option,
      # option 0 : no restriction to gathering size 
      "0" = k,
      # option 1: set to c (most conservative)
      "1" = data.frame(
        M = k$M[k$M <= c],
        N = c(k$N[k$M < c], sum(k$N[k$M >= c])),
        prob = c(k$prob[k$M < c], sum(k$prob[k$M >= c]))
      ), 
      # option 2: set to 0 (least conservative)
      "2" = data.frame(
        M = k$M[k$M <= c],
        N = c(k$N[k$M == 0] + sum(k$N[k$M > c]), k$N[k$M > 0 & k$M <= c]),
        prob = c(k$prob[k$M == 0] + sum(k$prob[k$M > c]), k$prob[k$M > 0 & k$M <= c])
      ), 
      # option 3: redraw until less than c (realistic?) 
      "3" = data.frame(
        M = k$M[k$M <= c],
        N = k$N[k$M <= c],
        prob = k$N[k$M <= c] / sum(k$N[k$M <= c])
      )
    )
  }
  # draw contacts for each infectious individual for each day
  draws <- 
    matrix(
      data = sample(
        x = k$M,
        size = days * N,
        replace = TRUE,
        prob = k$prob
      ),
      ncol = days
    )
  
  # return total contacts over infectious period
  return(rowSums(draws))
}

