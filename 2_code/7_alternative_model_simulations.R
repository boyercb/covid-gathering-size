# load the BBC pandemic data ----------------------------------------------

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")

k_home <- k_bbc %>%
  rename(M = e_home) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))

k_work <- k_bbc %>%
  rename(M = e_work) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))

k_gather <- k_bbc %>%
  rename(M = e_other) %>%
  count(M, name = "N") %>%
  mutate(prob = N / sum(N))
#  mutate(prob = N / (M + 1) / sum(N / (M + 1)))


# simulation 1: no restrictions -------------------------------------------

X <- offspring_model(
  N = 100000,
  k_work = k_work,
  k_gather = k_gather,
  k_home = k_home
)

R <- colMeans(X) 
names(R) <- c("R_work", "R_gather", "R_home")

R


# simulation 2: restrictions ----------------------------------------------

X <- offspring_model(
  N = 100000,
  k_work = k_work,
  k_gather = k_gather,
  k_home = k_home,
  c_gather = 5,
  option = "1"
)

R <- colMeans(X) 
names(R) <- c("R_work", "R_gather", "R_home")

R

X <- offspring_model(
  N = 100000,
  k_work = k_work,
  k_gather = k_gather,
  k_home = k_home,
  c_gather = 5,
  option = "2"
)

R <- colMeans(X) 
names(R) <- c("R_work", "R_gather", "R_home")

R

X <- offspring_model(
  N = 100000,
  k_work = k_work,
  k_gather = k_gather,
  k_home = k_home,
  c_gather = 5,
  option = "3"
)

R <- colMeans(X) 
names(R) <- c("R_work", "R_gather", "R_home")

R


# simulation 3: masks -----------------------------------------------------

X <- offspring_model(
  N = 100000,
  k_work = k_work,
  k_gather = k_gather,
  k_home = k_home,
  mu_gather = 0.04,
  c_gather = 5,
  option = "2"
)

R <- colMeans(X) 
names(R) <- c("R_work", "R_gather", "R_home")

R


# simulation 4: branching process -----------------------------------------

sims <- 
  map_dfr(1:10000, function(x) {
    N <- 1
    pop <- 50000L
    Nvec <- vector()
    
    pi <- 0
    pr <- 0
    i <- 1
    
    while(i <= 50) {
      N <-
        offspring_model(
          N = N,
          k_work = k_work,
          k_gather = k_gather,
          k_home = k_home,
          pi = pi,
          pr = pr,
          mu_gather = 0.04,
          c_gather = 5,
          option = "2"
        ) %>% colSums() %>% sum()
      Nvec[i] <- N
      pi <- N / pop
      if (i > 1) {
        pr <- sum(Nvec[1:(i-1)]) / pop
      }
      i <- i + 1
      #print(pi)
      #print(pr)
    }
    return(data.frame(
      sim = rep(x, 50),
      period = 1:50,
      infected = Nvec
    ))
  })

ggplot(sims, aes(x = period, y = infected)) +
  geom_hex(binwidth=c(1, 30)) +
  gatherings_theme() +
  scale_fill_distiller(trans = "log10", palette = "Spectral")


# simulation 5: more complex branching process ----------------------------

sim5 <- 
  map_dfr(1:1000, function(x) {
    
    N <- 100
    pop <- 100000L
    pi <- N / pop
    pr <- 0
    
    Nvec <- vector()
    Rtvec <- vector()

    i <- 1
    
    while(i <= 20) {
      if (i <= 5) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
          ) 
      } else {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
            c_gather = 5,
            c_work = 5,
            option = "2"
          )
      }
      N <- X %>% colSums() %>% sum()
      Rt <- X %>% colMeans() %>% sum()
      
      Nvec[i] <- N
      Rtvec[i] <- Rt
      pi <- N / pop
      if (i > 1) {
        pr <- sum(Nvec[1:(i-1)]) / pop
      }
      i <- i + 1
    }
    return(data.frame(
      sim = rep(x, 20),
      period = 1:20,
      infected = Nvec, 
      Rt = Rtvec
    ))
  })

ggplot(sim5, aes(x = period, y = Rt)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = 5, linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


ggplot(sim5, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = 5, linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


# simulation 6: intermitten interventions ---------------------------------

sim6 <- 
  map_dfr(1:1000, function(x) {
    
    N <- 20
    pop <- 100000L
    pi <- N / pop
    pr <- 0
    
    Nvec <- vector()
    Rtvec <- vector()
    
    i <- 1
    
    while(i <= 20) {
      if (i <= 5) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
          ) 
      } else if (i <= 10) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
            c_gather = 5,
            c_work = 5,
            option = "2"
          )
      } else if (i <= 15) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
            c_gather = 10,
            c_work = 10,
            option = "2"
          )
      } else if (i <= 20) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
            c_gather = 20,
            c_work = 20,
            option = "2"
          )
      }
      N <- X %>% colSums() %>% sum()
      Rt <- X %>% colMeans() %>% sum()
      
      Nvec[i] <- N
      Rtvec[i] <- Rt
      pi <- N / pop
      if (i > 1) {
        pr <- sum(Nvec[1:(i-1)]) / pop
      }
      i <- i + 1
    }
    return(data.frame(
      sim = rep(x, 20),
      period = 1:20,
      infected = Nvec, 
      Rt = Rtvec
    ))
  })

ggplot(sim6, aes(x = period, y = Rt)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


ggplot(sim6, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 



# simulation 7: prevalence targets ----------------------------------------

sim7 <- 
  map_dfr(1:1000, function(x) {
    
    N <- 20
    pop <- 100000L
    pi <- N / pop
    pr <- 0
    stop <- TRUE
    Nvec <- vector()
    Rtvec <- vector()
    
    i <- 1
    
    while(i <= 20) {
      if (pi < 0.01 & stop) {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            phi_gather = 0.1,
            pi = pi,
            pr = pr,
          ) 
      } else {
        X <-
          offspring_model(
            N = N,
            k_work = k_work,
            k_gather = k_gather,
            k_home = k_home,
            pi = pi,
            pr = pr,
            mu_gather = 0.04,
            phi_gather = 0.1,
            c_gather = 5,
            c_work = 5,
            option = "2",
            days = 3
          )
        stop <- FALSE
      } 
      N <- X %>% colSums() %>% sum()
      Rt <- X %>% colMeans() %>% sum()
      
      Nvec[i] <- N
      Rtvec[i] <- Rt
      pi <- N / pop
      if (i > 1) {
        pr <- sum(Nvec[1:(i-1)]) / pop
      }
      i <- i + 1
    }
    return(data.frame(
      sim = rep(x, 20),
      period = 1:20,
      infected = Nvec, 
      Rt = Rtvec
    ))
  })

ggplot(sim7, aes(x = period, y = Rt)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_hline(yintercept = 1, linetype = "dashed") +
 # geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 


ggplot(sim7, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
# geom_vline(xintercept = c(5, 10, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() 
