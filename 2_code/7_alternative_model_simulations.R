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
  map_dfr(1:1000, function(x) {
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

ggplot(sims, aes(x = period, y = infected, group = sim)) +
  geom_line(alpha = 0.2)
