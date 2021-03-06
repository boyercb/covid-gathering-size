# load the BBC pandemic data ----------------------------------------------

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18_v1.csv")

# /!\ /!\ /!\ 
# /!\ /!\ /!\ Which one is the right dataset ???
# /!\ /!\ /!\ 

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


# Create plot 

max(k_home$M) ; max(k_work$M) ; max(k_gather$M)
k_home$prob[k_home$M == 0] # 11% 
k_work$prob[k_work$M == 0] # 39%
k_gather$prob[k_gather$M == 0] # 35%

k_home2 <- k_home ; k_home2$M <- k_home2$M +1
k_work2 <- k_work ; k_work2$M <- k_work2$M +1
k_gather2 <- k_gather ; k_gather2$M <- k_gather2$M +1

# ggplot(data = k_home, mapping = aes(x = M, y = prob)) + 
#   geom_point() + 
#   labs(title = "Households") + 
#   ylab("Probability") + 
#   xlab("Number of contacts") + 
#   gatherings_theme()

plot1 <- ggplot(data = k_home2, mapping = aes(x = M, y = prob)) + 
  geom_point() + scale_y_log10() +
  scale_x_continuous(trans = 'log10',
                     breaks = c(1, 4, 11, 31),
                     labels = c("1" = "0", "4" = "3", "11" = "10", "31" = "30")) + 
  labs(title = "Households") + 
  ylab("Probability (log scale)") + 
  xlab("Number of contacts (log scale)") + 
  gatherings_theme()

plot2 <- ggplot(data = k_work2, mapping = aes(x = M, y = prob)) + 
  geom_point() + scale_y_log10() +
  scale_x_continuous(trans = 'log10',
                     breaks = c(1, 11, 101),
                     labels = c("1" = "0", "11" = "10", "101" = "100")) + 
  labs(title = "Work or School") + 
  ylab("Probability (log scale)") + 
  xlab("Number of contacts (log scale)") + 
  gatherings_theme()

plot3 <- ggplot(data = k_gather2, mapping = aes(x = M, y = prob)) + 
  geom_point() + scale_y_log10() +
  scale_x_continuous(trans = 'log10',
                     breaks = c(1, 11, 101),
                     labels = c("1" = "0", "11" = "10", "101" = "100")) + 
  labs(title = "Social gatherings") + 
  ylab("Probability (log scale)") + 
  xlab("Number of contacts (log scale)") + 
  gatherings_theme()

grid.arrange(plot1, plot2, plot3, ncol=3, widths = c(4,6,6))

pdf("3_results/bbc_plot.pdf", width = 7.2, height = 4.5)
grid.arrange(plot1, plot2, plot3, ncol=3, widths = c(4,6,6))
dev.off()

bbc_plot2 <- ggplot() + 
  geom_point(mapping = aes(x = M, y = prob, colour = "#a6cee3" ), data = k_home2) +
  geom_point(mapping = aes(x = M, y = prob, colour = "#fb9a99"), data = k_work2) +
  geom_point(mapping = aes(x = M, y = prob, colour = "#fdbf6f"), data = k_gather2) +
  scale_y_log10() + scale_x_continuous(trans = 'log10',
                                       breaks = c(1, 11, 101),
                                       labels = c("1" = "0", "11" = "10", "101" = "100")) +
  labs(title = "Daily contacts (adults)") + 
  ylab("Probability (log scale)") +
  xlab("Number of contacts (log scale)") +
  scale_color_identity(name = "Type of contact",
                       breaks = c("#a6cee3", "#fb9a99", "#fdbf6f"),
                       labels = c("Home", "Work or School", "Other"),
                       guide = "legend") + 
  gatherings_theme()

bbc_plot2

pdf("3_results/bbc_plot2.pdf", width = 5, height = 5)
print(bbc_plot2)
dev.off()

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
  geom_ribbon(aes(ymin = 0, ymax = 1), fill = "grey70", alpha = 0.3) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = 5, linetype = 'dotted') +
 # geom_hline(yintercept = 1, linetype = "dashed") +
  coord_cartesian(expand = F) +
  ylab(bquote(R[t])) +
  gatherings_theme() 


ggplot(sim5, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = 5, linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")


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
      } else if (i <= 15) {
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
      } else {
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
  geom_ribbon(aes(ymin = 0, ymax = 1), fill = "grey70", alpha = 0.3) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#FC9272") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = c(5, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  ylab(bquote(R[t])) +
  gatherings_theme() 


ggplot(sim6, aes(x = period, y = infected)) +
  geom_line(aes(group = sim), alpha = 0.01, color = "#3182BD") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point") +
  geom_vline(xintercept = c(5, 15), linetype = 'dotted') +
  coord_cartesian(expand = F) +
  gatherings_theme() + 
  ylab("Incident cases")



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
  gatherings_theme() + 
  ylab("Incident cases")
