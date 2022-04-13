# load the BBC pandemic data ----------------------------------------------

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")

k_home <- k_bbc %>%
  rename(M = e_home) %>%
  count(M, name = "N") %>%
  mutate(
    M_plus_1 = M + 1,
    prob = N / sum(N)
  )

k_work <- k_bbc %>%
  rename(M = e_work) %>%
  count(M, name = "N") %>%
  mutate(
    M_plus_1 = M + 1,
    prob = N / sum(N)
  )

k_gather <- k_bbc %>%
  rename(M = e_other) %>%
  count(M, name = "N") %>%
  mutate(
    M_plus_1 = M + 1,
    prob = N / sum(N)
  )

