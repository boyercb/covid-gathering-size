# Load and modify the BBC pandemic data 
bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv")
bbc <- select(bbc, e_other)

# gathering sizes are contacts + 1 (index person)
bbc$M <- bbc$e_other + 1

# plotting raw BBC pandemic data
bbc_data <- ggplot(data = bbc, mapping = aes(x = M)) +
  geom_point(stat = "count") +
  scale_y_log10() + 
  scale_x_log10()


# option 1: IPW -----------------------------------------------------------

# Testing the IP weighting
popsize <- 52078525        # From www.ons.gov.uk mid-2017 over 18yo population in the UK.
popsize <- 33              # works with any number !!
bbc$wt <- popsize / bbc$M  # weights
total_wt <- sum(bbc$wt)    # sum of all weights


# estimate density for each value of gathering size
dist_ipw <- bbc %>%
  group_by(M) %>%
  summarise(prob = sum(wt) / total_wt)

# plotting it
bbc_data_ipw <- 
  ggplot(data = dist_ipw, mapping = aes(x = M, y = prob)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()


# option 2: divide by M ---------------------------------------------------


# Testing the simpler approach of dividing gathering proba by gathering size
# It seems to me that the output is the same as what we get with the IPW
dist_m <- bbc %>% count(M)
dist_m$M_wt <- dist_m$n / dist_m$M
dist_m$prob <- dist_m$M_wt / sum(dist_m$M_wt)

bbc_data_m <- 
  ggplot(data = dist_m, mapping = aes(x = M, y = prob)) + 
  geom_point() +
  scale_y_log10() + 
  scale_x_log10()

pdf("3_results/bbc_data.pdf", width = 5, height = 6)
print(bbc_data_m)
dev.off()  
