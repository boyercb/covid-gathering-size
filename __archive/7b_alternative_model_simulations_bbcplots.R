# load the BBC pandemic data ----------------------------------------------

k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18.csv") # Stephen adviced to use this one
#k_bbc <- read_csv("1_data/contact_dist_BBCPandemic/contact_distributions_o18_v1.csv") #Kucharski et al. don't use this one?
# /!\ Which version of dataset ? /!\ 
# /!\ Week or weekend or both ? 
# /!\ What kind of contacts ? 

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
  geom_point(mapping = aes(x = M, y = prob, colour = "#a6cee3"), data = k_home2) +
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
                       labels = c("Home", "Work or School", "Other gatherings"),
                       guide = "legend") + 
  gatherings_theme() + 
  theme(legend.position = c(0.85,0.85))

bbc_plot2

pdf("3_results/bbc_plot2.pdf", width = 5, height = 5)
print(bbc_plot2)
dev.off()


# Ploting the total number of atendees with gathering size -------------------------------------------
# I want to show the reason why restrictions with high c values are not effective : ------------------
# they do not affect many people !  ------------------------------------------------------------------
## Alternative idea : We could also plot the number of people that  ----------------------------------
## are preveted from going to a gathering with each gathering size ?? --------------------------------

k_gather$Minverse <- max(k_gather$M) - k_gather$M
k_gather$Ncumul <- cumsum(k_gather$N)
k_gather

k_home$Minverse <- max(k_home$M) - k_home$M
k_home$Ncumul <- cumsum(k_home$N)
k_home

k_work$Minverse <- max(k_work$M) - k_work$M
k_work$Ncumul <- cumsum(k_work$N)
k_work

ggplot(k_gather) + 
  geom_line(aes(x = M, y = Ncumul)) + 
  geom_abline(
    data = NULL,
    slope = ((max(k_gather$Ncumul) - min(k_gather$Ncumul)) / max(k_gather$M)),
    intercept = min(k_gather$Ncumul),
    linetype = 'dashed', 
    color = 'grey70'
  ) + 
  xlab("Size of gatherings") +
  ylab("Cumulative number of attendees\nout of population of 38117") + 
  gatherings_theme()


ggplot( ) +
  geom_line(data = k_gather, aes(x = M, y = Ncumul), colour = "#fdbf6f") +
  geom_line(data = k_home, aes(x = M, y = Ncumul), colour = "#a6cee3") +
  geom_line(data = k_work, aes(x = M, y = Ncumul), colour = "#fb9a99") +
  geom_abline(
    data = NULL,
    slope = ((max(k_gather$Ncumul) - min(k_gather$Ncumul)) / max(k_gather$M)),
    intercept = min(k_gather$Ncumul),
    linetype = 'dashed', 
    color = "#fdbf6f") + 
  geom_abline(
    data = NULL,
    slope = ((max(k_work$Ncumul) - min(k_work$Ncumul)) / max(k_work$M)),
    intercept = min(k_work$Ncumul),
    linetype = 'dashed', 
    color = "#fb9a99") + 
  geom_abline(
    data = NULL,
    slope = ((max(k_home$Ncumul) - min(k_home$Ncumul)) / max(k_home$M)),
    intercept = min(k_home$Ncumul),
    linetype = 'dashed', 
    color = "#a6cee3") + 
  xlab("Size of gatherings") +
  ylab("Cumulative number of attendees") + 
  gatherings_theme() + 
  #scale_color_identity(name = "Type of contact",
  #                    breaks = c("#a6cee3", "#fb9a99", "#fdbf6f"),
  #                    labels = c("Home", "Work or School", "Other gatherings"),
  #                    guide = "legend") +
  #theme(legend.position = c(1,1))
  scale_color_manual()

