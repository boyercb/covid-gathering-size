# plots of contact distribution -------------------------------------------

# probability of 0 contacts across settings
k_home$prob[k_home$M == 0] # 11% 
k_work$prob[k_work$M == 0] # 39%
k_gather$prob[k_gather$M == 0] # 35%

bbc_plot <- ggplot() +
  geom_point(mapping = aes(x = M_plus_1, y = N, colour = "#a6cee3"),
             data = k_home, alpha = 0.75, size = 2) +
  geom_point(mapping = aes(x = M_plus_1, y = N, colour = "#fb9a99"),
             data = k_work, alpha = 0.75, size = 2) +
  geom_point(mapping = aes(x = M_plus_1, y = N, colour = "#fdbf6f"),
             data = k_gather, alpha = 0.75, size = 2) +
  scale_y_log10() + scale_x_continuous(
    trans = 'log10',
    breaks = c(1, 2, 11, 101),
    labels = c("1" = "0", "2" = "1",  "11" = "10", "101" = "100")
  ) +
#  labs(title = "Daily contacts (adults)") +
  ylab("Number of participants (log scale)") +
  xlab("Number of contacts (log scale)") +
  scale_color_identity(
    name = "Type of contact",
    breaks = c("#a6cee3", "#fb9a99", "#fdbf6f"),
    labels = c("Home", "Work or school", "Other gatherings"),
    guide = "legend"
  ) +
  gatherings_theme() +
  theme(legend.position = c(0.85, 0.85))

bbc_plot


pdf("3_results/bbc_plot.pdf", width = 7, height = 7)
print(bbc_plot)
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