# values of R_g, R_h, R_w -------------------------------------------------

lorenz_results %>%
  filter(selected == 1) %>%
  group_by(name) %>%
  summarise(
    value = mean(value)
  ) %>%
  mutate(
    letter = c("h", "g", "w")
  ) %>%
  ggplot(., aes(x = name, y = value, fill = name)) +
  geom_col() +
  geom_text(
    aes(label = paste0("R[", letter, "] == ", round(value, 2))),
    nudge_y = 0.05,
    family = "Palatino",
    parse = T
  ) +
  scale_fill_manual(values = c("#a6cee3", "#fdbf6f", "#fb9a99")) +
  gatherings_theme() +
  coord_cartesian(expand = F) +
  xlab("") +
  ylab("R value") +
  ylim(c(0, 1.5)) +
  theme(legend.position = "none")
