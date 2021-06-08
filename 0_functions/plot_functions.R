gatherings_theme <-
  function() {
    theme_bw() +
      theme(
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino")
      ) 
  }


# our_colours 
c("#fdbf6f", "#a6cee3", "#fb9a99") # gather, home, work


