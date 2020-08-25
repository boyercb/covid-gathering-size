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