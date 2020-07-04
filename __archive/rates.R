library(deSolve)
library(tidyverse)


# SIR model definition ----------------------------------------------------

model <- function(t, states, params) {
  with(as.list(c(states, params)), {
    dS <- - (b * k * I * S) / (S + I + R + D)
    dI <- (b * k * I * S) / (S + I + R + D) - gamma * I - mu * I
    dR <- gamma * I
    dD <- mu * I
    
    list(c(dS, dI, dR, dD))
  })
}


# Parameter definitions ---------------------------------------------------

popsize <- 1000000

params <- c(
  b = 0.175,
  gamma = 1/14,
  k = 1,
  mu = 1/24
)

states <- c(
  S = popsize - 1,
  I = 1,
  R = 0,
  D = 0
)

times <- seq(0, 600, 1)
slices <- c(101, 151, 176, 201, 251)


# Solve ODEs --------------------------------------------------------------

out <- lsoda(states, times, model, params)
out <- as.data.frame(out)

ps <- out$S[slices] / popsize 
pi <- out$I[slices] / popsize
no <- seq(1, length(ps))
df <- 
  data.frame(
    time = slices,
    no = no,
    pi = paste0("p[i] == ", round(pi, 3)),
    ps = paste0("p[s] == ", round(ps, 3))
  )


# Plot SIR results --------------------------------------------------------

#pdf("SIR.pdf", height = 7.5, width = 13.33)
out %>% 
  gather(key, value, -time) %>% 
  ggplot(., aes(x = time, y = value/popsize, color = fct_relevel(key, c("S", "I", "R")))) + 
  geom_vline(xintercept = slices, linetype = "dashed", alpha = 0.4) +
  geom_line() +
  geom_text(
    aes(x = time + 5, y = 0.95, label = no),
    data = df, 
    color = "black", 
    parse = T, 
    hjust = "left"
  ) +
  scale_color_discrete(name = "") +
  theme_classic() +
  labs(
    x = "days",
    y = "proportion of population"
  ) +
  coord_cartesian(expand = F, clip = "off")
#dev.off()
