library(deSolve)
library(tidyverse)
 

# SIR model definition ----------------------------------------------------

model <- function(t, states, params) {
  with(as.list(c(states, params)), {
    dS <- - (b * k * I * S) / (S + I + R)
    dI <- (b * k * I * S) / (S + I + R) - gamma * I 
    dR <- gamma * I
    
    list(c(dS, dI, dR))
  })
}


# Parameter definitions ---------------------------------------------------

popsize <- 1000000

params <- c(
  b = 0.15,
  gamma = 1/14,
  k = 1
)

states <- c(
  S = popsize - 1,
  I = 1,
  R = 0
)

times <- seq(0, 400, 1)
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

pdf("SIR.pdf", height = 7.5, width = 13.33)
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
dev.off()


# Plot impact of gathering size at different epidemic points --------------

N <- seq(2, 20, 1)          # size of gatherings
q <- seq(0.1, 0.9, 0.01)    # probability of infection given contact

df <- expand.grid(N = N, p = paste(no, ps, pi), q = q)

df[, c("no", "ps", "pi")] <- as.numeric(str_split(df$p, " ", simplify = T))
df$dI <- with(df, N * ps * (1 - (1 - q)^(N * pi)))
df$lab <- paste0(df$no,"\n~p[i] == ", round(df$pi, 3), "~','~p[s] == ", round(df$ps, 3))
  
pdf("SIR_exact.pdf", height = 7.5, width = 13.33)
ggplot(df, aes(x = N, y = q, z = dI)) +
  facet_wrap( ~ lab, labeller = label_parsed, ) +
  geom_contour_filled(binwidth = 1) +
  xlab("size of gathering (N)") + 
  scale_fill_viridis_d(name = "expected\nnew cases", option = "plasma") +
  coord_cartesian(expand = F)
dev.off()