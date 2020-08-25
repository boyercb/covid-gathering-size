g <- sample_pa(30, power = 2, m = 10, directed = F)
plot(g,
     layout = layout.fruchterman.reingold,
     vertex.size = 6,
     vertex.label = NA)

g2 <- sample_pa(100, power = 0.25, m = 25, directed = F)
plot(g2,
     layout = layout.fruchterman.reingold,
     vertex.size = 6,
     vertex.label = NA)

degs <- sample(1:100, 100, replace=TRUE, prob=(1:100)^-1)
if (sum(degs) %% 2 != 0) { degs[1] <- degs[1] + 1 }
g5 <- sample_degseq(degs, method="vl")

plot(g5,
     layout = layout.fruchterman.reingold,
     vertex.size = 6,
     vertex.label = NA)

hist(rt(10000, 0.1, 0.1/30))
