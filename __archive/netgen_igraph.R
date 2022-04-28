library(igraph) 

g <- barabasi.game(n=100,power=0,m=2,directed=FALSE)

plot(g, layout=layout_with_fr, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)