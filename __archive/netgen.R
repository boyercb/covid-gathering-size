library(igraph) 

n <- 100
m <- 1
nugget <- 0.1

adj <- matrix(rep(0,n^2),nrow=n,ncol=n)

for(indexA in 2:n){

	pvec <- colSums(adj) + c(rep(nugget,indexA-1),0,rep(0,n-indexA))

	newconnex <- rowSums(rmultinom(m,1,pvec))

	newrow <- unlist(lapply(newconnex, function(x){min(x,1)}))

	adj[,indexA] <- newrow
	adj[indexA,] <- newrow

}

hist(rowSums(adj))