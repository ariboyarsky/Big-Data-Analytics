graph.edges <- read.csv(file = '0.edges',
                        sep=" ", header=FALSE,
                        col.names=c("source", "target"))
#get edges
graph.e <- graph.edges - min(graph.edges) + 1
#get nodes
graph.n <- data.frame(id=seq(max(graph.e) - min(graph.e) + 1))

graph.n$group <- 1

#import into igraph
g = graph.data.frame(graph.edges, directed=TRUE, vertices=graph.nodes)

#simplify code
g2 = simplify(g)

#plot
plot(g2)