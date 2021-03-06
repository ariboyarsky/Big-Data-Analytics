#clean workspace
rm(list = ls())

#require igraph
library(igraph)

#read edges csv for nodeid 0
graph.edges <- read.csv(file = 'facebook/0.edges', sep=" ", header=FALSE, col.names=c("source", "target"))

#get edges
graph.e <- graph.edges - min(graph.edges) + 1
#get nodes
graph.n <- data.frame(id=seq(max(graph.e) - min(graph.e) + 1))

graph.n$group <- 1

#import into igraph
g = graph.data.frame(graph.e, directed=FALSE, vertices=graph.n)

#simplify code
g2 = simplify(g)

#--------------------------------------------------------------------------------
#Question 3. Explore with some functions from ppt
#plot
plot(g2)

#get verticies
V(g2)

#get edges
edges(g2)

#find vertex attributes, did not load attributes so we should not expect much here
vertex_attr(g2)

#check if the graph is simple (does not have loops or multiple edges between verticies)
is.simple(g2)


degree(g2, "198")

degree(g2)
astrocollab <- upgrade_graph(g)
plot(astrocollab)

#----------------------------------------------------------------------------
#Question 4 Explore 10 functions

#fidn eccentricity of verticies
eccentricity(g2)

#dfs with node 1 as root
dfs(g2, 1)

#bfs with node 1 as root
bfs(g2, 1)

#count triangles a vertex is a part of
count_triangles(g2)

#is the graph directed
is.directed(g2)

#is the graph weighted
is.weighted(g2)

# is this graph simple, prior to simplification
is.simple(g)

#is the graph connected
is.connected(g2)

#get closeness centrality of vertices
closeness(g2)

#find loop edges
count_multiple(g2)

#fine components of graph
components(g2)
#----------------------------------------------------------------------
#Question 5


#determine central person
ac1 = alpha_centrality(g)
tail(sort(ac1),5)

#TODO: check for accuracy
#longest path
which.max(distances(g2))

#TODO: lookover for accuracy
#largest clique
largest.cliques(g)
#Hence, the largest click is 15?

#ego
ego(g2, gorder(g), V(g2))


# power centrality
power_centrality(g, nodes = V(g))




