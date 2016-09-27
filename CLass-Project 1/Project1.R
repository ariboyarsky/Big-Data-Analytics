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
g = graph.data.frame(graph.e, directed=TRUE, vertices=graph.n)

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

#find vertex attributes
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

#is the graph connected
is.connected(g2)

#get radius of graph
radius(g2)

#get closeness centrality of vertices
closeness(g2)

#find loop edges
count_multiple(g2)
#----------------------------------------------------------------------
#Question 5

#TODO: not running
#determine central person
alpha_centrality(g2)

#TODO: go from this to longest path
#longest path
distance_table(g2)

#TODO: lookover for accuracy
#largest clique
largest.cliques(g2)
#Hence, the largest click is 15?

#ego
ego(g2, gorder(g), V(g2))

# TODO: failing to run
# power centrality
power_centrality(g2, nodes = V(g2))



