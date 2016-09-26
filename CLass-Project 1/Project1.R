rm(list = ls())

library(igraph)

data <- list.files("facebook")

setwd("facebook")
for(i in 1:length(data)) assign(data[i], read.csv(data[i]))

read.table(data[5])

graph <- graph.adjacency(data[0])
