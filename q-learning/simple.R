source('dfs.R')
source('Stack.R')


require(igraph)

cities <- data.frame(
  name = c("A","B","C","D","E","F","G"),
)

maps <-data.frame(
  from = c("A","A","A","B","C","D","G","G"),
  to = c("B","C","F","D","E","F","A","E"),
)

g <- graph.data.frame(maps, directed=FALSE, vertices=cities)

#plot.igraph(g)
