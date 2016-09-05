source('dfs.R')
source('PriorityQueue.R')

require(igraph)

cities <- data.frame(name = c("A", "B", "C", "D", "E", "F", "G") )

maps <-data.frame(
  from = c( "A",  "A",  "A",  "A",   "B",  "D",   "G",  "C"),
  to = c(   "B",  "C",  "F",  "G",   "D",  "F" ,  "E",  "E")
)

g <- graph.data.frame(maps, directed=FALSE, vertices=cities)
E(g)["A" %->% "B"]$weight= 30
E(g)["A" %->% "C"]$weight= 50
E(g)["A" %->% "F"]$weight= 85
E(g)["A" %->% "G"]$weight= 27
E(g)["B" %->% "D"]$weight= 40
E(g)["D" %->% "F"]$weight= 40
E(g)["G" %->% "E"]$weight= 56
E(g)["C" %->% "E"]$weight= 50
E(g)$label= E(g)$weight

plot.igraph(g)

dfs(g, "A", "E")


