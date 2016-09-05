source('dfs.R')
source('PriorityQueue.R')

require(igraph)

cities <- data.frame(name = c("A", "C", "D", "E", "Z", "G", "H") )

maps <-data.frame(
  from = c( "A", "A", "C", "D", "E", "E", "H", "Z"),
  to = c(   "C", "E", "D", "Z", "Z", "H", "G", "G")
)

g <- graph.data.frame(maps, directed=FALSE, vertices=cities)
E(g)["A" %->% "C"]$weight= 120
E(g)["A" %->% "E"]$weight= 140
E(g)["C" %->% "D"]$weight= 150
E(g)["D" %->% "Z"]$weight= 110
E(g)["E" %->% "Z"]$weight= 50
E(g)["E" %->% "H"]$weight= 150
E(g)["H" %->% "G"]$weight= 50
E(g)["Z" %->% "G"]$weight= 80
E(g)$label= E(g)$weight

plot.igraph(g)

path = dfs(g, "A", "G")

path
