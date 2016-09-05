source('dfs.R')
source('PriorityQueue.R')

require(igraph)

cities <- data.frame(name = c("Kampot", "Preyveng", "Takeo", "Phnom Penh", "Kampongcham", "Kampongchnang", "Posat", "Battambang","Siem Reap", "Banteay Meanchey", "Kampongspeu", "Krojes") )

maps <-data.frame(
  from = c( "Takeo",   "Phnom Penh",  "Phnom Penh",  "Phnom Penh",   "Phnom Penh",     "Phnom Penh",   "Kampongchnang",  "Kampongchnang",  "Posat",          "Banteay Meanchey",   "Kampongchnang", "Posat",     "Battambang"),
  to = c(   "Kampot",  "Preyveng",    "Takeo",       "Kampongcham",  "Kampongchnang",  "Battambang" ,  "Posat",          "Kampongspeu",     "Battambang",    "Siem Reap",          "Krojes",      "Siem Reap", "Siem Reap")
)

g <- graph.data.frame(maps, directed=FALSE, vertices=cities)
E(g)["Phnom Penh" %->% "Battambang"]$weight= 200
E(g)["Phnom Penh" %->% "Kampongcham"]$weight= 120
E(g)["Phnom Penh" %->% "Preyveng"]$weight= 180
E(g)["Phnom Penh" %->% "Kampongchnang"]$weight= 90
E(g)["Phnom Penh" %->% "Takeo"]$weight= 60
E(g)["Takeo" %->% "Kampot"]$weight= 300
E(g)["Kampongchnang" %->% "Krojes"]$weight= 150
E(g)["Kampongchnang" %->% "Kampongspeu"]$weight= 90
E(g)["Battambang" %->% "Posat"]$weight= 50
E(g)["Kampongchnang" %->% "Posat"]$weight= 70
E(g)["Posat" %->% "Siem Reap"]$weight= 140
E(g)["Battambang" %->% "Siem Reap"]$weight= 160
E(g)["Siem Reap" %->% "Banteay Meanchey"]$weight= 80
E(g)$label= E(g)$weight

plot.igraph(g)

dfs(g, "Phnom Penh", "Siem Reap")


