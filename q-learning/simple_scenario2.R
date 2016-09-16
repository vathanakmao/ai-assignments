source('dfs.R')
source('Stack.R')


require(igraph)

cities <- data.frame(name = c("Kampot", "Preyveng", "Takeo", "Phnom Penh", "Kampongcham", "Kampongchnang", "Posat", "Battambang","Siem Reap", "Banteay Meanchey", "Kampongspeu", "Krojes") )

maps <-data.frame(
  from = c( "Takeo",   "Phnom Penh",  "Phnom Penh",  "Phnom Penh",   "Phnom Penh",     "Phnom Penh",   "Kampongchnang",  "Kampongchnang",  "Posat",          "Banteay Meanchey",   "Kampongspeu", "Posat",     "Battambang"),
  to = c(   "Kampot",  "Preyveng",    "Takeo",       "Kampongcham",  "Kampongchnang",  "Battambang" ,  "Posat",          "Kampongspeu",     "Battambang",    "Siem Reap",          "Krojes",      "Siem Reap", "Siem Reap")
  )

g <- graph.data.frame(maps, directed=FALSE, vertices=cities)

road <- dfs(g, "Phnom Penh", "Siem Reap")

plot.igraph(g)

print(road)

