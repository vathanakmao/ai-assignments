source("GridCell.R")
source("Agent.R")
 
print_grid = function(mygrid, max.col) {
  row = ""
  for (i in 1:length(mygrid)) {
    # tmp <- c("(id=", mygrid[[i]]$id, ",self=", mygrid[[i]]$self ,",l=", mygrid[[i]]$left, ",r=", mygrid[[i]]$right, ",u=", mygrid[[i]]$up, ",d=", mygrid[[i]]$down, ")")
    # tmp <- c("(id=", mygrid[[i]]$id, ",l=", mygrid[[i]]$left, ",r=", mygrid[[i]]$right, ",u=", mygrid[[i]]$up, ",d=", mygrid[[i]]$down, ")")
    tmp <- c("(l=", mygrid[[i]]$left, ",r=", mygrid[[i]]$right, ",u=", mygrid[[i]]$up, ",d=", mygrid[[i]]$down, ")")
    cell <- paste(tmp, collapse='')
    # print(cell2)
    row <- paste(row, cell, sep=", ")
    if (i %% max.col == 0) {
      print(row)
      row = ""
    }
  }
}

generate_r_table = function(max.row, max.col) {
  r.table <- c()
  total.cell = max.row * max.col
  for (i in 1:total.cell) {
    gc <- GridCell$new()
    gc$id = i
    gc$up = 0
    gc$down = 0
    gc$left = 0
    gc$right = 0
    gc$self = 0
    r.table <- c(r.table, gc)
    
    # if top row, no path up
    if (i >= 1 && i <= max.col) {
      gc$up = -1
    }
    
    # if the most left cells, no path to left
    if (i == 1 || (i-1) %% max.col == 0) {
      gc$left = -1
    }
    
    # if the right most cells, no path to right
    if (i %% max.col == 0) {
      gc$right = -1
    }
    
    # if the bottom row, no path down
    if (i + max.col > total.cell) {
      gc$down = -1
    }
  }
  
  # r.table[[6]]
  
  return (r.table)
}


##====================================================================================


## train agent

## EXAMPLE 1
# init.id = 1
# goal.id = 25
# reward = 100
# col = 5
# row = 5
# r.table = generate_r_table(row, col)
# r.table[[20]]$down = reward
# r.table[[24]]$right = reward

## EXAMPLE 2
init.id = 1
goal.id = 15
reward = 100
col = 3
row = 5
r.table = generate_r_table(row, col)
r.table[[12]]$down = reward
r.table[[14]]$right = reward
r.table[[15]]$self = reward

print("============== R Table ==============")
print_grid(r.table, col)

agent <- Agent$new()
# explored_cells = agent$learn(r.table, row, col, init.id, goal.id)

for (i in 1:50) {
  init.id = sample(1:15, 1)  
  print( paste( c("Init Cell ID: ", init.id, ", Goal Cell ID: ", goal.id), collapse = "") )
  
  explored_cells = agent$learn(r.table, row, col, init.id, goal.id)
  # closest_cell = explored_cells[[length(explored_cells)]]
  # if (closest_cell$id != 14 && closest_cell$id != 12) {
  #   stop()
  # }
}

max_weight = 0
# print("------------------------------------ length:")
# print(length(agent$q.table))
for (i in 1:length(agent$q.table)) {
  weight = 0
  # print(agent$q.table[[i]])
  if (weight < agent$q.table[[i]]$left) {
    weight = agent$q.table[[i]]$left
  }
  if (weight < agent$q.table[[i]]$up) {
    weight = agent$q.table[[i]]$up
  }
  if (weight < agent$q.table[[i]]$right) {
    weight = agent$q.table[[i]]$right
  }
  if (weight < agent$q.table[[i]]$down) {
    weight = agent$q.table[[i]]$down
  }
  # print(weight)
  if (max_weight < weight) {
    max_weight = weight
  }
}
 print(max_weight)

for (i in length(agent$q.table)) {
  if (agent$q.table[[i]]$left != 0 && agent$q.table[[i]]$left != -1) {
    agent$q.table[[i]]$left = agent$q.table[[i]]$left * 100 / max_weight
  }
  if (agent$q.table[[i]]$up != 0 && agent$q.table[[i]]$up != -1) {
    agent$q.table[[i]]$up = agent$q.table[[i]]$up * 100 / max_weight
  }
  if (agent$q.table[[i]]$right != 0 && agent$q.table[[i]]$right != -1) {
    agent$q.table[[i]]$right = agent$q.table[[i]]$right * 100 / max_weight
  }
  if (agent$q.table[[i]]$down != 0 && agent$q.table[[i]]$down != -1) {
    agent$q.table[[i]]$down = agent$q.table[[i]]$down * 100 / max_weight
  }
}
print("------------------------ After Normalization ------------------------")
print_grid(agent$q.table, col) ## NOT WORK






