source("GridCell.R")
source("Agent.R")
 
print_grid = function(mygrid, max.col) {
  row = ""
  for (i in 1:length(mygrid)) {
    tmp <- c("(id=", mygrid[[i]]$id, ",self=", mygrid[[i]]$self ,",l=", mygrid[[i]]$left, ",r=", mygrid[[i]]$right, ",u=", mygrid[[i]]$up, ",d=", mygrid[[i]]$down, ")")
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
# r.table[[25]]$self = reward

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

agent <- Agent$new(q.table=list(-1))
for (i in 1:10) {
  agent$learn(r.table, row, col, init.id, goal.id)  
}

print("============== Q Table ==============")
print_grid(agent$q.table, col)





