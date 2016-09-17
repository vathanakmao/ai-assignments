source("GridCell.R")
source("Agent.R")

print_grid = function(mygrid, max.col) {
  row = ""
  for (i in 1:length(mygrid)) {
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

generate_r_table = function() {
  r.table <- c()
  max.col = 5
  max.row = 5
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


## prepare R table
r.table = generate_r_table()

## train agent
init = GridCell$new()
init$id = 1
goal = GridCell$new()
goal$id = 25
agent <- Agent$new()
q.table = agent$learn(r.table, init, goal)

print_grid(r.table, max.col)





