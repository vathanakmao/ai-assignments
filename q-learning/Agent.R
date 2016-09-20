Agent <- setRefClass(
  "Agent",
  
  fields = list(
    q.table = "vector",
    gamma = "numeric"
  ),
  
  methods = list(
    
    ##
    # r.table - one dimensional array
    #
    learn = function(r.table, r.row, r.col, init.id, goal.id) {
      print("=================== START LEARNING ====================")
      
      if (length(q.table) == 0) {
        ## NOTE: q.table.row return numeric(0) instead of the real value so i use r.row instead
        initialize_q_table(r.row, r.col)
      }
      if (length(gamma) == 0) {
        gamma <<- 0.8
      }

      cur.cell = get_cell_by_id(r.table, init.id)
      goal.cell = get_cell_by_id(r.table, goal.id)
      # print(cur.cell)

      explored_cells = c()
      
      while (cur.cell$id != goal.id) {
        # print( paste( c("(id=", cur.cell$id, ",l=", cur.cell$left, ",r=", cur.cell$right, ",u=", cur.cell$up, ",d=", cur.cell$down, ")"), collapse="" ) )
        
        explored_cells = c(explored_cells, cur.cell)
        
        # str <- paste(c(", cur.cell$id=", cur.cell$id, ",   cur.cell$down=", cur.cell$down, ", cur.cell$right=", cur.cell$right), collapse="")
        # print(str)

        if (cur.cell$left <= -1 && cur.cell$up <= -1 && cur.cell$right <= -1 && cur.cell$down <= -1) {
          print("No path so quitting...")
          return (NULL)
        }

        # list all available actions (cells)
        if (cur.cell$left <= 0 && cur.cell$up <= 0 && cur.cell$right <= 0 && cur.cell$down <= 0) { # if there is a path to move and the weight is zero
          # 1=left, 2=up, 3=right, 4=down
          directions <- c()
          if (cur.cell$left == 0) {
            directions <- c(directions,1)
          }
          if (cur.cell$up == 0) {
            directions <- c(directions,2)
          }
          if (cur.cell$right == 0) {
            directions <- c(directions,3)
          }
          if (cur.cell$down == 0) {
            directions <- c(directions,4)
          }
          # print(directions)

          # randomly choose action
          direction = sample(directions, 1)
        } else { # otherwise, choose the action with the greatest weight
          direction = 1
          tmp.max = cur.cell$left
          if (tmp.max < cur.cell$up) {
            direction = 2
            tmp.max = cur.cell$up
          }
          if (tmp.max < cur.cell$right) {
            direction = 3
            tmp.max = cur.cell$right
          }
          if (tmp.max < cur.cell$down) {
            direction = 4
            tmp.max = cur.cell$down
          }
        }

        next.cell = get_next_cell_by_direction(cur.cell, direction, q.table, r.row, r.col) # next.cell is never null in this case
        
        # print("--------------------------------------------")
        # print(next.cell)
        
        ## max( Q(next state, all actions)
        max_weight = next.cell$left
        if (max_weight < next.cell$up) {
          max_weight = next.cell$up
        }
        if (max_weight < next.cell$right) {
          max_weight = next.cell$right
        }
        if (max_weight < next.cell$down) {
          max_weight = next.cell$down
        }
        if (max_weight < next.cell$self) {
          max_weight = next.cell$self
        }
        
        # Q(state, action) = R(state, action) + (gamma) * max( Q(next state, all actions) )
        # Q(B, F) = R(B,F) + gamma * max( Q(F,B), Q(F,E), Q(F,F) )
        cur.cell.weight = get_weight(cur.cell, direction)
        q_state_action = cur.cell.weight + round(gamma * max_weight)
        
        # if (cur.cell$id == 20 || cur.cell$id == 24) {
        #   str <- paste(c("q_state_action=", q_state_action, ", cur.cell.weight=", cur.cell.weight, ", max_weight=", max_weight, ", direction=", direction, ", cur.cell$down=", cur.cell$down, ", cur.cell$right=", cur.cell$right, ", cur.cell$id=", cur.cell$id), collapse="")
        #   print(str)
        # }
        
        set_weight(cur.cell, direction, q_state_action)
        # next.cell$self = q_state_action
        q.table[cur.cell$id] <<- cur.cell
        
        if (next.cell$id == goal.id) {
          goal.cell$self = q_state_action
          q.table[goal.id] <<- goal.cell
          break
        }
        
        ## move to next cell
        cur.cell = get_cell_by_id(r.table, next.cell$id)
      }
      
      # print(cur.cell$id)
      print("================== END LEARNING ===================")

      return (explored_cells)
    },

    get_next_cell_by_direction = function(cur.cell, direction, grid, row, col) {
      # print("====================== get_next_cell_by_direction():")
      # print(paste(c("cur.cell$id=", cur.cell$id, ", direction=", direction, ", col=", col, ", row=", row), collapse=""))
      
      if (direction == 1) { # if left
        if (cur.cell$left != -1) {
          return (grid[[cur.cell$id - 1]])
        }
      } else if (direction == 2) { # if up
        if (cur.cell$up != -1) {
          return (grid[[cur.cell$id - col]])
        }
      } else if (direction == 3) { # if right
        if (cur.cell$right != -1) {
          return (grid[[cur.cell$id + 1]])
        }
      } else if (direction == 4) { # if down
        if (cur.cell$down != -1) {
          return (grid[[cur.cell$id + col]])
        }
      }
      return (NULL)
    },

    initialize_q_table = function(row, col) {
      total.cell = row * col
      for (i in 1:total.cell) {
        gc <- GridCell$new()
        gc$id = i
        gc$up = 0
        gc$down = 0
        gc$left = 0
        gc$right = 0
        gc$self = 0
        q.table <<- c(q.table, gc)

        # if top row, no path up
        if (i >= 1 && i <= col) {
          gc$up = -1
        }

        # if the most left cells, no path to left
        if (i == 1 || (i-1) %% col == 0) {
          gc$left = -1
        }

        # if the right most cells, no path to right
        if (i %% col == 0) {
          gc$right = -1
        }

        # if the down row, no path down
        if (i + col > total.cell) {
          gc$down = -1
        }
      }

      # r.table[[6]]

      return (q.table)
    },

    set_weight = function (cell, direction, weight) {
      if (direction == 1) {
        cell$left = weight
      } else if (direction == 2) {
        cell$up = weight
      } else if (direction == 3) {
        cell$right = weight
      } else if (direction == 4) {
        cell$down = weight
      } else {
        print ("direction is invalid")
      }
    },

    get_weight = function (cell, direction) {
      if (direction == 1) {
        return (cell$left)
      } else if (direction == 2) {
        return (cell$up)
      } else if (direction == 3) {
        return (cell$right)
      } else if (direction == 4) {
        return (cell$down)
      } else {
        print ("direction is invalid")
        return (-1)
      }
    },

    get_cell_by_id = function(r.table, cell.id) {
      for (i in 1:length(r.table)) {
        if (r.table[[i]]$id == cell.id) {
          return (r.table[[i]])
        }
      }
      return (NULL)
    }
    
  )
)