source("Stack.R")

dfs = function(g, init, goal) {
  'Depth First Search'
  
  prev <- init
  explored <- c()
  goalFound <- FALSE
  solution <- new.env(hash=T, parent=emptyenv())

  frontier <- Stack$new()
  frontier$push(prev)
  
  while (!frontier$isEmpty() && !goalFound) {
    
    'get the next node to be expanded'
    prev <- frontier$pop()
    explored[length(explored)+1] <- prev

    'get all children of prev node and sort in reversed alphabetical order'
    '(so that when pop from the stack, the child nodes will be ordered alphabetically)'
    child <- V(g)[neighbors(g,prev)]$name
    # child <- sort(child, decreasing=TRUE)

    for (i in 1:length(child)) {
      each_child <- child[i]
      'add the newly discovered node if it is not already explored and not already generated'
      if (!(each_child %in% explored | each_child %in% frontier$data)) {
        frontier$push(child[i])
        'hashmap the parent of the child to prev (for backtrack to generate a solution path'
        assign(each_child, prev, solution)
      }
      'goal test: stop the search when the goal is generated'
      goalFound = each_child==goal
      if (goalFound) break
    }
    'debugging'
    print("EXPLORED")
    print(explored)
    print("FRONTIER")
    print(frontier$data)
  }
 
  'Generate a solution path'
  'answer[[1]]=number of steps from initial to goal'
  'answer[[2]]=a solution path'
  'answer[[3]]=the list of nodes in the order they are explored'
  'Special case: initial and goal are the same node'
  if (init==goal) {
    answer <- list()
    answer[[1]] <- 0
    answer[[2]] <- init
    answer[[3]] <- explored
    return(answer)
  }

  'Special case: the goal node is not reachable from the initial node'
  if (!exists(goal,solution)) stop("queue is empty!")

  'backtrack from the goal to the start following child->to->parent links'
  answer <- list()
  path <- c(goal)
  p <- goal
  repeat {
    p <- get(p,solution)
    path <- c(p,path)
    if (p==init) break
  }
  answer[[1]] <- length(path)-1
  answer[[2]] <- path
  answer[[3]] <- explored
  return(answer)

}