source("PriorityQueue.R")

dfs = function(g, init, goal) {
  'Depth First Search'
  
  prev <- init
  explored <- c()
  goalFound <- FALSE
  solution <- new.env(hash=T, parent=emptyenv())
  
  frontier <- PriorityQueue$new()
  frontier$enqueue(prev, 0)
  
  while (!frontier$isEmpty() && !goalFound) {
    
    'get the next node to be expanded'
    prev <- frontier$dequeue()
    explored[length(explored)+1] <- prev

    'get all children of prev node and sort in reversed alphabetical order'
    '(so that when pop from the stack, the child nodes will be ordered alphabetically)'
    child <- V(g)[neighbors(g,prev)]$name
    
    print("------------------------------")
    print(child)
    
    # (bubble) sort the items by path cost
    j = length(child) - 1
    i = 1
    while (i < j) {
      for (r in i:(j-1)) {
        child1 = child[r]
        pathcost1 = E(g,path=c(prev,child[r]))$weight
        
        child2 = child[r+1]
        pathcost2 = E(g,path=c(prev,child[r+1]))$weight
        
        if (pathcost1 > pathcost2) {
          tmp = child[r]
          child[r] = child[r+1]
          child[r+1] = tmp
        } 
      }
      j = j - 1
    }
    
    print("------------------------------2")
    print(child)
    print("------------------------------3")

    for (i in 1:length(child)) {
      each_child <- child[i]
      
      'add the newly discovered node if it is not already explored and not already generated'
      if (!(each_child %in% explored | each_child %in% frontier$data)) {
        frontier$enqueue(child[i], 0+E(g,path=c(prev,child[i]))$weight)
        
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

sort_nodes_by_pathcost = function(nodes.pathcosts, asc=TRUE) {
  # (bubble) sort the items by path cost
  while (i < j) {
    i = 1;
    for (r in i:(j-1)) {
      item.pathcost1 <- nodes.pathcosts[[r]]
      item.pathcost2 <- nodes.pathcosts[[r+1]]
      if (asc && item.pathcost1[2] > item.pathcost2[2]) {
        nodes.pathcosts[[r]] <<- item.pathcost2
        nodes.pathcosts[[r+1]] <<- item.pathcost1    
      } else if (!asc && item.pathcost1[2] < item.pathcost2[2]) {
        nodes.pathcosts[[r]] <<- item.pathcost2
        nodes.pathcosts[[r+1]] <<- item.pathcost1
      }
    }
    j = j - 1
  }
  return (nodes.pathcosts)
}