source("PriorityQueue.R")

ucs = function(g, init, goal) {
  'Depth First Search'
  
  prev <- Vertex$new()
  prev$name <- init
  prev$totalpathcost <- 0
  
  explored <- list()
  goalFound <- FALSE
  solution <- new.env(hash=T, parent=emptyenv())
  
  frontier <- PriorityQueue$new()
  frontier$enqueue(prev)
  
  while (!frontier$isEmpty() && !goalFound) {
    
    'get the next node to be expanded'
    prev <- frontier$dequeue()
    explored[length(explored)+1] <- prev
    
    print(prev$name)
    
    'get all children of prev node and sort in reversed alphabetical order'
    '(so that when pop from the stack, the child nodes will be ordered alphabetically)'
    child <- V(g)[neighbors(g,prev$name)]$name
    
    ## (bubble) sort the items by path cost
    j = length(child) - 1
    i = 1
    while (i < j) {
      for (r in i:(j-1)) {
        totalpathcost1 = E(g,path=c(prev$name,child[r]))$weight + prev$totalpathcost
        totalpathcost2 = E(g,path=c(prev$name,child[r+1]))$weight + prev$totalpathcost
        
        if (totalpathcost1 > totalpathcost2) {
          tmp = child[r]
          child[r] = child[r+1]
          child[r+1] = tmp
        } 
      }
      j = j - 1
    }
    
    for (i in 1:length(child)) {
      each_child <- child[i]
      
      'add the newly discovered node if it is not already explored and not already generated'
      # if (!(each_child %in% explored | each_child %in% frontier$data)) {
      if ( !(inExplored(each_child, explored) | inFrontiers(each_child, frontier$data)) ) {
        vertex <- Vertex$new()
        vertex$name <- each_child
        vertex$totalpathcost <- prev$totalpathcost + E(g,path=c(prev$name,each_child))$weight 
        frontier$enqueue(vertex)
        
        'hashmap the parent of the child to prev (for backtrack to generate a solution path'
        assign(each_child, prev$name, solution)
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
    p <- get(p,solution)  # get parent vertex
    path <- c(p,path)
    if (p==init) break
  }
  answer[[1]] <- length(path)-1
  answer[[2]] <- path
  answer[[3]] <- explored
  return(answer)
}

inExplored = function(name, vertices) {
  i = 1
  while (i <= length(vertices)) {
    if (vertices[[i]]$name == name) {
      return (T)
    }
    i = i + 1
  }
  return (F)
}

inFrontiers = function(name, data) {
  i = 1
  while (i <= length(data)) {
    if (data[[i]]$name == name) {
      return (T)
    }
    i = i + 1
  }
  return (F)
}

Vertex <- setRefClass(
  "Vertex",
  
  fields = c("name", "totalpathcost")
)
