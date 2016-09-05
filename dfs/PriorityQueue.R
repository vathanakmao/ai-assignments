PriorityQueue <- setRefClass(
  "PriorityQueue",
  
  fields = list(
    data = "list"
  ),
  
  methods = list (
    
    size = function(){
      'Returns the number of items'
      return(length(data))
    },
    
    enqueue = function(item, path.cost) {
      'Enqueue inserts element at the tail'
      
      l <- list()
      l <- append( l, list(c(item, path.cost)) )
      
      # sort items by path cost
      i = 1;
      j = length(data);
      
      # (bubble) sort the items by path cost
      while (i < j) {
        i = 1;
        for (r in i:(j-1)) {
          item.pathcost1 <- data[[r]]
          item.pathcost2 <- data[[r+1]]
          if (item.pathcost1[2] < item.pathcost2[2]) {
            data[[r]] <<- item.pathcost2
            data[[r+1]] <<- item.pathcost1
          }
        }
        j = j - 1
      }
      
      data <<- c(l, data)
    },
    
    dequeue = function() {
      'Dequeue removes and returns element from the head'
      if (length(data)==0) return (NULL)
      item <- data[[1]][1]
      data <<- data[-1]
      return(item)
    },
    
    isEmpty = function(){
      'Returns true if the queue is empty, false otherwise'
      return(size()==0)
    }
  )
)