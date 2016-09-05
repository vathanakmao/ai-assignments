PriorityQueue <- setRefClass(
  "PriorityQueue",
  
  fields = list(
    data = "vector"
  ),
  
  methods = list (
    
    size = function(){
      'Returns the number of items'
      return(length(data))
    },
    
    enqueue = function(vertex) {
      'Enqueue inserts element at the tail'
      
      data <<- c(vertex, data)
      
      ## (bubble) sort the items by path cost
      i = 1;
      j = length(data);
      while (i < j) {
        i = 1;
        for (r in i:(j-1)) {
          if ( data[[r]]$totalpathcost > data[[r+1]]$totalpathcost) {
            tmp =  data[[r]]
            data[[r]] <<- data[[r+1]]
            data[[r+1]] <<- tmp
          }
        }
        j = j - 1
      }
    },
    
    dequeue = function() {
      'Dequeue removes and returns element from the head'
      if (length(data)==0) return (NULL)
      vertex = data[[1]]
      data <<- data[-1]
      return(vertex)
    },
    
    isEmpty = function(){
      'Returns true if the queue is empty, false otherwise'
      return(size()==0)
    }
  )
)