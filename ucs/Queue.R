Queue <- setRefClass(
  "Queue",
  
  fields = list(
    data = "vector"
  ),
  
  methods = list (
    
    size = function(){
      'Returns the number of items'
      return(length(data))
    },
    
    enqueue = function(item) {
      'Enqueue inserts element at the tail'
      data <<- c(item, data)
    },
    
    dequeue = function() {
      'Dequeue removes and returns element from the head'
      if (length(data)==0) return (NULL)
      item <- data[1]
      data <<- data[-1]
      return(item)
    },
    
    isEmpty = function(){
      'Returns true if the queue is empty, false otherwise'
      return(size()==0)
    }
  )
)