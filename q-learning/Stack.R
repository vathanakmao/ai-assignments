Stack <- setRefClass(
  "Stack",
  
  fields = list(
    data = "vector"
  ),
  
  methods = list (
    
    size = function(){
      'Returns the number of items in the stack.'
      return(length(data))
    },
    
    push = function(item) {
      'Push inserts element at the front of the stack.'
      data <<- c(item, data)
    },
    
    pop = function() {
      'Pop removes and returns the head of the stack'
      if (length(data)==0) return (NULL)
      item <- data[1]
      data <<- data[-1]
      return(item)
    },
    
    isEmpty = function(){
      'Returns true if the stack is empty, false otherwise'
      return(size()==0)
    }
  )
)