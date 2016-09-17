Agent <- setRefClass(
  "Agent",
  
  fields = list(
    q.table = "matrix"
  ),
  
  methods = list(
    
    ##
    # r.table - (matrix) 
    #
    learn = function(r.table, init, goal) {
      return (r.table)
    }
  )
)