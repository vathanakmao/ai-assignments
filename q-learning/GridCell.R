GridCell <- setRefClass(
    "GridCell",
    
    fields = list(
              id = "numeric",
              up = "numeric",
              down = "numeric",
              left = "numeric",
              right = "numeric"
            )
                        
    )