#Function used to validate inputs to functions in this package, checks type and length of variable


#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)
  

#2) check1(): function that evaluates type and length of a given argument in the function
      check1 = function(var, varname, type.check,  nu.check = -1)
      {
        
        # Get the call stack
          call_stack <- sys.calls()
  
        # Check the second last entry in the stack, which is the calling function
          if (length(call_stack) >= 2) {
          caller <- as.character(call_stack[[length(call_stack) - 2]][[1]])
          }
        
        
        #Unique.values
          nu=length(unique(var))
          if (nu != nu.check & nu.check!= -1) {
              exit(paste0(caller,"() says: The argument '",varname,"' must have '",nu.check,"' unique values, but it has '",nu,"'")) 
          }
            
      
        #Type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) {
              exit(caller,"() says: The argument '",varname,"' must be an integer, but '",var, "' isn't.")  
            }
              
              }
        
        #Type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit(caller,"() says: The argument '",varname,"' must be a character variable but '",var, "' isn't.")  
          }
        
        #Type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit(caller,"() says: The argument '",varname,"' must be a numeric, but '",var, "' isn't.")  
          }
        #Type loical
          if (type.check=='logical')
          {
            
            if (is.logical(var)==FALSE)  exit(caller,"() says: The argument '",varname,"' must be either TRUE or FALSE, but '",var, "' is neither.")  
          }
      }
        
        
 
      