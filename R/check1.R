
#Check if a variable is as it should be
 

#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)
  

#2) check1(): function that evaluates type and length of a given argument in the function
      check1 = function(var, varname, type.check,  nu.check = -1)
      {
        
        #Unique.values
          nu=length(unique(var))
          if (nu != nu.check & nu.check!= -1) {
              exit("stimulus.plot() says the argument '",varname,"' must have '",nu.check,"'\n unique values, but it has '",nu,"'")  
          }
            
      
        #Type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) {
              exit("interprobe() says the argument '",varname,"' must be an integer, but '",var, "' isn't.")  
            }
              
              }
        
        #Type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit("interprobe() says the argument '",varname,"' must be a character variable but '",var, "' isn't.")  
          }
        
        #Type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit("interprobe() says the argument '",varname,"' must be a numeric, but '",var, "' isn't.")  
          }
        
        
        }