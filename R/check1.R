#Function used to validate inputs to functions in this package, checks type and length of variable


#1) Auxiliary - Integer?    
    is.integer2 = function(x) all(floor(x)==x)

    
    #Color
    valid_color <- function(col1) {
        # Try converting the color to RGB; if it fails, it's not a valid color
        tryCatch({
          col2rgb(col1)
          TRUE  # If no error, the color is valid
        }, error = function(e) {
          message("Error: '", col1, "' is not a valid color.")
          FALSE  # If error, the color is invalid
        })
      }
            

#2) check1(): function that evaluates type and length of a given argument in the function
      check1 = function(f,var, type.check,  nu.check = -1,args_passed)
      {
        
        #varname
          varname <- deparse(substitute(var))
          
        #If was not assigned a value 
          if (!varname %in% args_passed) return(TRUE)
        
        
      
        #Unique.values
          nu=length(unique(var))
          if (nu != nu.check & nu.check!= -1) {
              exit(paste0(f,"() says: the argument '",varname,"' must have '",nu.check,"' unique values, but it has '",nu,"'")) 
          }
            
      
        #Type integer
          if (type.check=='integer')
            {
            if (is.integer2(var)==FALSE) {
              exit(f,"() says: the argument '",varname,"' must be an integer, but '",var, "' isn't.")  
            }
              
              }
        
        #Type character
          if (type.check=='character')
            {
            if (is.character(var)==FALSE) exit(f,"() says: the argument '",varname,"' must be a character variable but '",var, "' isn't.")  
          }
        
        #Type numeric
          if (type.check=='numeric')
            {
            if (is.numeric(var)==FALSE) exit(f,"() says: the argument '",varname,"' must be a numeric, but '",var, "' isn't.")  
          }
        #Type loical
          if (type.check=='logical')
          {
            
            if (is.logical(var)==FALSE)  exit(f,"() says: the argument '",varname,"' must be either TRUE or FALSE, but '",var, "' is neither.")  
          }
          
        #Type color
          if (type.check=='color')
          {
            is.color <- valid_color(var) 
            if (!is.color) exit(f,"() says: the argument '",varname,"' must be a valid color but '",var, "' isn't recognized as a color.")  
            
            }
            
          
      }
        
        
      
      