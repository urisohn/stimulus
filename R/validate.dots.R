  validate.dots=function(f,...)
  {
    #Get the arguments  
      dot_args <- list(...)
      #dot_args <- paste0('"', dot_args, '"')

      
    # Get the list of valid arguments for plot
      valid_plot_args <- names(formals(graphics::plot.default))
  
    # Check if all names in ... are valid plot arguments
       invalid_args <- setdiff(names(dot_args), valid_plot_args)
  
  # If there are any invalid arguments, throw an error
  if (length(invalid_args) > 0) {
    
    invalid_args <- paste0('"', invalid_args, '"')
    
    exit("These arguments are neither part of ",f,"(), nor Base R plot():\n", 
        paste(invalid_args, collapse = ", "))
  }
  }