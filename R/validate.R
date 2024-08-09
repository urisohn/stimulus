
#Function 1 - validate arguments

  validate.arguments = function(data, dv, condition, stimulus, sort.by, plot.type, 
                              flip.conditions, ylab1, ylab2, xlab1, xlab2, 
                               decimals,
                              participant, legend.title,simtot,
                              dataname,model,    
                              overall.estimate, overall.ci,overall.p,overall.label)
  {
  
  #1 Check all arguments are of the appropriate length and type
      check1(plot.type , 'plot.type', 'character', 1)
      check1(flip.conditions , 'flip.conditions', 'logical', 1)
      check1(ylab1 , 'ylab1', 'character', 1)
      check1(ylab2 , 'ylab2', 'character', 1)
      check1(xlab1 , 'xlab1', 'character', 1)
      check1(xlab2 , 'xlab2', 'character', 1)
      if (decimals!='auto') check1(decimals , 'decimals', 'numeric', 1)
      check1(plot.type , 'plot.type', 'character', 1)

    #Model  
      if (any(!model %in% c('all','regression','intercepts','slopes'))) {
        exit("If the the argument 'model' is set, it must include only a subset of the following four values:\n ",
             "'all','regression','intercepts','slopes' ")
      }
      
    #overall
      n1=length(overall.estimate)
      n2=length(overall.ci)
      n3=length(overall.p)
      n4=length(overall.label)

      if (length(unique(c(n1,n3,n4)))>1 & n1>0) exit("The 'overall' arguments (estimate, p, and label) must have the same legnth")
      if (n2!=2*n1) exit("Make sure that there are twice the number of values in overall.ci as in overall.estimate")
      if (n1>0 & any(!is.numeric(overall.estimate),!is.numeric(overall.estimate),!is.numeric(overall.estimate))) exit ("The 'overall' arguments must be numeric")
      

      }


#Function 2 - Validate data

  validate.data = function(data, dv, condition, stimulus, sort.by,participant,dataname)
  {
      n1=names(data)
      if (!dv %in% n1)        exit("stimulus.plot() says the dv ('",dv,    "') is not in the dataset '",dataname,"'.")
      if (!condition %in% n1) exit("stimulus.plot() says the condition variable ('",condition,"') is not in the dataset '",dataname,"'.")
      if (!stimulus %in% n1)  exit("stimulus.plot() says the stimulus variable ('",stimulus,"') is not in the dataset '",dataname,"'.")
      if (!sort.by %in% c(n1,"")    ) exit("stimulus.plot() says the sort.by variable ('",sort.by,"') is not in the dataset '",dataname,"'.")
      if (!participant %in% c(n1,"")) exit("stimulus.plot() says the participant variable ('",participant,"') is not in the dataset '",dataname,"'.")
  }
  
  
  
#FUnction 3 - validate dots
  validate.dots=function(...)
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
    
    exit("All arguments in the `stimulus.plot()` call must either be defined for\n",
        "that function (check out help page) or must be arguments for base R's plot(),\n",
        "but you included the following arguments that are neither:\n", 
        paste(invalid_args, collapse = ", "))
  }
  }