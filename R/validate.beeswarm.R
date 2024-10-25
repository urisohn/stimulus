 
     validate.beeswarm=function(data,  dv, stimulus, condition, 
                        flip.conditions, 
                        dv.is.percentage,
                        simtot,
                        confidence, ylim, ylab1, ylab2,
                        xlab1, xlab2, dot.spacing, 
                        col1, col2, main,
                        watermark, save.as, svg.width, svg.height,
                        args_passed)
    {
  
  #1 Function to identify source of error
      f = 'stimulus::stimulus.plot'
      
  #2 Check all arguments are of the appropriate length and type; see file check1.R
        check1(f, flip.conditions, 'logical', 1,args_passed)
        check1(f, dv.is.percentage, 'logical', 1,args_passed)
        check1(f, simtot, 'integer', 1,args_passed)
        check1(f, confidence, 'numeric', 1,args_passed)
        check1(f, ylim, 'numeric', 2,args_passed)
        check1(f, ylab1, 'character', 1,args_passed)
        check1(f, ylab2, 'character', 1,args_passed)
        check1(f, xlab1, 'character', 1,args_passed)
        check1(f, xlab2, 'character', 2,args_passed)
        check1(f, dot.spacing, 'numeric', 1,args_passed)
        check1(f, col1, 'color', 1,args_passed)
        check1(f, col2, 'color', 1,args_passed)
        check1(f, main, 'character', 1,args_passed)
        check1(f, watermark, 'logical', 1,args_passed)
        check1(f, save.as, 'character', 1,args_passed)
        check1(f, svg.width, 'numeric', 1,args_passed)
        check1(f, svg.height, 'numeric', 1,args_passed)
    
      
  
  #4 Custome checks
      check.save.as(save.as) #function in validate.R

 
      
  }

  
      
      
      