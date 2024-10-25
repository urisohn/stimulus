
  
#FUnction 3 - validate dots

  
  
#Function 4 - Validate beeswarm
    validate.beeswarm=function(data,  dv, stimulus, condition, 
                        flip.conditions, 
                        dv.is.percentage,
                        simtot,
                        confidence,
                        ylim,
                        ylab1,
                        ylab2,
                        xlab1,
                        xlab2,
                        dot.spacing,
                        col1,
                        col2,
                        main,
                        watermark,
                        save.as,
                        svg.width,
                        svg.height,
                        args_passed)
    {
      
    
    
    check1(flip.conditions , 'svg.heigth', 'logical', 1,args_passed)
    check1(dv , 'dv', 'character', 1,args_passed)
    check1(stimulus , 'stimulus', 'character', 1,args_passed)
    check1(condition , 'condition', 'character', 1,args_passed)
    check1(simtot , 'simtot', 'integer', 1,args_passed)
    check1(ylab1 , 'ylab1', 'character', 1,args_passed)
    check1(ylab2 , 'ylab2', 'character', 1,args_passed)
    check1(xlab1 , 'xlab1', 'character', 1,args_passed)
    check1(xlab2 , 'xlab2', 'character', 1,args_passed)
    check1(watermark,'watermark','logical',1,args_passed)

    check1(confidence ,'confidence', 'numeric', 1,args_passed)
    if (svg.height!='') check1(svg.width , 'svg.heigth', 'numeric', 1,args_passed)
    if (svg.width!='') check1(svg.width , 'svg.heigth', 'numeric', 1,args_passed)
      
    check.save.as(save.as)       #function 5 
    check.confidence(confidence) #function 6


    }
    
    
  check.save.as=function(save.as)
  {
    if (save.as!='')  
    {
           call_stack <- sys.calls()
  
        # Check the second last entry in the stack, which is the calling function
          if (length(call_stack) >= 2) {
          caller <- as.character(call_stack[[length(call_stack) - 2]][[1]])
          }
     extension= tools::file_ext(save.as)  
     
     if (!extension %in% c('svg','png')) exit(caller,"() says: 'save.as' must have extension .svg or .png")
    }
  }
  
  check.confidence=function(confidence)
  {
     # Get the call stack
          call_stack <- sys.calls()
  
        # Check the second last entry in the stack, which is the calling function
          if (length(call_stack) >= 2) {
          caller <- as.character(call_stack[[length(call_stack) - 2]][[1]])
          }
    
    if (confidence<5.1 | confidence>=99.9) exit(caller,"() says: 'confidence' must be between 5.1 and 99.9; for 95% confidence use 95, not .95")
  
    
  }
    
    