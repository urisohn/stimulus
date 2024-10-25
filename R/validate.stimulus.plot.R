validate.stimulus.plot=function(plot.type, data,dv, condition, stimulus, save.as,  svg.width, svg.height,  sort.by,  flip.conditions,model, 
                                  overall.estimate, overall.ci,overall.p, overall.label,ylab1, ylab2, xlab1, xlab2,
                                  decimals, null.method,dv.is.percentage, legend.title,simtot, watermark, seed, ylim,args_passed)
    {
  
  #1 Function to identify source of error
      f = 'stimulus::stimulus.plot'
      
  #2 Check all arguments are of the appropriate length and type; see file check1.R
      check1(f, plot.type, 'character', 1,args_passed)
      check1(f, save.as, 'character', 1,args_passed)
      check1(f, svg.width, 'numeric', 1,args_passed)
      check1(f, svg.height, 'numeric', 1,args_passed)
      check1(f, sort.by, 'charcter', 1,args_passed)
      check1(f, flip.conditions, 'logical', 1,args_passed)
      check1(f, overall.estimate, 'numeric', 1,args_passed)
      check1(f, overall.ci, 'numeric', -1,args_passed)       #Checked below, and it need not be equal to 2.    
      check1(f, overall.p, 'numeric', 1,args_passed)
      check1(f, overall.label, 'character', 1,args_passed)
      check1(f, ylab1, 'character', 1,args_passed)
      check1(f, ylab2, 'character', 1,args_passed)
      check1(f, xlab1, 'character', 1,args_passed)
      check1(f, xlab2, 'character', 1,args_passed)
      check1(f, decimals, 'numeric', 1,args_passed)
      check1(f, null.method, 'character', 1,args_passed)
      check1(f, dv.is.percentage, 'logical', 1,args_passed)
      check1(f, legend.title, 'character', 1,args_passed)
      check1(f, simtot, 'integer', 1,args_passed)
      check1(f, watermark, 'logical', 1,args_passed)
      check1(f, seed, 'numeric', 1,args_passed)
      check1(f, ylim, 'numeric', 2,args_passed)
      
      
  #3 Check with limited set of values
      if (!plot.type %in% c('means','effects'))     exit(f,"() says: the argument 'plot.type' must be either 'effects' or 'means', you entered '",plot.type,"'")  
      if (!null.method %in% c('shuffle','demeans')) exit(f,"() says: the argument 'null.method' must be either 'shuffle' or 'demeans', you entered '",null.method,"'")  
      if (any(!model %in% c('all','regression','intercepts','slopes'))) {
          exit(f,"() says:If the the argument 'model' is set, it must include only a subset of the following four values:\n ",
             "'all','regression','intercepts','slopes' ")
      }
      

  #4 Custome checks
      check.save.as(save.as) #function 5 here

  #5 No stimulus plot for compared design
      t = table(data[,stimulus],data[,condition])
      matched =mean(t[,1]*t[,2]>0) ==1
      if (matched==FALSE) {
              exit(format_msg(paste0(
              f,"() says: The stimulus variable ('", stimulus,"') does not have the same values ",
              "across conditions. If you have a compared-stimulus design, with different ",
              "and unmatched stimuli across condition, use stimulus.beeswarm(). If you do ",
              "have a treated- or matched-stimulus design, then check that you have a matching ",
              "stimulus identifier for the pairs of stimuli across conditions."),header='Cannot do Stimulus Plot for compared-stimulus designs'))
      
      }
      
  #6 overall
      n1=length(overall.estimate)
      n2=length(overall.ci)
      n3=length(overall.p)
      n4=length(overall.label)

      if (length(unique(c(n1,n3,n4)))>1 & n1>0) exit("The 'overall' arguments (estimate, p, and label) must have the same legnth")
      if (n2!=2*n1) exit(f,"() says: Make sure that there are twice the number of values in overall.ci as in overall.estimate")
     

      
      
  }

  
      
      
      