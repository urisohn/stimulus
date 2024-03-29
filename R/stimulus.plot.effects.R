 stimulus.plot.effects=function(df,  dv, stimulus, condition, participant , value.labels.offset=-1,
                                 flip.sign,
                                 stimuli.numeric.labels,decimals, 
                                 ylab1,ylab2,xlab1,xlab2,simtot,...)
    {
    
    
    #1 Grab the arguments passed on to ...  
      args = list(...)
    

    #2 Compute means
       obs = get.means.condition(df=df,dv=dv,stimulus=stimulus,condition=condition,participant=participant)


      #Sort
        obs=obs[order(obs$effect),]
        
       
    #Localize variables
        d = obs$effect
        n = length(d)
        
      #2.2 Get the null distribution 
        dnull =  get.null.distribution (df=df, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot)
      
    #3 Flip sign\
        if (flip.sign==TRUE)
          { 
          d = -1 * d 
          dnull = -1 * dnull
          }
          
    
      
    #3 ylim: range of y values in the plot
      
        
      ylim = range(c(d , dnull$low , dnull$high))
      dy = diff(ylim)
      ylim[2]=ylim[2]+.28*dy  #Give a 28% buffer on top (for the legend)
      ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels
   
    #4 Margins
      #Get current margins
        mar.before =  par("mar")
        mar.after  =  mar.before
              
      #Only change them if they are not the default (so users can over-ride it by choosing their own)
        mar.default = c(5.1, 4.1, 4.1, 2.1)

        
            
      #Label calculations for bottom margin 
        max.length = max(nchar(unique(df[,stimulus])))
        xlabel.buffer = max(0,max.length-3)*.3
        if (stimuli.numeric.labels==TRUE) xlabel.buffer=0
        
      #4.1 BOTTOM
        if (all(mar.before==mar.default))  {
          mar.after[1] = mar.before[1] + xlabel.buffer
        }
        
      
      #4.2 TOP
          #Drop top margin if there is no main header
            if (!"main" %in% names(args)) {
              mar.after[3] = 1
            }
          
      #4.3 LEFT
          if (ylab2!='') mar.after[2]=5.5
          
      #4.4 Apply margin
          par(mar=mar.after)
        
     
  #5 Formatting
    #5.1 cex (side of dots)
      cex = 2
      
                   
  #6 Black dots
     if (!'cex' %in% args) plot(d,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',cex=1.5,...)
     if ('cex' %in% args)  plot(d,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',...)
    
      
      #horizontal line
        abline(h=0,lty=2,col='gray66')
    
   
  #7 Null region
    points(dnull$mean,type='l')
    polygon(x=c(1:n,n:1),y=c(dnull$low , rev(dnull$high)),col=adjustcolor('blue',.1),border=NA)

  
  #8 Value labels


    #offset for y-position
      if (value.labels.offset==-1) value.labels.offset = diff(ylim)*.03


    #set positiom   
      #y.text = ifelse(d < dnull$mean, d - value.labels.offset,d + value.labels.offset )
      offset=ifelse(d < dnull$mean, -value.labels.offset,value.labels.offset)
      #y.text=d-value.labels.offset
      y.text=d + offset
      

   #print them
      text(1:n,y.text ,round2(d ,decimals),col='gray60',cex=.65)

  
  #9 Y axis
      
      if (!"yaxt" %in% names(args))
      {
        
      #Default ylabs
        if (ylab1=="") {
            ylab1='Effect' 
            ylab2='(difference of means)'
            } 
        
      
       
      if (ylab2!="")  
        {
        mtext(side=2,line=3.8,font=2,cex=1.2,ylab1)
        mtext(side=2,line=2.65,font=3,cex=1,ylab2)
        }
      
      }
      
      
    #10 x-axis
      #Skip if xaxt='n' is set
      if (!'xaxt' %in% args)
      {
    
      #10.1 Stimuli labels
        
        if (stimuli.numeric.labels==TRUE) {
          #All numbers if less than 10
            if (n<=10)  axis(side=1, at=1:n)
          #Every 5 otherwise
            if (n>10)   axis(side=1, at=c(1,seq(5,n,5),n))
        } else {
    
        text(1:n,par('usr')[3] , paste0(obs[,1],"  "),srt=80,xpd=TRUE,adj=1)
        }

    #14.2 Headers
        if (xlab2=="" ) xlab2='(sorted by effect size)'
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)

      
      }
      
        
  #15 Legend
        leg1 = legend('top',
                      bty='n',
                      pch=c(16,NA,NA), 
                      lty=c(NA,1,1),
                      lwd=c(NA,1,14),
                      col=c('black','black',adjustcolor('blue',.2)),
                      c('Observed effects',
                        "Expected when all stimuli have same effect", 
                        "95% confidence band"),
                        inset=.03)
    
      
  
    return(list(observed=obs, under.null=dnull))     
    
        
    }#End of function
  