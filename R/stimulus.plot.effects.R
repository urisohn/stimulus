 stimulus.plot.effects=function(df,  dv, stimulus, condition, participant , sort.by, value.labels.offset=-1,
                                 flip.sign, label.high, label.low,
                                 stimuli.numeric.labels,decimals, 
                                 ylab1,ylab2,xlab1,xlab2,simtot,...)
    {
    
    
    #1 Grab the arguments passed on to ...  
      args = list(...)
    

      col.null1='dodgerblue'
      col.null2=adjustcolor(col.null1,.1)
      col.ci = 'gray50'
      
    #2 Compute means
       obs = get.means.condition(df=df,dv=dv,stimulus=stimulus,condition=condition,sort.by=sort.by,flip.sign=flip.sign)

      #Sort
        #if (sort.by=='') obs=obs[order(obs$effect),]
        #if (sort.by!='') obs=obs[order(obs[,sort.by]),]
       
       
       #unnecessary since it is sorted by get.means.condition() already
       
       
      #Localize variables
        d = obs$effect
        ciL = obs$ciL
        ciH = obs$ciH
        n = length(d)
        label2  =  sub(paste0("^",condition,"_"), "", names(obs[2]))
        label1  =  sub(paste0("^",condition,"_"), "", names(obs[1]))
       
      #2.2 Get the null distribution  (only if sort.by is not set)
          d0=rep(d,length(unique(df[,stimulus])))   #make it equal to d just to help with code below, e.g., ylim=range(...)
          dnull=data.frame(low=d0,high=d0,mean=d0)
        
        #Resampling if sort.by is not specified
            if (sort.by=='') 
                  {
                  dnull =  get.null.distribution (df=df, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot,flip.sign=flip.sign)
                  }
            
    #3 ylim: range of y values in the plot
      
      ylim = range(c(ciL,ciH,dnull))
      
          
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
        
      #4.1 Bottom
            max.x.label = max(nchar(unique(df[,stimulus])))
            xlabel.buffer = max(0,max.x.label)*.3
            if (stimuli.numeric.labels==TRUE) xlabel.buffer=0
            mar.after[1] = mar.before[1] + xlabel.buffer
      
     #4.2 Top
        #Drop top margin if there is no main header
          mar.after[3] = ifelse ("main" %in% names(args),3,1)
      
      #4.3 Left
         width.y.label = nchar(max(d))
         mar.after[2] = max(width.y.label/3, 5.1)
         if (ylab2!='') mar.after[2]= mar.after[2] + 1
        
      #4.4 Assign it
         par(mar=mar.after)
        
     
  #8 Black dots
      if (!'cex' %in% args) plot(d,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',cex=1.5,...)
      if ('cex' %in% args)  plot(d,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',...)

   
      #horizontal line
        abline(h=0,lty=3,col='gray66')
    
   
  #7 Plot the null and its CI
    if (sort.by=='')
    {
    points(dnull$mean,type='l',col=col.null1,lty=2)
    polygon(x=c(1:n,n:1),y=c(dnull$low , rev(dnull$high)),col=col.null2,border=NA)
    }
  
  #8 Value labels


    #offset for y-position
      if (value.labels.offset==-1) value.labels.offset = diff(ylim)*.03


    #set positiom   
      offset=ifelse(d < dnull$mean, -value.labels.offset,value.labels.offset)
      y.text=d + offset
      
   #print them
      text(1:n,y.text ,round2(d ,decimals),col='blue4',cex=.65)

                  
     
  #7 CI
      arrows(x0=1:n, x1=1:n, y0=ciL,y1=ciH,col=adjustcolor('gray60',.8),code=3,angle=90,length=.02)

      
  #9 Y axis
      
      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1.5,font=2,cex=1.2,ylab1)
      mtext(side=2,line=mar.after[2]-2.5,font=3,cex=1,ylab2)
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
    
        text(1:n,par('usr')[3] , paste0(obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
        }

    #14.2 Headers
        if (xlab2=="" & sort.by=='') xlab2='(sorted by effect size)'
        if (xlab2=="" & sort.by!='') xlab2=paste0('(sorted by ',sort.by,')')
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)
      }
      
        
  #15 Legend
        if (sort.by=="")
        {
        leg1 = legend('top',
                      bty='n',
                      pch=c(16,NA,NA,NA), 
                      lty=c(NA,1,2,1),
                      lwd=c(NA,1,1,14),
                      col=c('black', col.ci, col.null1 , col.null2),
                      c(paste0('Observed effect: ',label1," - ",label2),
                        "95 CI for observed effect ",
                        "Expected under null that all stimuli same effect", 
                        "95% confidence band under null"),
                        inset=.03)
        } else {
            leg1 = legend('top',
                      bty='n',
                      pch=c(16,NA), 
                      lty=c(NA,1),
                      lwd=c(NA,1),
                      col=c('black',col.ci),
                      c(paste0('Observed effect: ',label1," - ",label2),
                        "95 CI for observed effect"),
                        inset=.03)
          
        }
          
          
    
    par(mar=mar.before)
    return(list(observed=obs, under.null=dnull))     
    
        
    }#End of function
  