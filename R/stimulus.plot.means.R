 stimulus.plot.means = function(df, dv, condition, stimulus, 
                    ylab1, ylab2,  xlab1, xlab2, value.labels.offset, stimuli.numeric.labels,
                    participant,
                    label.low, label.high, decimals, legend.title, ...)
    
      {
   
    #Grab the arguments passed on to ...  
     args = list(...)
   
      
    #0 Is it a matched design?
          t = table(df[,stimulus],df[,condition])
          matched = FALSE
          if (mean(t[,1]*t[,2]>0) ==1 ) matched=TRUE
          
              #This computes the frequency of stimuli id by condition
              #if it is compared, a given ID appears only in one condition
              #if it is matched, it appears in both
              #this classifies a design as matched if 100%
              #of stimuli appear in both conditions
              


    #1 Get the means by condition
      means.obs = get.means.condition(df=df,dv=dv,stimulus=stimulus,condition=condition,participant=participant)
      


    #2 local names
      if (matched==TRUE) {
      y1 = means.obs[,3] #The high values condition
      y0 = means.obs[,2] #the low values condition
      n=length(y0)
      label.high =  sub("^condition_", "", names(means.obs[3]))
      label.low  =  sub("^condition_", "", names(means.obs[2]))
      
      } else {
        n=nrow(means.obs)/2
        y0=means.obs[1:n,2]
        y1=means.obs[(n+1):(2*n),2]
      }
      


      
      
    #3 ylim: range of y values in the plot
      ylim = range(c(y0,y1))
      dy = diff(ylim)
      ylim[2]=ylim[2]+.25*dy  #Give a 25% buffer on top (for the legend)
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
        
        
        if (all(mar.before==mar.default))
        {
         
        #4.1 BOTTOM
            
            mar.after[1] = mar.before[1] + xlabel.buffer
        }
        
        

        #4.2 TOP
          #Drop top margin if there is no main header
            if (!"main" %in% names(args)) {
              mar.after[3] = 1
            }
          
        
        #4.3 LEFT
          if (ylab2!='') mar.after[2]=5.5
          
          par(mar=mar.after)
            
           
  #5 Black dots
       n=length(y1)

      plot(y1,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',xlim=c(1,n+3),...)
    
   
 
  #6 Segments
    if (matched==TRUE)
    {
    lty=ifelse(y1>y0,1,2)
    col=ifelse(y1 > y0,'blue','red')
    segments(x0=1:n, x1=1:n,y0=y0, y1=y1,lty=lty,col=col)
    }
   
  #7 White dots   
      cex=1
      if ('cex' %in% names(args)) cex=args$cex
    points(y0,pch=21,col='black',bg='white',cex=cex)

    
  #8 Redo black dots to cover any red lines
    if (matched==TRUE) points(y1,pch=16,cex=cex)
    
  #9 Value labels
    
    #Values
      bh=pmax(y0,y1)
      bl=pmin(y0,y1)
   
    #offset for y-position
      if (value.labels.offset==-1) value.labels.offset = diff(ylim)*.03
   
    #Labels themselves
      text(1:n,bh + value.labels.offset,round2(bh,decimals),col='gray60',cex=.65)
      text(1:n,bl - value.labels.offset,round2(bl,decimals),col='gray60',cex=.65)
   
  #12 Overall means
    m1 = mean(y1)
    m0 = mean(y0)
    if (matched==TRUE) segments(x0=n+3, x1=n+3,y0=m0, y1=m1)
    points(n+3,m1,pch=16,cex=2) 
    points(n+3,m0,pch=21,cex=2,col='black',bg='white')
    axis(side=1,at=n+3,"MEAN",font=2)
    text(n+3,m1+value.labels.offset*2 , round2(m1,decimals),cex=.8,col='gray50')
    text(n+3,m0-value.labels.offset*2 , round2(m0,decimals),cex=.8,col='gray50')

  #13 Y axis
      if (!"yaxt" %in% names(args))
      {
      if (ylab2=="") mtext(side=2,line=2.65,font=2,cex=1.2,ylab1)
       
      if (ylab2!="")  
        {
        mtext(side=2,line=3.8,font=2,cex=1.2,ylab1)
        mtext(side=2,line=2.65,font=3,cex=1,ylab2)
        }
      
      }
    
    
  #14 X-axis
    
    #14.1 Stimuli labels
        
        if (stimuli.numeric.labels==TRUE) {
          #All numbers if less than 10
            if (n<=10)  axis(side=1, at=1:n)
          #Every 5 otherwise
            if (n>10)   axis(side=1, at=c(1,seq(5,n,5),n))
        } else {
    
        if (matched==TRUE) text(1:n,par('usr')[3] , paste0(means.obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
        if (matched==FALSE) {
          text(1:n-.25,par('usr')[3] , paste0(means.obs[1:n ,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
          text(1:n+.25,par('usr')[3] , paste0(means.obs[(n+1):(2*n) , stimulus],"  "),srt=80,xpd=TRUE,adj=1,font=2)
          
          
        }
          
          }

    #14.2 Headers
        if (xlab2=="" & matched==TRUE) xlab2='(sorted by effect size)'
        if (xlab2=="" & matched==FALSE) xlab2='(sorted by raw means)'

            #For matched stimuli, the default is the above text
        
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)

        
  #15 Legend
        leg1 = legend('top',pch=c(16,1),c(label.high,label.low),inset=.03,bty='n')
        
        #Legend title?
        if (legend.title!='')
        {
        title.y = par("usr")[4] - (par("usr")[4] - leg1$rect$top)*.75
        title.x = leg1$rect$left
        text(title.x,title.y,legend.title,adj=0,font=2)
        }
    
  #16 Return margins to where they were
    par(mar=mar.before)


    return(means.obs)
  
  }
  