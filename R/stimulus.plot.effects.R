 stimulus.plot.effects=function(df,  dv, stimulus, condition, participant ,
                                overall.estimate,
                                overall.ci,
                                overall.label, 
                                overall.p,
                                model,
                                sort.by, 
                                value.labels.offset=-1,
                                flip.sign, 
                                label.high, 
                                label.low,
                                stimuli.numeric.labels,
                                decimals, 
                                ylab1,ylab2,xlab1,xlab2,
                                cex,
                                simtot,...)
    {
    
    
    #1 Grab the arguments passed on to ...  
      args = list(...)
      col.null1='dodgerblue'
      col.null2=adjustcolor(col.null1,.1)
      col.ci = 'gray50'
      col.overall = 'red4'
      
    #2 Compute means by stimulus
       obs = get.means.condition(df=df,dv=dv,stimulus=stimulus,condition=condition,sort.by=sort.by,flip.sign=flip.sign)

      #Localize stimulus variables
        d = obs$effect
        ciL = obs$ciL
        ciH = obs$ciH
        n = length(d)
        label2  =  sub(paste0("^",condition,"_"), "", names(obs[2]))
        label1  =  sub(paste0("^",condition,"_"), "", names(obs[1]))
       
    #3 Get the null distribution  (only if sort.by is not set)
          d0=rep(d,length(unique(df[,stimulus])))   #make it equal to d just to help with code below, e.g., ylim=range(...)
          dnull=data.frame(low=d0,high=d0,mean=d0)
        
        #Resampling if sort.by is not specified
            if (sort.by=='') 
                  {
                  dnull =  get.null.distribution (df=df, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot,flip.sign=flip.sign)
                  }
            
    #4 ylim: range of y values in the plot
      
      ylim = range(c(ciL,ciH,dnull))
      dy = diff(ylim)
      ylim[2]=ylim[2]+.28*dy  #Give a 28% buffer on top (for the legend)
      ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels
   
    #5 get models if specified
       if (length(model)>0)
       {
        model.results = get.model.results(df, dv, stimulus, condition, participant,model,flip.sign)

        overall.estimate  = model.results$m.mean
        overall.ci        = model.results$m.ci
        overall.labels    = model.results$m.labels
        overall.p         = model.results$m.p
  
       }
      
        
      
      
    #5 xlim 
      n1 = length(overall.estimate)
      xmax = ifelse(n1 > 0, length(d) + n1 +1.5, length(d))
      xlim = c(1,xmax)
              
      
    #6 Margins

          #Get current margins
            mar.before =  par("mar")
            mar.after  =  mar.before
                  
              
          #Label calculations for bottom margin 
            max.length = max(nchar(unique(df[,stimulus])))
            xlabel.buffer = max(0,max.length-3)*.3
            if (stimuli.numeric.labels==TRUE) xlabel.buffer=0
            
          #Bottom
                max.x.label = max(nchar(unique(df[,stimulus])))
                xlabel.buffer = max(0,max.x.label)*.3
                if (stimuli.numeric.labels==TRUE) xlabel.buffer=0
                mar.after[1] = mar.before[1] + xlabel.buffer
          
          #Top
            #Drop top margin if there is no main header
              mar.after[3] = ifelse ("main" %in% names(args),3,1)
          
          #Left
               width.y.label = nchar(max(d))
               mar.after[2] = max(width.y.label/3, 5.1)
               if (ylab2!='') mar.after[2]= mar.after[2] + 1
              
          #Implement
             par(mar=mar.after)
        
    
  #6 Black dots
      plot(d,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='', cex=cex, xlim=xlim)#, ...)

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
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1, at=n/2)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2, at=n/2)
      }
      
        
  #15 Legend
        if (sort.by=="")
        {
        leg1 = legend('topleft',
                      bty='n',
                      pch=c(16,NA,NA,NA), 
                      lty=c(NA,1,2,1),
                      lwd=c(NA,1,1,14),
                      col=c('black', col.ci, col.null1 , col.null2),
                      c(paste0('Observed effect: ',label1," - ",label2),
                        "95 CI for observed effect ",
                        "Expected under null of same effect size for all stimuli", 
                        "95% confidence band under null"),
                        inset=.03)
        } else {
            leg1 = legend('topleft',
                      bty='n',
                      pch=c(16,NA), 
                      lty=c(NA,1),
                      lwd=c(NA,1),
                      col=c('black',col.ci),
                      c(paste0('Observed effect: ',label1," - ",label2),
                        "95 CI for observed effect"),
                        inset=.03)
          
        }
          
          
  #16 Overall
      if (n1 > 0)
      {
        
        xs=(n+1):(n+n1)+1
        
      
      
      #Markers
        points( x=xs,
                y= overall.estimate,
                pch=16,
                cex=cex*1.5,
                col=col.overall)
          
      #CI
        arrows(x0=xs,
               x1=xs,
               y0=overall.ci[seq(1,n1*2,2)],
               y1=overall.ci[seq(2,n1*2,2)],
               col=col.overall,
               code=3,
               length=.03,
               angle=90)
        
      #Labels
         text(xs,par('usr')[3] , paste0(overall.labels," ") ,srt=80,xpd=TRUE,adj=1,col=col.overall)


      #p-value
         text(xs,max(overall.ci),pos=3,format.p(overall.p),col=col.overall,cex=.7,font=2)

     #"Overall" 
         y.overall=par('usr')[4]- 0.18* (par('usr')[4] - par('usr')[3])
         text(mean(xs),y.overall,pos=3,"Overall",cex=1.2,font=2)

     #Vertical separator
           abline(v= n+1 ,lwd=2) 

         
      #Vertical line
        # abline(v = n+1,col='black')
          
      }
      

    par(mar=mar.before)
    
  #Results
    results = list(observed=obs, under.null=dnull)
    if (exists('model.results')) results$model.results= model.results
    return(results)     
    
        
    }#End of function
  