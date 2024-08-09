 stimulus.plot.effects=function(data,  dv, stimulus, condition, participant ,
                                dataname,
                                overall.estimate,
                                overall.ci,
                                overall.label, 
                                overall.p,
                                model,
                                sort.by, 
                                flip.conditions, 
                                decimals, 
                                dv.is.percentage,
                                ylab1,ylab2,xlab1,xlab2,
                                simtot,
                                ylim,
                                seed, 
                                ...)
    {
    

         
         
  #------------------------------------
    #0 colors
      col.null1   = 'dodgerblue'
      col.null2   =  adjustcolor(col.null1,.1)
      col.ci      = 'gray50'
      col.overall = 'purple'
    
    #1 Grab the arguments passed on to ...  
      args = list(...)
      args_to_drop=c(decimals, dv.is.percentage,ylab1,ylab2,xlab1,xlab2)
      args <- args[!(names(args) %in% args_to_drop)]

      
    #2 Compute means by stimulus
       obs = get.means.condition(data=data,dv=dv,stimulus=stimulus,condition=condition,sort.by=sort.by,flip.conditions=flip.conditions)

      #Localize stimulus variables
        d = obs$effect
        ciL = obs$ciL
        ciH = obs$ciH
        n = length(d)
        label2  =  sub(paste0("^",condition,"_"), "", names(obs[2]))
        label1  =  sub(paste0("^",condition,"_"), "", names(obs[1]))
       
        if (flip.conditions)
        {
          l1=label1
          l2=label2
          label1=l2
          label2=l1
          }
        
    #3 Get the null distribution  (only if sort.by is not set)
          d0=rep(d,length(unique(data[,stimulus])))   #make it equal to d just to help with code below, e.g., ylim=range(...)
          dnull=data.frame(low=d0,high=d0,mean=d0)
        
        #Resampling if sort.by is not specified
            if (sort.by=='') 
                  {
              
                  #get new call's md5
                     #Arguments
                        mc <- match.call(expand.dots = TRUE)
                        args <- as.list(mc)[-1]
                        args <- lapply(args, eval, envir = parent.frame())
                        md5.args = get.md5(args)
                    
                     #dataframe
                      md5.data   = get.md5(data)  
                      
                    #Combine for single md5 
                      md5s=paste0(md5.args ,  md5.data)

                    
                  #if dnull for that md5s has been saved, load it
                    if (does.cache.d.exist(md5s)) 
                    {
                      dnull = .GlobalEnv$.stimulus.cache[[md5s]]
                      
                  #else run it                  
                    } else  {
                      set.seed(seed)
                      dnull =  get.null.distribution (data=data, dv=dv, stimulus=stimulus, condition=condition, participant=participant,simtot=simtot,flip.conditions=flip.conditions)
                   
                  #Save
                      .GlobalEnv$.stimulus.cache[[md5s]]=dnull
                    
                }

            } #End if sort.by!=''
          
          
    #4 ylim: range of y values in the plot
      
      if (length(ylim)<2)
      {
      ylim = range(c(ciL,ciH,dnull))
      dy = diff(ylim)
      
      ylim[2]=ylim[2]+.28*dy  #Give a 28% buffer on top (for the legend)
      ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels
      }
          
    #5 get models if specified
       if (length(model)>0)
       {
        model.results = get.model.results(data, dataname, dv, stimulus, condition, participant,model,flip.conditions)

        overall.estimate  = model.results$m.mean
        overall.ci        = model.results$m.ci
        if (length(overall.label)==0) overall.label= model.results$m.labels
        overall.p         = model.results$m.p
  
       }

    #5 xlim 
      n1 = length(overall.estimate)
      xmax = ifelse(n1 > 0, length(d) + n1 +1, length(d)+.25)
      xlim = c(1,xmax)
              
      
    #6 Margins

          #Get current margins
            mar.before =  par("mar")
            mar.after  =  mar.before
                  
              
          #Label calculations for bottom margin 
            max.length = max(nchar(unique(data[,stimulus])))
            xlabel.buffer = max(0,max.length-3)*.3

          #Bottom
                max.x.label = max(nchar(unique(data[,stimulus])))
                xlabel.buffer = max(0,max.x.label)*.3
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
     if (dv.is.percentage==FALSE)  plot(d,          pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='', cex=1.5, xlim=xlim, ...)
     if (dv.is.percentage==TRUE )  plot(d,yaxt='n', pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='', cex=1.5, xlim=xlim, ...)


      #horizontal line
        abline(h=0,lty=3,col='gray66')
    
   
  #7 Plot the null and its CI
    if (sort.by=='')
    {
    points(dnull$mean,type='l',col=col.null1,lty=2)
    polygon(x=c(1:n,n:1),y=c(dnull$low , rev(dnull$high)),col=col.null2,border=NA)
    }
  
  #8 Value labels

    #set position   
      y.text=d 
      
   #print them
    
      #How many decimals to show?
        if (decimals=='auto') d.decimals=auto.decimals(d)
        if (decimals!='auto') d.decimals=decimals
      
      if (dv.is.percentage==FALSE) text(1:n,d ,round2(d , d.decimals),col='blue4',cex=.65,pos=4)
      if (dv.is.percentage==TRUE)  text(1:n,d ,format_percent(d), col='blue4',cex=.65,pos=4) #utils.R #9 <-- 'format_percent()'

     
  #7 CI
      arrows(x0=1:n, x1=1:n, y0=ciL,y1=ciH,col=adjustcolor('gray60',.8),code=3,angle=90,length=.02)

      
  #9 Y axis
      if (ylab1=='') ylab1=paste0("Effect on '",dv,"'")

      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1.5,font=2,cex=1.2,ylab1)
      mtext(side=2,line=mar.after[2]-2.5,font=3,cex=1,ylab2)
      
      #Y labels for percentages
        ys=pretty(c(ciL,ciH))
        if (dv.is.percentage==TRUE) axis(side=2,at=ys,paste0(ys*100,"%"),las=1)
      }

      
      
      
    #10 x-axis
      #Skip if xaxt='n' is set
      if (!'xaxt' %in% args)
      {
    
      #10.1 Stimuli labels
        text(1:n,par('usr')[3] , paste0(obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
        

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
                      y.intersp = 1.5,
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
                      y.intersp = 1.5,
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
                cex=1.5*1.5,
                col=col.overall)
          
      #CI
        arrows(x0=xs,x1=xs, 
               y0=overall.ci[seq(1,n1*2,2)], y1=overall.ci[seq(2,n1*2,2)],
               col=col.overall, code=3, length=.03, angle=90)
        
      #Labels
         overall.label =  gsub("\\n", " \n", overall.label)
         text(xs,par('usr')[3] , paste0(overall.label," ") ,srt=80,xpd=TRUE,adj=1,col=col.overall)


      #p-value
         text(xs,max(overall.ci),pos=3,formatted.p(overall.p),col=col.overall,cex=.7,font=2)

     #"Overall" 
         y.overall=par('usr')[4]- 0.18* (par('usr')[4] - par('usr')[3])
         text(mean(xs),y.overall,pos=3,"Overall",cex=1.2,font=2)

     #Vertical separator
           abline(v= n+1 ,lwd=2) 

     #Overall value label
            if (dv.is.percentage==FALSE)  text(xs,overall.estimate , round(overall.estimate, auto.decimals(overall.estimate)),    cex=.65,col='purple',pos=4)
            if (dv.is.percentage==TRUE)   text(xs,overall.estimate , format_percent(overall.estimate),                            cex=.65,col='purple',pos=4)      
      }
    par(mar=mar.before)
    
  #Results
    results = list(observed=obs, under.null=dnull)
    if (exists('model.results')) results$model.results= model.results
    return(results)     
    
        
    }#End of function
  
 