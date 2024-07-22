 stimulus.plot.means = function(df, dv, condition, stimulus, 
                    participant, sort.by,
                    flip.condition,
                    ylab1, ylab2,  xlab1, xlab2, 
                    decimals, legend.title, col1,col2,...)
    
      {

    #Grab the arguments passed on to ...  
     args = list(...)
   
      col1='black'
      col2='red4'
      
      
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
      means.obs = get.means.condition(df=df,dv=dv,stimulus=stimulus,flip.condition=flip.condition, condition=condition,sort.by=sort.by)

   #2 local names
      n=nrow(means.obs)/2

      
      #Condition
       ucond=sort(unique(df[,condition]))
       cond1= paste0(condition,"_",ucond[1])
       cond2= paste0(condition,"_",ucond[2])
      
      if (matched==TRUE) {
        y1 = means.obs[,cond1] #Condition 1
        y2 = means.obs[,cond2] #Condition 2
        label1  =  sub("^condition_", "", cond1)
        label2  =  sub("^condition_", "", cond2)
        
        } else {
          y1=means.obs[1:n,2]
          y2=means.obs[(n+1):(2*n),2]
      }

      
      #Which value is higher for each stimulus
        bh=pmax(y1 , y2)
        bl=pmin(y1 , y2)  
      


      #Overall means
        m1 = mean(y1)
        m2 = mean(y2)

      
    #3 ylim: range of y values in the plot
      ylim = range(c(y1,y2))
      dy = diff(ylim)
      ylim[2]=ylim[2]+.25*dy  #Give a 25% buffer on top (for the legend)
      ylim[1]=ylim[1]-.03*dy  #give a 3% buffer below, for the value labels

    #4 Margins
      #Get current margins
        mar.before =  par("mar")
        mar.after  =  mar.before
         

      #Only change margin if not the default (so users can set own in)
        mar.default = c(5.1, 4.1, 4.1, 2.1)
            max.x.label = max(nchar(unique(df[,stimulus])))
            xlabel.buffer = max(0,max.x.label)*.3
         
        if (all(mar.before==mar.default))
        {
        #4.1 Bottom
             mar.after[1] = mar.before[1] + xlabel.buffer
        
        #4.2 Top
          #Drop top margin if there is no main header
            mar.after[3] = ifelse ("main" %in% names(args),3,1)
        
        #4.3 Left
          width.y.label = nchar(max(y1))
           mar.after[2] = max(width.y.label/3, 5.1)
          if (ylab2!='') mar.after[2]= mar.after[2] + 1
          
        #4.4 Assign it
           par(mar=mar.after)
           
        } 
           
  #5 black dots
       n=length(y1)
       plot(y1,pch=16,ylim=ylim,xaxt='n',xlab='',las=1,ylab='',xlim=c(1,n+3),cex=1.5, ...)
       
  #6 Segments
    if (matched==TRUE)
    {
      
    e=mean(means.obs$effect)    
    lty=ifelse( (y1 - y2)*(m1-m2)>0 ,1,2)
    col=ifelse( (y1 - y2)*(m1-m2)>0 ,col1, col2)
    segments(x0=1:n, x1=1:n,y0=y1, y1=y2,lty=lty,col=col)
    }
   
  #7 White dots   
      points(y2,pch=21,col='black',bg='white',cex=1.5)
 
  #8 Redo black dots to cover any red lines
    if (matched==TRUE) points(y1,pch=16,cex=1.5)
    
  #9 Value labels
    
 
    #Color for text
      col.h = ifelse(bh==y1,adjustcolor(col1,.5),adjustcolor(col2,.5))
      col.l = ifelse(bl==y2,adjustcolor(col2,.5),adjustcolor(col1,.5))

     
    #Labels themselves
      text(1:n,bh,round2(bh,auto.decimals(bh)),col=col.h,cex=.65,pos=3)
      text(1:n,bl,round2(bl,auto.decimals(bl)),col=col.l,cex=.65,pos=1)
   
  #10 Overall means
      if (matched==TRUE) segments(x0=n+3, x1=n+3,y0=m1, y1=m2)
    points(n+3,m1,pch=16,cex=1.5*1.5) 
    points(n+3,m2,pch=21,cex=1.5*1.5,col='black',bg='white')
    axis(side=1,at=n+3,"MEAN",font=2)
    
    
    d.pos=c(3,1)
    if (m1<m2) d.pos=rev(d.pos)
    text(n+3,m1 , round2(m1,auto.decimals(m1)),cex=.8,col='gray50',pos=d.pos[1])
    text(n+3,m2 , round2(m2,auto.decimals(m1)),cex=.8,col='gray50',pos=d.pos[2])

  #11 Y axis
      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1,font=2,cex=1.2,ylab1)
      mtext(side=2,line=mar.after[2]-2,font=3,cex=1,ylab2)
      }

    
  #12 X-axis
    
    #12.1 Stimuli labels
        if (matched==TRUE) text(1:n,par('usr')[3] , paste0(means.obs[,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
        if (matched==FALSE) {
          text(1:n-.25,par('usr')[3] , paste0(means.obs[1:n ,stimulus],"  "),srt=80,xpd=TRUE,adj=1)
          text(1:n+.25,par('usr')[3] , paste0(means.obs[(n+1):(2*n) , stimulus],"  "),srt=80,xpd=TRUE,adj=1,font=2)
          }

    #12.2 Headers
        if (xlab2=="" & matched==TRUE  & sort.by=='') xlab2='(sorted by effect size)'
        if (xlab2=="" & matched==FALSE & sort.by=='') xlab2='(sorted by raw means)'
        if (xlab2=="" & sort.by!='')                  xlab2=paste0('(sorted by ',sort.by,')')

            #For matched stimuli, the default is the above text
        
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)

        
  #13 Legend
        labels=c(label1,label2)
        if (flip.condition) labels=rev(labels)
        leg1 = legend('topleft',pch=c(16,1), labels ,inset=.03,bty='n',cex=1.2)
        
        #Legend title?
        if (legend.title!='')
        {
        title.y = par("usr")[4] - (par("usr")[4] - leg1$rect$top)*.75
        title.x = leg1$rect$left
        text(title.x,title.y,legend.title,adj=0,font=2)
        }
    
  #14 Return margins to where they were
    par(mar=mar.before)
    
    return(means.obs)
  
  }
  