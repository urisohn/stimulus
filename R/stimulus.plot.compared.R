
stop('this is work in progress, unclear we really want it')

  stimulus.plot.compared=function(data,dv,stimulus,condition)
  {
    
    #Unique conditions
      uc=as.character(unique(data[,condition]))
      
      dc1=data[,condition]==uc[1]
      dc2=data[,condition]==uc[2]
          
    #Get Means and CIs
      m1=aggregate(data[dc1,dv],list(data[dc1,stimulus]),mean)
      m2=aggregate(data[dc2,dv],list(data[dc2,stimulus]),mean)
      
      sd1=aggregate(data[dc1,dv],list(data[dc1,stimulus]),sd)
      sd2=aggregate(data[dc2,dv],list(data[dc2,stimulus]),sd)

      n1=aggregate(data[dc1,dv],list(data[dc1,stimulus]),length)
      n2=aggregate(data[dc2,dv],list(data[dc2,stimulus]),length)

      df1=cbind(m1,sd1,n1)
      df2=cbind(m2,sd2,n2)
      
      df1=df1[,c(1,2,4,6)]
      df2=df2[,c(1,2,4,6)]
      names(df1)=names(df2)=c("stimulus","mean","sd","n")
      df1$se=df1$sd/sqrt(df1$n)
      df2$se=df2$sd/sqrt(df2$n)
      df1$ciL = df1$mean -  qt(.975,2*df1$n-2)*df1$se
      df1$ciH = df1$mean +  qt(.975,2*df1$n-2)*df1$se
      df2$ciL = df2$mean -  qt(.975,2*df2$n-2)*df2$se
      df2$ciH = df2$mean +  qt(.975,2*df2$n-2)*df2$se
      
      df1=df1[order(df1$mean),]
      df2=df2[order(df2$mean),]
      
   
      y1=df1$mean
      y2=df2$mean   
      cis=c(df1$ciL,df1$ciH,df2$ciL,df2$ciH)
      k1=length(y1)
      k2=length(y2)
   
      col1='dodgerblue'
      col2='red4'
      
    #Plot
      plot  (y1 , pch=16,ylim=range(cis),cex=1.5,xlab='',xaxt='n',las=1,ylab='',col=col1)
      points(y2 , pch=16,cex=1.5,col=col2)
      
      
    #Ci
      segments(x0 = 1:k1+.03, x1 = 1:k1+.03, y0=df1$ciL,y1=df1$ciH,col=adjustcolor(col1,.5))
      segments(x0 = 1:k1-.03, x1 = 1:k1-.03, y0=df2$ciL,y1=df2$ciH,col=adjustcolor(col2,.5))
      
      
    #Stimulus labels
      axis(side=1,line=0,at=1:k1 , df1$stimulus ,tick=FALSE)
      axis(side=1,line=1,at=1:k2 , df2$stimulus ,tick=FALSE)
      
      xpd=par('xpd')
      par(xpd=TRUE)
      axis(side=1,line=0,at=-.5,uc[1],tick=FALSE,font=2)
      axis(side=1,line=1,at=-.5,uc[2],tick=FALSE,font=2)
      par(xpd=xpd)
      
   #Value labels
      pos1=1
      pos2=3
      if (mean(y1)>mean(y2)) {
        pos1=3
        pos2=1
      }
      text(1:k1+.1,y1,round2(y1,auto.decimals(y1)),col=col1,cex=.75,pos=pos1)
      text(1:k2+.1,y2,round2(y2,auto.decimals(y2)),col=col2,cex=.75,pos=pos2)
         
    #x-axis
       if (xlab2=="" & matched==TRUE  & sort.by=='') xlab2='(sorted by effect size)'
        if (xlab2=="" & matched==FALSE & sort.by=='') xlab2='(sorted by raw means)'
        if (xlab2=="" & sort.by!='')                  xlab2=paste0('(sorted by ',sort.by,')')

            #For matched stimuli, the default is the above text
        
        mtext(side=1,line=2.7 + xlabel.buffer , font=2,cex=1.2,xlab1)
        mtext(side=1,line=3.7 + xlabel.buffer   ,font=3,cex=1,xlab2)
      
      
      
  }