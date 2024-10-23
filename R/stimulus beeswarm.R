#'Make beeswarm plots for stimuli in compared-stimulus design (see "Stimulus Sampling Reimagined" by Simonsohn, Montealegre, & Evangelidis (2024))
#' 
#'@param data dataframe containing variables to be analyzed
#'@param dv name of the dependent variable (e.g., dv='y'), quotes are not required
#'@param condition name of the variable containing the condition indicator 
#'(e.g., condition='cond'), quotes are not required. Figure legend will use values
#'in this variable to identify the two conditions. If you want to customize the legend 
#'(e.g., so that it is not "0" vs "1"), create new variable, say df$cond2 
#'with descriptive values for condition (e.g., df$cond2=ifelse(df$cond==1,'control','treatment'))
#'and then use condition='cond2' in the stimulus.plot call.
#'@param stimulus name of the variable containing the stimulus ID 
#'(e.g., stimulus='stim_id'), quotes are not required. The x-axis will have these values as tick-lables.
#'If you want to customize them, e.g., so that they have descriptive labels instead of numeric ID1, ID2...,
#'create a new variable with those descriptions and use that variable as the stimulus variable in the call
#'(e.g., df$item.id =c(1,2,3,4,5,...) --> df$item.id2=c('Chair','Airplane','Fries','Dalmatian','Coat',...))
#'@param save.as filepath for saving figure. Must be .svg or .png file (optional)
#'@param flip.conditions by default the condition labels are sorted alphabetically.  
#'Set flip.conditions=TRUE to reverse the order
#'@param main figure name, same as base-R plot main argument
#'@param ylab1,ylab2 labels on the y-axis (optional)
#'@param xlab1 labels on the x-axis, centered, by default it is 'Condition' (optional)
#'@param xlab2 vectro of size 2 with labels for the two conditions (optional)
#'@param dv.is.percentage if set to TRUE values of the dependent variable are formatted as percentages
#'@param dot.spacing horizontal distance between labels with stimuli names
#'@param watermark set to FALSE to not display {stimulus version} in bottom left of figure
#'
#'@export
  stimulus.beeswarm=function(data,  dv, stimulus, condition, 
                              flip.conditions=FALSE, 
                              dv.is.percentage=FALSE,
                              ylab1='',
                              ylab2='',
                              xlab1='',
                              xlab2=NULL,
                              ylim=c(),
                              dot.spacing='auto',
                              col1='blue4',
                              col2='red4',
                              main='',
                              watermark=TRUE,
                              save.as='',
                              svg.width  ='',
                              svg.height ='',
                              ...)
    
  {
    
#outline
  #1  Compute means by stimulus and put in data.frame
  #2  Set figure parameters
  #3  Make the beeswarm with markers with {beeswarm}
  #4  Start svg if requested
  #4  Margins
  #5  Beeswarm with stimulus labels 
  #6  Y axis
  #7  Header
  #8  Pkg version watermark
  #9  Saved file feedback
  #10 Return 
        
  
  #------------------------------------------------------------------

    
    
  #1 Compute means by stimulus and put in data.frame
      #Get the condition names
        uc=sort(as.character(unique(data[,condition])),decreasing=flip.conditions)
        
      #Subset dasta into two conditions
        dc1=data[,condition]==uc[1]
        dc2=data[,condition]==uc[2]
            
      #Get Means and CIs
        ms1=aggregate(data[dc1,dv],list(data[dc1,stimulus]),mean)
        ms2=aggregate(data[dc2,dv],list(data[dc2,stimulus]),mean)
        
      #Add condition to summary
        ms1$condition=uc[1]
        ms2$condition=uc[2]
        
     #Name the columns
        names(ms1)=names(ms2)=c("stimulus",'mean','condition')
        ms=rbind(ms1,ms2)
      

  #2 Set figure parameters
      if (dot.spacing=='auto')
      {
        stimulus.length = mean(nchar(ms$stimulus))
        dot.spacing = stimulus.length/3 + 1.5
      }
      
      col1a=adjustcolor(col1,.75)
      col2a=adjustcolor(col2,.75)
      
          
  #3 Make the beeswarm with markers with {beeswarm}
      b = beeswarm::beeswarm(ms$mean~ms$condition, pch=16,col=c(col1, col2),cex=1.5,xlab='',
                 las=1,ylab='',spacing = dot.spacing , do.plot=F,method='compactswarm')
      print(b)
  #4 Start svg if requested
     if (save.as!='') {
        
          #File
              filename=save.as
              
          #Get extension of file name
              extension= tools::file_ext(filename)
        
          #Width and height of file
              w  = 8
              h  = 6
          
              
          #If svg weight or height specified
            if (svg.width!='')  w=svg.width
            if (svg.height!='') h=svg.height
              
          #start the figure
            if (extension=='svg') svg(filename , w,h)
            if (extension=='png') png(filename , w*1000,h*1000,res=1000)
          
         } #End if save.as()
          
      
       
  #5 Margins
      #Get current margins
        mar.before =  par("mar")  #margins right now
        mar.after  =  mar.before  #will set back to this on exit
         
      #Only change margins if they are the default (so users can deviate from ours and we don't over-ride)
        custom_mar <- getOption("graphics.par")$mar                    #Rprofile margins? (unlikely)
        if (!is.null(custom_mar)) mar.default = custom_mar
        if (is.null(custom_mar))  mar.default = c(5.1, 4.1, 4.1, 2.1)  #common default
        
        
      #Adjust margins here
        if (all(mar.before==mar.default))
        {
        #4.1 Top
          #Drop top margin if there is no main header
            mar.after[3] = ifelse (main=='',1,3)
            
        #4.3 Left
           width.y.label = nchar(max(pretty(ms$mean)))
           mar.after[2] = max(width.y.label/3.5, 5)
           if (ylab2!='') mar.after[2]= mar.after[2] + 1
           if (dv.is.percentage==TRUE) {
             
             mar.after[2] = mar.after[2] + 1
             
           }
          
        #4.4 Assign it
           par(mar=mar.after)
           on.exit(par(mar=mar.before))    #return to default on exit
        } 
        
  #6 Beeswarm with stimulus labels 
      plot(b$x,b$y,pch=NA,
           xlim=c(min(.75,b$x), max(2.25,b$x)),
           xlab='',
           ylab='',
           las=1,
           yaxt=ifelse(dv.is.percentage,'n','s'),  #no y-ticks if percentage
           xaxt='n',
           ...)
      
        
        x=c(1,2,3)
        y=c(3,3,3)
        
        
      #Split the data
        b1=b[b$x<=1.5,]
        b2=b[b$x>1.5,]
        
      #The markers of words with stimulus labels
        text(b1$x,b1$y,ms[ms$mean==b1$y.orig,'stimulus'] ,cex=.65,col=col1a)
        text(b2$x,b2$y,ms[ms$mean==b2$y.orig,'stimulus'] ,cex=.65,col=col2a)
      
      
      
    #X-axis
        
      if (xlab1=='')      xlab1=condition
      if (is.null(xlab2)) xlab2=uc
        
      mtext (side=1,font=2,xlab1,line=3,cex=1.65)
      mtext(side=1,at=c(1,2),line=1.5,xlab2,cex=1.25,font=2)
      
      
    #means 
        #Compute
          mean1 = mean(data[data[,condition]==uc[1],dv],na.rm=TRUE)
          mean2 = mean(data[data[,condition]==uc[2],dv],na.rm=TRUE)
          
        #Lines
          segments(x0=.85,x1=1.15, y0=mean1, y1=mean1,col=col1,lwd=3)
          segments(x0=1.85,x1=2.15,y0=mean2, y1=mean2,col=col2,lwd=3)
        
       #value labels
          if (dv.is.percentage==FALSE)
          {
            rm1=round_smart(mean1)
            rm2=round_smart(mean2)
          }  else {
            rm1=format_percent(mean1)
            rm2=format_percent(mean2)
          }
          
          
            text(1.25,mean1,paste0("M=",rm1),col=col1,cex=1)
            text(1.85,mean2,paste0("M=",rm2),col=col2,cex=1,pos=2)
          
    
 #7 Y axis
      if (ylab1=='') ylab1=dv
        
    
      if (!"yaxt" %in% names(args))
      {
      mtext(side=2,line=mar.after[2]-1.8,font=2,cex=1.65,ylab1)
      mtext(side=2,line=mar.after[2]-2.8,font=3,cex=1.25,ylab2,col='gray30')
      }

    
      if (dv.is.percentage==TRUE)
      {
          ys=pretty(ms$mean)
          if (dv.is.percentage==TRUE) axis(side=2,at=ys,paste0(ys*100,"%"),las=1)
      }
    
            
  #8 Header
      if (main!='') mtext(side=3,line=1,font=2,cex=1.65,main)
      
  #9 Pkg version watermark
          if (watermark==TRUE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }

  #10 Saved file feedback
      if (save.as!='')
        {
        message2("\nFigure was saved as '", save.as,"'")
        if (svg.width=="" & svg.height=="") {
            message2(paste0(
            "NOTE: We used default width=",w,", and height=",h,", customize with svg.width & svg.height arguments."))
            }
            
         #Close the graph
            dev.off()
           
          }
                      
        
  #11 Return
            
     invisible(b[,1:2])
  }
        
  
  
