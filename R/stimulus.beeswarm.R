#'Make beeswarm plots for stimuli in compared-stimulus design (see "Stimulus Sampling Reimagined" by Simonsohn, Montealegre, & Evangelidis (2024))
#' 
#'@param data dataframe containing variables to be analyzed
#'@param dv name of the dependent variable (e.g., dv='y'), quotes are not required
#'@param condition name of the variable containing the condition indicator 
#'(e.g., condition='cond'), quotes are not required. The x-axis label will use values
#'in this variable to identify the two conditions. If you want to customize the legend 
#'(e.g., so that it is not "0" vs "1"), create new variable, say df$cond2 
#'with descriptive values for condition (e.g., df$cond2=ifelse(df$cond==1,'control','treatment'))
#'and then use condition='cond2' in the stimulus.plot call.
#'@param stimulus name of the variable containing the stimulus ID 
#'(e.g., stimulus='stim_id'), quotes are not required. The 'markers' in the beeswarm
#'will be the text for this labels, if you want the markers to be different 
#'(e.g., abbreviations) or single word summaries, create a new variable with those 
#'values and use that in the stimulus.beeswarm() call
#'@param dv.is.percentage if set to TRUE values of the dependent variable 
#'are formatted as percentages
#'@param simtot integer value for number of bootstraps used to build a confidence band
#'for the range of means under homogeneity
#'@param confidence integer value for confidence level for confidence band. Defaults to 95.
#'@param save.as filepath for saving figure. Must be .svg or .png file (optional)
#'@param flip.conditions by default the condition labels are sorted alphabetically.  
#'Set flip.conditions=TRUE to reverse the order
#'@param main figure name, same as base-R plot main argument
#'@param ylab1,ylab2 labels on the y-axis (optional)
#'@param dv.is.percentage if set to TRUE values of the dependent variable are formatted as percentages
#'@param dot.spacing scalar for horizontal distance between labels with stimuli names
#'@param col1,col2 strings with colors for first and second condition in the plot 
#'(lighter versions of colors are automatically generated)
#'@param watermark set to FALSE to not display {stimulus version} in bottom left of figure
#'
#'@export
  stimulus.beeswarm=function(data,  dv, stimulus, condition, 
                              flip.conditions=FALSE, 
                              dv.is.percentage=FALSE,
                              simtot=500,
                              confidence=95,
                              ylim=c(),
                              ylab1='',
                              ylab2='',
                              xlab1='',
                              xlab2=NULL,
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
  #2  Bootstrap CI of stimuli
  #3  Set figure parameters
  #4  Make the beeswarm with markers with {beeswarm}
  #5  Start svg if requested
  #6  Margins
  #7  Beeswarm with stimulus labels 
  #8  Y axis
  #9  Header
  #10 Bootstrapped confidence band for the set of values
  #11 Pkg version watermark
  #12 Saved file feedback
  #13 Invisible return 
        
  
  #------------------------------------------------------------------

        args = list(...)
    
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
      

   #2 Bootstrap CI of stimuli (load from cach eif it exists)
      #Search cache using md5 of the arguments
          dataname  <- clean_string(deparse(substitute(data)))
          md5k = get.md5(list(dataname,dv,stimulus,condition,simtot))

      #if resample exist,  load it
        if (does.cache.d.exist(md5k))   #see utils.R
          {
          maxmin_boot = .GlobalEnv$.stimulus.cache[[md5k]]
          message2("*Recycled results*:\n",
                  "You had run this same analysis before with all the same variables and options.\n",
                  "(data='",dataname,"' | dv='",dv,"' | stimulus='",stimulus,"' | condition='",condition,"' | simtot='",simtot,"')\n",
                  "To save time, we are re-using saved results. To force new calculations\n",
                  "change one of those parameters or clear your cache running: 'clear_stimulus_cache()'")
                    
       #Else run it                  
            } else  {
                  maxmin_boot =  get.maxmin.confidence(data,  dv, stimulus, condition, simtot,confidence,ms1,ms2,dc1,dc2)
                   
                  
            #Save
               .GlobalEnv$.stimulus.cache[[md5k]]=maxmin_boot
        } #End else
        
  #3 Set figure parameters
      if (dot.spacing=='auto')
      {
        stimulus.length = mean(nchar(ms$stimulus))
        dot.spacing = stimulus.length/3 + 2
      }
      
      col1a=adjustcolor(col1,.75)
      col2a=adjustcolor(col2,.75)
      
          
  #4 Make the beeswarm with markers with {beeswarm}
      b = beeswarm::beeswarm(ms$mean~ms$condition, spacing = dot.spacing , 
                             do.plot=F,method='swarm')

  
  #5 Start svg if requested
     if (save.as!='') {
        
          #File
              filename=save.as
              
          #Get extension of file name
              extension= tools::file_ext(filename)
        
          #Width and height of file
              w  = 9
              h  = 7
          
              
          #If svg weight or height specified
            if (svg.width!='')  w=svg.width
            if (svg.height!='') h=svg.height
              
          #start the figure
            if (extension=='svg') svg(filename , w,h)
            if (extension=='png') png(filename , w*1000,h*1000,res=1000)
          
         } #End if save.as()
          
      
       
  #6 Margins
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
        
  #7 Beeswarm with stimulus labels 
      if (length(ylim)<2) {
          ylim = range(ms$mean)
          dy = diff(ylim)
          ylim[2]=ylim[2]+.25*dy  #Give a  buffer on top (for the legend)
          #ylim[1]=ylim[1]-.03*dy  #give 
          }
        
      plot(b$x,b$y,pch=NA,
           xlim=c(min(.75,b$x), max(2.25,b$x)),
           xlab='',
           ylab='',
           las=1,
           yaxt=ifelse(dv.is.percentage,'n','s'),  #no y-ticks if percentage
           xaxt='n',
           ylim=ylim,
           ...)
        
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
        #xs
          xt1=min(b1$x , 0.85)
          xt2=max(b1$x , 1.15)
          xt3=min(b2$x , 1.85)
          xt4=max(b2$x , 2.15)
      
      
        #Compute
          mean1 = mean(data[data[,condition]==uc[1],dv],na.rm=TRUE)
          mean2 = mean(data[data[,condition]==uc[2],dv],na.rm=TRUE)
          
        #Lines
          segments(x0=xt1 , x1=xt2, y0=mean1, y1=mean1,col=col1,lwd=3)
          segments(x0=xt3 , x1=xt4 ,y0=mean2, y1=mean2,col=col2,lwd=3)
        
       #value labels
          if (dv.is.percentage==FALSE)
          {
            rm1=round_smart(mean1)
            rm2=round_smart(mean2)
          }  else {
            rm1=format_percent(mean1)
            rm2=format_percent(mean2)
          }
          
            text(xt2,mean1,paste0("M=",rm1),col=col1,cex=1,pos=4)
            text(xt3,mean2,paste0("M=",rm2),col=col2,cex=1,pos=2)
          
    
#8 Bootstrapped confidence band for the set of values
            b1L=maxmin_boot$b1L
            b1H=maxmin_boot$b1H
            b2L=maxmin_boot$b2L
            b2H=maxmin_boot$b2H
            
            polygon(x=c(xt1 , xt1 , xt2 , xt2),y=c(b1L, b1H, b1H, b1L),col=adjustcolor(col1,.1),border = NA)
            polygon(x=c(xt3 , xt3 , xt4 , xt4),y=c(b2L, b2H, b2H, b2L),col=adjustcolor(col2,.1),border = NA)
            
            
            
 #9 Y axis
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
    
            
  #10 Header
      if (main!='') mtext(side=3,line=1,font=2,cex=1.65,main)
      
  #11 Pkg version watermark
          if (watermark==TRUE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }

  #12 Legend
      #Use as example label for highest stimulus in Condition 1
        kmax=which.max(ms1$mean)
        stimulus.max = ms1$stimulus[kmax]
            
      #Legend
        
        
      legend('top',
             pch=c("Æ",NA,NA,NA),
             bty='n',
             lty=c(NA,1,1,NA),
             lwd=c(NA,3,20,NA),
             legend=c(
                      paste0('Stimulus label, e.g., "',stimulus.max,'"'), 
                      'Mean across stimuli', 
                      paste0(confidence,"% confidence band for Max-to-Min stimulus range"),
                      paste0("Based on ",simtot," resamples under null of equal distributions")
                      ),
             cex=1,
             col=c(col1a,col1,adjustcolor(col1,.1))
       )
            
  #12 Saved file feedback
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
                      
        
  #13 Return
     invisible(b[,1:2])
  }
        
  
  

  