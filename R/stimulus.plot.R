#'Make stimulus plots as in the article "Stimulus Sampling Reimagined" by Simonsohn, Montealegre, & Evangelidis (2024) 
#' 
#'@param plot.type can be either "means" or "effects", determines what's plotted in the y-axis of the figure
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
#'@param participant name of the variable containing participant IDs; necessary for valid inference when 
#'plot.type='effects' and each participant provided more than one observation
#'@param save.as filepath for saving figure. Must be .svg or .png file (optional)
#'@param sort.by name of variable to sort stimuli by. Defaults to sorting by observed effect size.
#'@param flip.conditions by default the condition labels are sorted alphabetically and the 2nd subtracted from the first. 
#'set flip.conditions=TRUE so that the first is subtracted from the second instead
#'@param model method used to compute overall average: (1) 'regression', (2) mixed-model with stimulus intercepts, 
#'and/or (3) mixed-model with random intercepts. If `participant` is provided, the regression clusters by participant
#'and the mixed models include participant random intercepts. Possible values: 'regression', 'intercepts', 'slopes', 'all'
#'@param overall.estimate scalar or vector of overall average effect if computed outside of `{stimulus}` (may not be set jointly with `model`)
#'@param overall.ci vector of confidence interval bounds for `overall.estimate` (may not be set jointly with `model`)
#'@param overall.p scalar or vector of p-value for overall average effect if computed outside of `{stimulus}` (may not be set jointly with `model`)
#'@param overall.label label to show in x-axis for overall averages, can be used in conjuction with `model` to over-rule
#'the default labels of 'regression','Random Intercepts', and 'Random Slopes'
#'@param ylab1,ylab2 labels on the y-axis (optional)
#'@param xlab1,xlab2 labels on the x-axis (optional)
#'@param decimals force number of decimals to show for value labels (optional)
#'@param dv.is.percentage if set to TRUE values of the dependent variable are formatted as percentages
#'@param legend.title text with title above legend (optional)
#'@param simtot number of simulations to rely on for estimating expected heterogeneity of 
#'observed effect size. Only needed if plot.type='effects' (defaults to 100)
#'@param watermark set to FALSE to not display {stimulus version} in bottom left of figure
#'@param seed used when resampling for the 'effects' plot, defaults to seed=2024 
#'@export
#4 stimulus.plot (Wrapper function)
  stimulus.plot = function(
                    plot.type='means',
                    data, dv, condition, stimulus, 
                    participant='',
                    save.as = '',
                    svg.width='',
                    svg.height='',
                    sort.by='',
                    flip.conditions=FALSE,
                    model=c(),
                    overall.estimate=c(),
                    overall.ci=c(),
                    overall.p=c(),
                    overall.label=c(),
                    ylab1='',
                    ylab2='',
                    xlab1='Stimuli',
                    xlab2='',
                    decimals='auto',
                    null.method='shuffle',
                    dv.is.percentage=FALSE,
                    legend.title='',
                    simtot=1000,
                    watermark = TRUE,
                    seed=2024,
                    ylim=c(),
                    main='',
                    ...
                    )
        {
  

#----[VALIDATION]------------------------------------------------------------------------
  
      args_passed <- as.list(match.call())[-1]  # Remove the function name

           
      #Ensure data is a data.frame
          if ("data.frame" %in% class(data)) data=data.frame(data)
          if (!"data.frame" %in% class(data)) exit("stimulus.plot() says: the argument data must be a data.frame, but '",dataname,"' is not a dataframe.")
          
          
          
          
      #Force to be data.frame type
          data=data.frame(data)
          
          
       #Grab the dataname
           # Convert the name of 'data' to a string
             dataname <- deparse(substitute(data))
          # Optional: Clean the string (if clean_string is necessary)
              dataname <- clean_string(dataname)  # Assuming clean_string outputs a character


          
                #Validate arguments type and length
        validate.stimulus.plot(plot.type,data,dv, condition, stimulus, 
                              save.as, svg.width,
                              svg.height, sort.by, flip.conditions,
                              model, overall.estimate, overall.ci,
                              overall.p, overall.label,
                              ylab1, ylab2, xlab1, xlab2,
                              decimals, null.method,
                              dv.is.percentage, legend.title,
                              simtot, watermark, seed, ylim, 
                              args_passed)
    
       #Validate dots
          f='stimulus::stimulus.plot'
          validate.dots(f,...) #see validate.R function 3
          
      #Variables names from the dataset
        dv        <- clean_string(deparse(substitute(dv)))
        condition <- clean_string(deparse(substitute(condition)))
        stimulus  <- clean_string(deparse(substitute(stimulus)))
        sort.by   <- clean_string(deparse(substitute(sort.by)))
        
      
     #Check data.frame has all the necessary variables
        validate.data(f, data, dv, condition, stimulus, sort.by,participant,dataname)  #validate.data.R
         
      #Colors
        
       
      #Keep only variables we will use to speed up calculations
        data=data[,names(data) %in% c('r',dv,condition,stimulus,participant)]
        
           
  #----[/preparation]------------------------------------------------------------------------
            
     #Drop missing values
          n1=nrow(data)
          data = data[!is.na(data[,stimulus]) & !is.na(data[,dv]) & !is.na(data[,condition]),]
          if (participant!='') data = data[!is.na(data[,participant]),]
          n2=nrow(data)
          if (n2<n1) message2('stimulus.plot() says: ',n1-n2,' observations were dropped because of missing values.')
          
    #Drop unmatched observations
           t = table(data[,stimulus],data[,condition])
           drop.stim = t[t[,1]==0 | t[,2]==0,]
           
           if (nrow(drop.stim)>0)
           {
           drop.stim.rows = data[,stimulus] %in% rownames(drop.stim)
           mean(drop.stim.rows)
           data=data[!drop.stim.rows,]
           n3=nrow(data)
           message2('stimulus.plot() says: ',n2-n3,' observations were dropped because their value of "',stimulus,'" appears in only one condition')
           
           }
          
    #If saving to svg or png: 
        if (save.as!='') {
      
          
          #File
              filename=save.as
              
          #Create directory if it does not exist
              dir.create(dirname(filename),recursive = TRUE,showWarnings = FALSE)
              
              
          #Get extension of file name
              extension= tools::file_ext(filename)
        
          #Width and height of file
              max.x.label = max(nchar(unique(data[,stimulus]))) #length of stimulus name
              ns = length(unique(data[,stimulus]))  #number of unique stimuli  
              nm = length(model)
              w  = 5+(ns+nm*1.5)*.4                   #Width
              h  = 5                              #height
              h  = h * (1 + max.x.label/40)
              
          #If svg weight or height specified
            if (svg.width!='')  w=svg.width
            if (svg.height!='') h=svg.height
              
          #start the figure
            if (extension=='svg') svg(filename , w,h)
            if (extension=='png') png(filename , w*1000,h*1000,res=1000)
          
         } #End if save.as()
          
      
          
        #Means
          if (plot.type=='means')
          {
            res=stimulus.plot.means(data=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    participant=participant,
                                    sort.by=sort.by,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    decimals=decimals,
                                    dv.is.percentage=dv.is.percentage,
                                    flip.conditions=flip.conditions,
                                    legend.title=legend.title,
                                    ylim=ylim,
                                    main=main,
                                    ...)
            
    
          }
    
        #Effects
          if (plot.type=='effects')
          {
             res=stimulus.plot.effects(
                                    data=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    dataname=dataname,
                                    model=model,
                                    overall.estimate=overall.estimate,
                                    overall.ci=overall.ci,
                                    overall.p=overall.p,
                                    overall.label=overall.label,
                                    participant=participant,
                                    sort.by=sort.by,
                                    flip.conditions=flip.conditions,
                                    null.method=null.method,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    dv.is.percentage=dv.is.percentage,
                                    simtot=simtot,
                                    decimals=decimals,
                                    ylim=ylim,
                                    seed=seed,
                                    main=main,
                                    ...)
             
                 
            }
          

        #Pkg version watermark
          if (watermark==TRUE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }
     
    
            
      if (save.as!='') dev.off()
        
      invisible(res)

  } #End of wrapper function
  
  