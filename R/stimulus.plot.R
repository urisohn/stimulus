#'Make stimulus plots as in "Stimulus Sampling Reimagined", Simonsohn, Montealegre, & Evangelidis (2024) 
#' 
#'@param plot.type can be either "means" or "effects", determines what's plotted in the y-axis of the figure
#'@param data dataframe containing variables to be analyzed
#'@param dv name of the dependent variable (e.g., dv='y'), quotes are not required
#'@param condition name of the variable containing the condition indicator 
#'(e.g., condition='treatment'), quotes are not required. Figure legend will use values
#'in 'condition' create new column with descriptive values if necessary
#' (e.g., cond=c(1,0),-->condition=c('Treatment','Controol'))
#'@param stimulus name of the variable containing the stimulus ID 
#'(e.g., stimulus='stim_id'), quotes are not required. The x-axis labels will use values
#'in 'stimulus', create new column with descriptive values and use that in stimulus.plot() if desired  
#'(e.g., df$item.id =c(1,2,3,4,5,...) --> df$item.id2=c('Chair','Airplace','Fries','Dalmatian','Coat',...))
#'@param participant name of the variable containing participant IDs; necessary for valid inference when 
#'plot.type='effects' and each participant provided more than one observation
#'@param save.as filepath for saving figure. Must be .svg or .png file (optional)
#'@param sort.by name of variable to sort stimuli by. Defaults to sorting by observed effect size.
#'@param flip.condition reverse order in which conditions are compared? 
#'(e.g., treatment-control instead of control-treatment). Defaults to FALSE
#'@param model method used to compute overall average: (1) 'regression', (2) mixed-model with stimulus intercepts, 
#'and/or (3) mixed-model with random intercepts. If `participant` is provided, the regression clusters by participant
#'and the mixed models include participant random intercepts. Possible values: 'regression', 'intercepts', 'slopes', 'all'
#'@param overall.estimate scalar or vector of overall average effect if computed outside of `{stimulus}` (may not be set jointly with `model`)
#'@param overall.ci vector of confidence interval bounds for `overall.estimate` (may not be set jointly with `model`)
#'@param overall.p scalar or vector of p-vauue for overall average effect if computed outside of `{stimulus}` (may not be set jointly with `model`)
#'@param overall.label label to show in x-axis for overall averages, can be used in conjuction with `model` to over-rule
#'the default labels of 'regression','Random Intercepts', and 'Random Slopes'
#'@param ylab1,ylab2 labels on the y-axis (optional)
#'@param xlab1,xlab2 labels on the x-axis (optional)
#'@param decimals force number of decimals to show for value labels (optional)
#'@param dv.is.percentage if set to TRUE values are formatted as percentages
#'@param legend.title text above legend (optional)
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
                    sort.by='',
                    flip.condition=FALSE,
                    model=c(),
                    overall.estimate=c(),
                    overall.ci=c(),
                    overall.p=c(),
                    overall.label=c(),
                    ylab1='',
                    ylab2='',
                    xlab1='Stimuli',
                    xlab2='',
                    label.low='',
                    label.high='',
                    decimals='auto',
                    dv.is.percentage=FALSE,
                    legend.title='',
                    simtot=100,
                    watermark = TRUE,
                    seed=2024,
                 
                    ...
                    )
        {
  

    
     #Required values entered
        if (missing(data)) exit("stimulus.plot() says: you must specify a dataframe")
        if (missing(dv)) exit("stimulus.plot() says: you must specify the dependent variable ('dv')")
        if (missing(condition)) exit("stimulus.plot() says: you must specify the condition variable ('condition')")
        if (missing(stimulus)) exit("stimulus.plot() says: you must specify the stimulus variable ('stimulus')")     
    
      #Grab the name
          dataname  <- clean_string(deparse(substitute(data)))
      
      #Ensure data is a data.frame
          if ("data.frame" %in% class(data)) data=data.frame(data)
          if (!"data.frame" %in% class(data)) exit("stimulus.plot() says: the argument data must be a data.frame, but '",dataname,"' is not a dataframe.")
  
      #Check arguments are set and of the right type
          validate.arguments(data, dv, condition, stimulus, sort.by, plot.type, 
                              flip.condition, ylab1, ylab2, xlab1, xlab2, 
                               decimals,
                              participant, legend.title,simtot,
                              dataname,model,    
                              overall.estimate, overall.ci,overall.p,overall.label)
            
      #Variables names from the dataset
        dv        <- clean_string(deparse(substitute(dv)))
        condition <- clean_string(deparse(substitute(condition)))
        stimulus  <- clean_string(deparse(substitute(stimulus)))
        sort.by   <- clean_string(deparse(substitute(sort.by)))
        
      
     #Check data.farme has all the necessary variables
        validate.data(data, dv, condition, stimulus, sort.by,participant,dataname)
                     
        
        #Drop missing values
          n1=nrow(data)
          data = data[!is.na(data[,stimulus]) & !is.na(data[,dv]) & !is.na(data[,condition]),]
          if (participant!='') data = data[!is.na(data[,participant]),]
          n2=nrow(data)
          if (n2<n1) message('stimulus.plot() says:\nA total of ',n1-n2,' observations were dropped because of missing values.')
          
          
      
          
        #Means
          if (plot.type=='means')
          {
            res=stimulus.plot.means(df=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    participant=participant,
                                    sort.by=sort.by,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    decimals=decimals,
                                    flip.condition=flip.condition,
                                    legend.title=legend.title,...)
            
    
          }
    
        #Effects
          if (plot.type=='effects')
          {
             res=stimulus.plot.effects(
                                    df=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    dataname=dataname,
                                    model=model,
                                    overall.estimate=overall.estimate,
                                    overall.ci=overall.ci,
                                    overall.p=overall.p,
                                    overall.label=overall.label,
                                    participant=participant,
                                    sort.by=sort.by,
                                    flip.condition=flip.condition,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    dv.is.percentage=dv.is.percentage,
                                    simtot=simtot,
                                    decimals=decimals,
                                    seed=seed,
                                    ...)
             
                 
            }
          

        #Pkg version watermark
          if (watermark==TRUE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }
     
      #If saving to svg or png: 
          
        if (save.as!='') {
      
          #File
              filename=save.as
              
          #Get extension of file name
              extension= tools::file_ext(filename)
        
          #Width and height of file
              max.x.label = max(nchar(unique(data[,stimulus]))) #length of stimulus name
              ns = length(unique(data[,stimulus]))  #number of unique stimuli  
              nm = length(model)
              w  = 5+(ns+nm*1.5)*.4                   #Width
              h  = 5                              #height
              h  = h * (1 + max.x.label/40)
               
              
          #Grab the figure that has been created
            figure_displayed <- recordPlot()
        
         
              
          #start the figure
            if (extension=='svg') svg(filename , w,h)
            if (extension=='png') png(filename , w*1000,h*1000,res=1000)
            
         #Actually save it to svg
            replayPlot(figure_displayed)

          #Feedback
            message("\n\nFigure was saved as '", save.as,"'\n")
          
          #Close the graph
            dev.off()
           
       
            
        }#End save as 
        
      invisible(res)

  } #End of wrapper function
  
  