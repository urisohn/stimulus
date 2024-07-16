#'Make stimulus plots as in "Stimulus Sampling Reimagined", Simonsohn, Montealegre, & Evangelidis (2024) 
#' 
#'@param data dataframe containing variables to be analyzed
#'@param dv name of the dependent variable (e.g., dv='y'), quotes are not required
#'@param condition name of the variable containing the condition indicator (e.g., condition='treatment'), quotes are not required
#'@param stimulus name of the variable containing the stimulus ID (e.g., stimulus='stim_id'), quotes are not required
#'@param sort.by name of the variable to sort stimuli by in the plots. Defaults to sorting by observed effect size.
#'May be be useful to deviate to explore moderators or sort stimuli alphabetically.
#'@param participant name of the variable containing participant IDs; if set it's entered as random participant effect
#' (character, optional)
#'@param plot.type can be either "means" or "effects", determines what's plotted in the y-axis of the figure
#'@param flip.condition whether to sort effect size in reverse order (TRUE/FALSE, defaults to FALSE)
#'@param ylab1,ylab2 labels on the y-axis (character, optional)
#'@param xlab1,xlab2 labels on the x-axis (character, optional)
#'@param decimals how many decimals to depict in the graph (integer)
#'@param legend.title text above legend (character)
#'@param simtot number of simulations to rely on for estimating expected heterogeneity of 
#'observed effect size. Only needed if plot.type='effects' (defaults to 100)
#'@param supress.version.label set to TRUE to prevent version of {stimulus} package used
#'to generate figure from appearing in the bottom left of the figure
#'@param save.as name of file to save figure as (optional, filename must have extension .svg or .png)
#' @export
#4 stimulus.plot (Wrapper function)
  stimulus.plot = function(
                    data, dv, condition, stimulus, 
                    sort.by='',
                    plot.type='means',
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
                    cex=1.5,
                    value.labels.offset = -1,
                    stimuli.numeric.labels=FALSE,
                    label.low='',
                    label.high='',
                    decimals=2,
                    participant='',
                    legend.title='',
                    simtot=100,
                    watermark = TRUE,
                    save.as = '',
                 
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
                              flip.condition, ylab1, ylab2, xlab1, xlab2, value.labels.offset,
                              stimuli.numeric.labels, label.low, label.high, decimals,
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
          
          
       #Save?
           if (save.as!='') {
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
               
          #start the figure
            if (extension=='svg') svg(filename , w,h)
            if (extension=='png') png(filename , w*1000,h*1000,res=1000)
             }         
          
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
                                    value.labels.offset=value.labels.offset,
                                    stimuli.numeric.labels=stimuli.numeric.labels,
                                    label.low=label.low, label.high=label.high,
                                    decimals=decimals,
                                    legend.title=legend.title,col1,col2,...)
            
    
          }
    
        #Effects
          if (plot.type=='effects')
          {
             res=stimulus.plot.effects(
                                    df=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    model=model,
                                    overall.estimate=overall.estimate,
                                    overall.ci=overall.ci,
                                    overall.p=overall.p,
                                    overall.label=overall.label,
                                    participant=participant,
                                    sort.by=sort.by,
                                    flip.condition=flip.condition,
                                    label.high=label.high, 
                                    label.low=label.low,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    cex=cex,
                                    value.labels.offset=value.labels.offset,
                                    stimuli.numeric.labels=stimuli.numeric.labels,
                                    simtot=simtot,
                                    decimals=decimals,...)
             
                 
            }
          

        #Pkg version watermark
          if (watermark==TRUE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }
     
      #If saving to svg or png: 
          
        if (save.as!='') {
          #Feedback
            message("Figure was saved as '", save.as,"'")
          
          #Close the graph
            dev.off()
           
          #Redraw on window
            call <- match.call()    #Get all arguments in the call
            call$save.as <- ""      #Replace save.as
            eval(call)              #Call it
        }#End save as 
        
      invisible(res)

  } #End of wrapper function
  
  