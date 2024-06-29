#'Make stimulus plots as in "Stimulus Sampling Reimagined", Simonsohn, Montealegre, & Evangelidis (2024) 
#' 
#'@param data dataframe containing variables to be analyzed
#'@param dv name of the dependent variable (e.g., dv='y'), quotes are not required
#'@param condition name of the variable containing the condition indicator (e.g., condition='treatment'), quotes are not required
#'@param stimulus name of the variable containing the stimulus ID (e.g., stimulus='stim_id'), quotes are not required
#'@param moderator name of the variable that may moderate effect across stimuli (character, optional)
#'@param participant name of the variable containing participant IDs; if set it's entered as random participant effect
#' (character, optional)
#'@param plot.type can be either "means" or "effects", determines what's plotted in the y-axis of the figure
#'@param flip.sign whether to sort effect size in reverse order (TRUE/FALSE, defaults to FALSE)
#'@param ylab1,ylab2 labels on the y-axis (character, optional)
#'@param xlab1,xlab2 labels on the x-axis (character, optional)
#'@param decimals how many decimals to depict in the graph (integer)
#'@param legend.title text above legend (character)
#'@param simtot number of simulations to rely on for estimating expected heterogeneity of 
#'observed effect size. Only needed if plot.type='effects' (defaults to 100)
#'@param supress.version.label set to TRUE to prevent version of {stimulus} package used
#'to generate figure from appearing in the bottom left of the figure
#'@param filename path to save figure as an image (optional, character, must has either .svg or .png extension)
#' @export
#4 stimulus.plot (Wrapper function)
  stimulus.plot = function(data, dv, condition, stimulus, 
                    moderator,
                    plot.type='means',
                    flip.sign=FALSE,
                    ylab1='',
                    ylab2='',
                    xlab1='Stimuli',
                    xlab2='',
                    value.labels.offset = -1,
                    stimuli.numeric.labels=FALSE,
                    label.low='',
                    label.high='',
                    decimals=2,
                    participant='',
                    legend.title='',
                    simtot=100,
                    suppress.version.label=FALSE,
                    filename='',
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
          validate.arguments(data, dv, condition, stimulus, moderator, plot.type, flip.sign,
                  ylab1, ylab2, xlab1, xlab2, value.labels.offset,
                  stimuli.numeric.labels, label.low, label.high, decimals,participant, legend.title,simtot,
                  dataname)
            
      #Variables names from the dataset
        dv        <- clean_string(deparse(substitute(dv)))
        condition <- clean_string(deparse(substitute(condition)))
        stimulus  <- clean_string(deparse(substitute(stimulus)))
        moderator <- clean_string(deparse(substitute(moderator)))
      
     #Check data.farme has all the necessary variables
        validate.data(data, dv, condition, stimulus, moderator,participant,dataname)
        
        
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
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    value.labels.offset=value.labels.offset,
                                    stimuli.numeric.labels=stimuli.numeric.labels,
                                    label.low=label.low, label.high=label.high,
                                    decimals=decimals,
                                    participant=participant,
                                    legend.title=legend.title,...)
            
    
          }
    
        #Effects
          if (plot.type=='effects')
          {
             res=stimulus.plot.effects(df=data, dv=dv, condition=condition, stimulus=stimulus, 
                                    participant=participant,
                                    flip.sign=flip.sign,
                                    ylab1=ylab1,
                                    ylab2=ylab2,
                                    xlab1=xlab1,
                                    xlab2=xlab2,
                                    value.labels.offset=value.labels.offset,
                                    stimuli.numeric.labels=stimuli.numeric.labels,
                                    simtot=simtot,
                                    decimals=decimals,...)
             
                 
            }
          

        #Pkg version label
          if (suppress.version.label==FALSE)
          {
            stim_vrs=paste0("{Stimulus v",packageVersion('stimulus'),"}")
            mtext(side=1,line=-1,cex=.7, stim_vrs ,col='gray66',adj=0,outer=TRUE)
          }
           
            
          return(res)

  } #End of wrapper function
  
  