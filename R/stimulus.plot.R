#' Make stimulus plots as in Simonsohn, Montealegre, & Evangelidis (2024) 
#' 
#'Make figure with stimulus level means or effects for experiments with multiple stimuli
#'
#'@param df dataframe containing variables to be analyzied
#'@param dv the dependent variable (e.g., dv='y')
#'@param condition a string or numeric vector containing the condition indicator (e.g., condition='treatment') 
#'@param stimulus a string or numeric vector identifying the different stimuli used
#'@param plot.type can be either "means" or "effects", determines what's plotted in the y-axis
#'@param flip.sign boolean on whether to sort effect size in reverse order (defaults to FALSE)
#'@param ylab1,ylab2 string variables for labels on the y-axis (optional)
#'@param xlab1,xlab2 string variables for labels on the x-axis (optional)
#'@param decimals numeric (integer), how many decimals to depict in the graph

#' @export
#4 stimulus.plot (Wrapper function)
  stimulus.plot = function(df, dv, condition, stimulus, 
                    plot.type='means',
                    flip.sign=FALSE,
                    ylab1='',
                    ylab2='',
                    xlab1='Stimuli',
                    xlab2='',
                    value.labels.offset = -1,
                    stimuli.numeric.labels=FALSE,
                    label.low='low',
                    label.high='high',
                    decimals=2,
                    participant='',
                    legend.title='',
                    simtot=100,
                    ...
                    )
        {
          
        #Ensure dataframe is a dataframe
          df=data.frame(df)
    
          
        #Drop missing values
          n1=nrow(df)
          df = df[!is.na(df[,stimulus]) & !is.na(df[,dv]) & !is.na(df[,condition]),]
          if (participant!='') df = df[!is.na(df[,participant]),]
          n2=nrow(df)
          if (n2<n1) message('stimulus.plot() says:\nA total of ',n1-n2,' observations were dropped because of missing values.')
          
        #Means
          if (plot.type=='means')
          {
            res=stimulus.plot.means(df=df, dv=dv, condition=condition, stimulus=stimulus, 
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
            
    
              return(invisible(res))
          }
    
        #Effects
          if (plot.type=='effects')
          {
             res=stimulus.plot.effects(df=df, dv=dv, condition=condition, stimulus=stimulus, 
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
            return(invisible(res))         
            }
          

          

  } #End of wrapper function