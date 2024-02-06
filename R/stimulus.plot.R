
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