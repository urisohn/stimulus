 get.null.distribution = function(df,  dv, stimulus, condition, participant,simtot=100)
  {
    
  #Model to residualize var controlling for stimuli, with or without participant id
    df=as.data.frame(df)
    df2=df[,0]
    df2$dv        = df[,dv]
    df2$condition = df[,condition]
    df2$stimulus  = df[,stimulus]
    df2$participant=""
    if (participant!='') df2$participant=df[,participant]

    df2=as.data.frame(df2)    
    
    
    #Residualize stimulus effects
      if (participant!='')     m0 = lme4::lmer(dv~(1|stimulus)+(1|participant),data=df2)
      if (participant=='')     m0 = lme4::lmer(dv~(1|stimulus)                ,data=df2)
    
      df2$r = residuals(m0)
                    
    #Compute means on residuals
      means.all=matrix(nrow=simtot,ncol=length(unique(df2$stimulus)))
      for (k in 1:simtot)
      {
        
      #Shuffle item within condition
        df2$stimulus_shuffled  <- ave(df2$stimulus, df2$condition, FUN = function(x) sample(x))
      
      #Get means of residualized dv on shuffled stimuli
        means.k   = get.means.condition(df=df2,dv='r',stimulus='stimulus_shuffled',condition='condition',participant='')
        
      #Extract estimates
        means.all[k,] = sort(means.k$effect)
        
      #Counter
        if (k%%50==0) cat('...',k)
    }
    
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
   
  return(data.frame(low=dL, mean=dM, high=dH))

  }
    