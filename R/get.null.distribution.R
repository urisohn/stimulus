 get.null.distribution = function(df,  dv, stimulus, condition, participant,simtot=100,flip.sign)
  {
    
    #Model to residualize var controlling for stimuli, with or without participant id
      #df=as.data.frame(df)
      #df2=df[,0]
      #df2$dv        = df[,dv]
      #df2$condition = df[,condition]
      #df2$stimulus  = df[,stimulus]
    
    
    #Residualize stimulus effects
      #m0 = lme4::lmer(dv~(1|stimulus),data=df2)
      m0=lm(df[,dv]~factor(df[,stimulus]))
      df$r=residuals(m0)
      
    #Compute means on residuals
      means.all=matrix(nrow=simtot,ncol=length(unique(df[,stimulus])))
      for (k in 1:simtot)
      {
        
      #Shuffle item within condition
        df[,paste0(stimulus,"_shuffled")]  <- ave(df2$stimulus, df2$condition, FUN = function(x) sample(x))
      
      #Get means of residualized dv on shuffled stimuli
        tk   = get.means.condition(df=df,dv='r',stimulus=paste0(stimulus,"_shuffled"),condition='condition',sort.by='',flip.sign = flip.sign)
            
      #Extract estimates
        means.all[k,] = sort(tk$effect)
        
      #Counter
        if (k%%50==0) cat('...',k)
    }
    
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
   
  return(data.frame(low=dL, mean=dM, high=dH))

  }
    