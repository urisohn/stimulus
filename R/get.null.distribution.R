 get.null.distribution = function(df,  dv, stimulus, condition, participant,simtot=100,flip.conditions)
  {
    
    #Residualize stimulus effects
      m0=lm(df[,dv]~factor(df[,stimulus]))
      df$r=residuals(m0)
      
    #Compute means on residuals
      means.all=matrix(nrow=simtot,ncol=length(unique(df[,stimulus])))
    
      message2("Will conduct ",simtot," resamples to estimate expected heterogeneity under null of homogeneity.")
      for (k in 1:simtot)
      {
        
      #Shuffle item within condition
        df[,paste0(stimulus,"_shuffled")]  <- ave(df[,stimulus], df[,condition], FUN = function(x) sample(x))
      
      #Get means of residualized dv on shuffled stimuli
        tk   = get.means.condition(df=df,dv='r',stimulus=paste0(stimulus,"_shuffled"),condition=condition,sort.by='',flip.conditions = flip.conditions)
            
      #Extract estimates
        means.all[k,] = sort(tk$effect)
        
      #Counter
        if (k%%50==0) cat('...',k)
    }
        cat("\n")
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
   
  return(data.frame(low=dL, mean=dM, high=dH))

  }
    