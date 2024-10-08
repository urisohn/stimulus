

 get.null.shuffle = function(data,  dv, stimulus, condition, participant,simtot=100,flip.conditions,obs)
  {
    
    #Residualize stimulus effects
      m0 = lm(data[,dv]~factor(data[,stimulus]))
      data$r=residuals(m0)
      
      
    #Compute means on residuals
      message2("Will conduct ",simtot," resamples to estimate expected heterogeneity under null of homogeneity.")
      
      means.all=matrix(nrow=simtot,ncol=length(unique(data[,stimulus])))
      for (k in 1:simtot)
      {
        
      #Shuffle item within condition
        data[,paste0(stimulus,"_shuffled")]  <- ave(data[,stimulus], data[,condition], FUN = function(x) sample(x))
      
      #Get means of residualized dv on shuffled stimuli
        tk   = get.means.condition(data=data,dv='r',stimulus=paste0(stimulus,"_shuffled"),condition=condition,sort.by='',flip.conditions = flip.conditions)
            
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
  
  #Full resamples saved
   under.null.resamples=data.frame(means.all)
   names(under.null.resamples)   =paste0('stimulus_',1:ncol(under.null.resamples))
   rownames(under.null.resamples)=paste0('resample_',1:nrow(under.null.resamples))
   
  #Compute heterogeneity p-value
    d.obs  = obs$effect
    e2.obs = sum((dM-d.obs)^2)
    
    #Resampled
      e2.rows <- (sweep(under.null.resamples, 2, dM, FUN = "-"))^2
      e2.resamples <- rowSums(e2.rows)
    
    #p-value
      p.hetero = mean(e2.resamples >= e2.obs)
      p.hetero_text = formatted.p(p.hetero)
     if (p.hetero == 0) {
        rounded_value <- 1 / simtot
        
        # Check if rounded_value is less than 0.0001
        if (rounded_value < 0.0001) {
          p.hetero_text <- 'p<.0001'
        } else {
          p.hetero_text <- paste0('p<', format(rounded_value, digits = 1, scientific = FALSE))
        }
      }

      

  #Output
    list(under.null.summary   = data.frame(low=dL, mean=dM, high=dH),
         e2.obs=e2.obs, e2.resamples=e2.resamples,
         p.hetero=p.hetero,
         p.hetero_text=p.hetero_text,
         under.null.resamples = under.null.resamples )

  }
    
 