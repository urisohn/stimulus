#Generates the expected distribution of effects by forcing the null without assuming homoskedsticity
#Only the mean is adjusted to force the null, allowing each stimulus to have its own variance\






 get.null.demean = function(data,  dv, stimulus, condition, participant,simtot=100,flip.conditions,obs)
  {
    #1 Unique conditions and participant ids
      uc=sort(unique(data[,condition]))
      ui=unique(data[,participant])
      nui=length(ui)
    
    #2 Make null true, all means the same
      #Observed means
        means.obs = get.means.condition(data,dv,stimulus,condition,sort.by='',FALSE)

      #Means for condition 1 & 2 for each stimulus
        m1.obs=means.obs[,1]
        m2.obs=means.obs[,2]
        
      #Overall means for conditions 1 & 2
        m1.all=mean(m1.obs)
        m2.all=mean(m2.obs)
      
      #Compute gap between condition mean and overall mean for each stimulus*condition
        gap1=m1.obs-m1.all
        gap2=m2.obs-m2.all
      
      #Turn gaps to dataframe that is merged with the data to have the gaps in the full data
        df.gaps = data.frame(means.obs[,stimulus],gap1,gap2)
        names(df.gaps)[1]=stimulus
        data=merge(data, df.gaps, by=stimulus)
        
      #Generate the null
        data[,'dv.null'] = ifelse(data[,condition]==uc[1], data[,dv]-data[,'gap1'], data[,dv]-data[,'gap2'])
        
      #Sampling error by randomly drawing participants
        simtot=500
        means.all=matrix(nrow=simtot,ncol=length(unique(data[,stimulus])))
        for (k in 1:simtot)
       {
         
        #Generate data
            ids=sample(ui,replace=TRUE)
            length(unique(ids))
            
            data.null.list=list()
            j=1
            for (idk in ids)
            {
              data.null.list[[j]]=data[data[,participant]==idk,]
              j=j+1
            }
            data.boot <- do.call(rbind, data.null.list)
  
        #Get null.means
          mk   = get.means.condition(data=data.boot,dv='dv.null',stimulus=stimulus,condition=condition,sort.by='',flip.conditions = flip.conditions)
              
        #Extract estimates
          means.all[k,] = sort(mk$effect)
          
        #Counter
          if (k%%50==0) cat('...',k)
        }
      
      
      
  #Compute the full set of sorted effect sizes
   dM=colMeans(means.all) 
   dL=apply(means.all,2,quantile,.025) 
   dH=apply(means.all,2,quantile,.975) 
  
  #Full resamples saved
   under.null.resamples=data.frame(means.all)
   names(under.null.resamples)   =paste0('stimulus_',1:ncol(under.null.resamples))
   rownames(under.null.resamples)=paste0('resample_',1:nrow(under.null.resamples))
   
  #Compute heterogeneity p-value
    d.obs  = means.obs$effect
    e2.obs = sum((dM-d.obs)^2)
    
  #Resampled
      e2.rows <- (sweep(under.null.resamples, 2, dM, FUN = "-"))^2
      e2.resamples <- rowSums(e2.rows)
    
  #p-value
      p.hetero = mean(e2.resamples >= e2.obs)
      p.hetero_text = formatted.p(p.hetero)
      if (p.hetero==0) p.hetero_text = paste0('p<',1/simtot)
      

  #Output
    list(under.null.summary   = data.frame(low=dL, mean=dM, high=dH),
         e2.obs=e2.obs, e2.resamples=e2.resamples,
         p.hetero=p.hetero,
         p.hetero_text=p.hetero_text,
         under.null.resamples = under.null.resamples )

  }
    
 