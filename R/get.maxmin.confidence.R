

#Does resampling under null of all stimuli having the same distribution, 
#see  stimulus.beeswarm.R 

    get.maxmin.confidence = function(data,  dv, stimulus, condition, simtot=500,confidence=95,ms1,ms2,dc1,dc2)
    {
        
        #how many stimuli
          n1=nrow(ms1)
          n2=nrow(ms2)
          
        #Empty matrices
          ms1.boot=matrix(nrow=simtot,ncol=n1)
          ms2.boot=matrix(nrow=simtot,ncol=n2)
        
          
        #Bootstrap itself under null of equal distributions
          datak=data
          message2("Will run ",simtot," resamples to compute confidence band")
          for (bk in 1:simtot)
          {
          #Shuffle the stimulus
            datak[,stimulus]=unsplit(lapply(split(datak[,stimulus], data[,condition]), sample), datak[,condition])

            ms1.boot[bk,]=sort(aggregate(datak[dc1,dv],list(datak[dc1,stimulus]),mean)$x)
            ms2.boot[bk,]=sort(aggregate(datak[dc2,dv],list(datak[dc2,stimulus]),mean)$x)
            
            if (bk %% 100==0) cat("...",bk)
          }
        
        #Set quantiles for confidence level required
            qL = ((100-confidence)/2)/100
            qH = 1-qL
            
        #Compute quantiles 
            b1L = quantile(ms1.boot[,1],qL)
            b1H = quantile(ms1.boot[,n1],qH)
            b2L = quantile(ms2.boot[,1],qL)
            b2H = quantile(ms2.boot[,n2],qH)
          
        #Output
            output=namedList(b1L,b1H,b2L,b2H)
            return(output)
      
    }