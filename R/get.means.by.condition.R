
  

  get.means.condition <- function(df, dv, stimulus, condition,participant) {
        df=as.data.frame(df)      #necessary because tidy tables mess things up
        u2=unique(df[,condition])
     
    #1 Is it a matched design?
          t = table(df[,stimulus],df[,condition])
          matched = FALSE
          if (mean(t[,1]*t[,2]>0) ==1 ) matched=TRUE
          
    #2 Compute the mean of the dv for each combination of stimulus and condition
          
          
      #2.1 Raw means if participant is left blank
      
        if (participant=='')
          {
          means.obs1 <- aggregate(df[, dv][df[, condition] == u2[1]],list(df[, stimulus][df[, condition] == u2[1]]), mean)
          means.obs2 <- aggregate(df[, dv][df[, condition] == u2[2]],list(df[, stimulus][df[, condition] == u2[2]]), mean)
          names(means.obs1)=names(means.obs2)=c("condition","x")
          
          } 
            
      #2.2 Partial out participant id if it is included
        if (participant!='') {
        df=as.data.frame(df)
        df2=df[,0]
        df2$dv        = df[,dv]
        df2$condition = df[,condition]
        df2$stimulus  = df[,stimulus]
        df2$participant=df[,participant]

          m1 = lme4::lmer(dv~(1|participant),data=df2)
          r1=residuals(m1)
          
          means.obs1 <- aggregate(r1[df2$condition == u2[1]],list(df2$stimulus[df2$condition == u2[1]]), mean)
          means.obs2 <- aggregate(r1[df2$condition == u2[2]],list(df2$stimulus[df2$condition == u2[2]]), mean)
          names(means.obs1)=names(means.obs2)=c("condition","x")
          intercept = summary(m1)$coefficients[1]
          means.obs1$x = means.obs1$x + intercept
          means.obs2$x = means.obs2$x + intercept
          
          

        }
    
    #2 If Unmatched, early return with sorted means by group
      if (matched==FALSE)
        {
          means.obs1$condition=u2[1]
          means.obs2$condition=u2[2]
          means.obs = rbind(means.obs1,means.obs2)
          names(means.obs)=c(stimulus,'mean','condition')
          means.obs = means.obs[order(means.obs$condition,means.obs$mean),]
          return(means.obs)
        }
        
        
      #Continue now with matched
      
      # Copy column for when condition=0
        means.obs    = means.obs1
        means.obs$r1 = means.obs2$x
        
      # Name the 3 columns, 
        names(means.obs) <- c(stimulus, paste0(condition,"_",u2[1]), paste0(condition,"_",u2[2]))
      
      #Swap columns if the bigger effect comes first
        m1=mean(means.obs[,2])
        m2=mean(means.obs[,3])
        if (m1>m2) means.obs=means.obs[,c(1,3,2)]
        means.obs$effect = means.obs[,3] - means.obs[,2]
        
      # Sort rows
          means.obs <- means.obs[order(means.obs$effect), ]
        
                
      return(means.obs)
  }
  
 