
  

  get.means.condition <- function(df, dv, stimulus, condition,participant,sort.by) {
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
          #Default: effect size
            if (sort.by=='') {
              means.obs <- means.obs[order(means.obs$effect), ]
            }
        
          #Else, by sort.by
            if (sort.by!='') 
            {
              
              #Is the sort.by value unique to each stimulus (e.g., alphabetical order)
                t = table(df[,stimulus],df[,sort.by])
                
                #If there are just as many cells with frequencies >0 as there are stimuli, then it is unique
                  item.unique = FALSE
                  if (sum(t!=0) == length(unique(df[,stimulus]))) item.unique = TRUE
        
        
              #If unique
                if (item.unique==TRUE)
                {
                  #Dataframe with unique values of sort.by for each stimulus
                    sort.by.df <- unique(df[,c(stimulus,sort.by)])
                }
                
              #If not unique it's numeric, so we compute the mean
                if (item.unique==FALSE)
                  {
                  #Compute mean by item
                    sort.by.df <- aggregate(df[, sort.by],list(df[, stimulus]), mean)
                    names(sort.by.df)=c(stimulus,sort.by) 
                
                  #Merge with sort.by
                    means.obs=merge(means.obs, sort.by.df,by=stimulus)

                } #End if sort.by is not unique to each stimulus
                  
                
        #Sort it
            means.obs <- means.obs[order(means.obs[,sort.by]), ]
              
               
        
        } #End if sort.by is not null
                  
              

              
       
                
      return(means.obs)
  }
  
 