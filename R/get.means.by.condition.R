
  

  get.means.condition <- function(df, dv, stimulus, condition,sort.by) {
        u2 = unique(df[,condition])
     
    #1 Is it a matched design?
          t = table(df[,stimulus],df[,condition])
          matched = FALSE
          if (mean(t[,1]*t[,2]>0) ==1 ) matched=TRUE
          if (matched==FALSE) exit("The stimuli are not matched across conditions, *effects* for individual stimuli may not be computed.")
          
    #2 Compute the mean of the dv for each combination of stimulus and condition
          
          stimulus.all=unique(df[,stimulus])
          k=1
          for (stimk in stimulus.all)
          {
            #Row with tidy t-test (as data.frame row)
              dfk=df2[df2$stimulus==stimk,]
               tk=tidy.t(t.test(dfk$dv~dfk$condition))
                #tidy.t puts the t-test results in a dataframe | See #utils.r #4
              
            #Start or add
              if (k == 1) t.all = tk
              if (k > 1)  t.all = rbind(t.all, tk)
              k=k+1
          } #End for loop
           
          
      #Add the stimulus identifier     
        t.all[,stimulus]=stimulus.all
          
              
      # Rename the means columns
        names(t.all)[1:2] <- c(paste0(condition,"_",u2[1]), paste0(condition,"_",u2[2]))
        
        
      #Swap columns if the bigger effect comes first
        # m1=mean(means.obs[,2])
        # m2=mean(means.obs[,3])
        # if (m1>m2) means.obs=means.obs[,c(1,3,2)]
        # means.obs$effect = means.obs[,3] - means.obs[,2]
        
          #if commenting back in, change means.obs to t.all
        
      # Sort rows
          #Default: effect size
            if (sort.by=='') {
              t.all <- t.all[order(t.all$effect), ]
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
                    t.all = merge(t.all, sort.by.df,by=stimulus)

                } #End if sort.by is not unique to each stimulus
                  
                
        #Sort it
            t.all <- t.all[order(t.all[,sort.by]), ]
              
               
        
        } #End if sort.by is not null
                  
              

              
       
                
      return(t.all)
  }
  
 