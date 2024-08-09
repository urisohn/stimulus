
  

  get.means.condition <- function(data, dv, stimulus, condition,sort.by,flip.conditions) {
      
     
    #1 Is it a matched design?
          t = table(data[,stimulus],data[,condition])
          matched = FALSE
          if (mean(t[,1]*t[,2]>0) ==1 ) matched=TRUE
          if (matched==FALSE) exit("The stimuli are not matched across conditions, *effects* for individual stimuli may not be computed.")
          
    #2 Process stimulus and condition values
        #Stimulus
          stimulus.all=unique(data[,stimulus])
          
        #Condition
          ucond=sort(unique(data[,condition]))
          if (flip.conditions==FALSE) data[,condition]=factor(data[,condition], levels=ucond)
          if (flip.conditions==TRUE)  data[,condition]=factor(data[,condition], levels=rev(ucond))
          
    
         
    #2 Compute the mean of the dv for each combination of stimulus and condition
          
          k=1
          for (stimk in stimulus.all)
          {
            #Row with tidy t-test (as data.frame row)
               datak=data[data[,stimulus]==stimk,]
               tk=tidy_t(t.test(datak[,dv]~datak[,condition]))
                #tidy_t puts the t-test results in a dataframe | See #utils.r #4
              
            #Start or add
              if (k == 1) t.all = tk
              if (k > 1)  t.all = rbind(t.all, tk)
              k=k+1
          } #End for loop
           
          
      #Add the stimulus identifier     
        t.all[,stimulus]=stimulus.all
          
              
      # Rename the means columns
        names(t.all)[1:2] <- c(paste0(condition,"_",ucond[1]), paste0(condition,"_",ucond[2]))
           
      # Sort rows
          #Default: effect size
            if (sort.by=='') {
              t.all <- t.all[order(t.all$effect), ]
            }
        
          #Else, by sort.by
            if (sort.by!='') 
            {
              
              #Is the sort.by value unique to each stimulus (e.g., alphabetical order)
                t = table(data[,stimulus],data[,sort.by])
                
                #If there are just as many cells with frequencies >0 as there are stimuli, then it is unique
                  item.unique = FALSE
                  if (sum(t!=0) == length(unique(data[,stimulus]))) item.unique = TRUE
        
        
              #If unique
                if (item.unique==TRUE)
                {
                  #Dataframe with unique values of sort.by for each stimulus
                    sort.by.data <- unique(data[,c(stimulus,sort.by)])

                }
                
              #If not unique it's numeric, so we compute the mean
                if (item.unique==FALSE)
                  {
                  #Compute mean by item
                    sort.by.data <- aggregate(data[, sort.by],list(data[, stimulus]), mean)
                    names(sort.by.data)=c(stimulus,sort.by) 
                } #End if sort.by is not unique to each stimulus
                  #Merge with sort.by
                    t.all = merge(t.all, sort.by.data,by=stimulus)

             
                  
                
        #Sort it
            t.all <- t.all[order(t.all[,sort.by]), ]

        
        } #End if sort.by is not null
                  
              

              
       
                
      return(t.all)
  }
  
 