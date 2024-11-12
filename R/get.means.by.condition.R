
  

  get.means.condition <- function(data, dv, stimulus, condition,sort.by,flip.conditions) {
      

    #  Process stimulus and condition values
        #Stimulus
          stimulus.all=unique(data[,stimulus])
          
        #Condition
          ucond=sort(unique(data[,condition]))
          if (flip.conditions==FALSE) data[,condition]=factor(data[,condition], levels=ucond)
          if (flip.conditions==TRUE)  data[,condition]=factor(data[,condition], levels=rev(ucond))
          
    
         
    #2 Compute means and CI via t-test
        
      # Split data by `stimulus`
          split_data <- split(data, data[,stimulus])
          
          # Apply t-test for each split, then extract p-value and statistic
          t.all_list <- lapply(split_data, function(sub_data) {
            test <- t.test(sub_data[,dv] ~sub_data[,condition])
            tk=tidy_t(test)
            tk
          })
          
        # Combine the results into a dataframe
          t.all <- do.call(rbind, t.all_list)
          t.all <- data.frame(stimulus = rownames(t.all), t.all, row.names = NULL)
          
              
      # Rename the means columns
        names(t.all)[2:3] <- c(paste0(condition,"_",ucond[1]), paste0(condition,"_",ucond[2]))
        names(t.all)[1] <-stimulus 
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
  
 