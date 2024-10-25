

#Function - Validate data

  validate.data = function(f, data, dv, condition, stimulus, sort.by,participant,dataname)
  {
        if (missing(data))      exit(f," says: you must specify a dataframe")
        if (missing(dv))        exit(f," says: you must specify the dependent variable ('dv')")
        if (missing(condition)) exit(f," says: you must specify the condition variable ('condition')")
        if (missing(stimulus))  exit(f," says: you must specify the stimulus variable ('stimulus')")     
    
    
      n1=names(data)
      if (!dv %in% n1)        exit(f,"() says the dv ('",dv,    "') is not in the dataset '",dataname,"'.")
      if (!condition %in% n1) exit(f,"() says the condition variable ('",condition,"') is not in the dataset '",dataname,"'.")
      if (!stimulus %in% n1)  exit(f,"() says the stimulus variable ('",stimulus,"') is not in the dataset '",dataname,"'.")
      if (!sort.by %in% c(n1,"")    ) exit(f," says the sort.by variable ('",sort.by,"') is not in the dataset '",dataname,"'.")
      if (!participant %in% c(n1,"")) exit(f," says the participant variable ('",participant,"') is not in the dataset '",dataname,"'.")
  }
  
  