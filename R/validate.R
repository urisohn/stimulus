
#Function 1 - validate arguments

  validate.arguments = function(data, dv, condition, stimulus, sort.by, plot.type, 
                              flip.sign, ylab1, ylab2, xlab1, xlab2, value.labels.offset,
                              stimuli.numeric.labels, label.low, label.high, decimals,
                              participant, legend.title,simtot,
                              dataname)
  {
  
  #1 Check all arguments are of the appropriate length and type
      check1(plot.type , 'plot.type', 'character', 1)
      check1(flip.sign , 'flip.sign', 'logical', 1)
      check1(ylab1 , 'ylab1', 'character', 1)
      check1(ylab2 , 'ylab2', 'character', 1)
      check1(xlab1 , 'xlab1', 'character', 1)
      check1(xlab2 , 'xlab2', 'character', 1)
      check1(value.labels.offset , 'value.labels.offset', 'numeric', 1)
      check1(stimuli.numeric.labels , 'stimuli.numeric.labels', 'logical', 1)
      check1(label.low , 'label.low', 'character', 1)
      check1(label.high , 'label.high', 'character', 1)
      check1(decimals , 'decimals', 'numeric', 1)
      check1(plot.type , 'plot.type', 'character', 1)
}


#Function 2 - Validate data

  validate.data = function(data, dv, condition, stimulus, sort.by,participant,dataname)
  {
      n1=names(data)
      if (!dv %in% n1)        exit("stimulus.plot() says the dv ('",dv,    "') is not in the dataset '",dataname,"'.")
      if (!condition %in% n1) exit("stimulus.plot() says the condition variable ('",condition,"') is not in the dataset '",dataname,"'.")
      if (!stimulus %in% n1)  exit("stimulus.plot() says the stimulus variable ('",stimulus,"') is not in the dataset '",dataname,"'.")
      if (!sort.by %in% c(n1,"")    ) exit("stimulus.plot() says the sort.by variable ('",sort.by,"') is not in the dataset '",dataname,"'.")
      if (!participant %in% c(n1,"")) exit("stimulus.plot() says the participant variable ('",participant,"') is not in the dataset '",dataname,"'.")
  }