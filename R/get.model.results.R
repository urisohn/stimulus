  #1 Make variable structure compatible with lmer which cannot do df[,dv]
  #2 Shared parameters
  #3 Regression
  #4 Stimulus intercepts
  #5 Stimulus slopes
  #6 Results



 get.model.results=function(df, dataname, dv, stimulus, condition, participant,model,flip.conditions)
{
#-------------------------------------------------------
  
           
  
          
  #1 Make variable structure compatible with lmer which cannot do df[,dv]
       df2=df
       df2$dv=df[,dv]
       df2$stimulus   = df[,stimulus]
       df2$condition  = df[,condition]
       if (participant !='') df2$participant  = df[,participant]

       ucond=sort(unique(df2$condition))
       if (flip.conditions==TRUE)    df2$condition=factor(df2$condition, levels=ucond)
       if (flip.conditions==FALSE)  df2$condition=factor(df2$condition, levels=rev(ucond))
      

#-------------------------------------------------------

  #2 Shared parameters
      crossed = FALSE
      if (participant!='')
          {  
            t=table(df2$participant,df2$condition)
            crossed <- sum(t[,1]*t[,2]>0)>0
      }
       
      #Stimulus
          stimulus.all=unique(df[,stimulus])
 
#-------------------------------------------------------

  #3 Regression
     if (any(c('all', 'regression') %in% model)) {
          message2("stimulus.plot() says:")
               
      #Fixed effects for participant only if they get different conditions
         #Set up teh regression
            if (crossed==FALSE)  m1.text = "m1=lm(dv~condition+factor(stimulus),data=df2)"
            if (crossed==TRUE)   m1.text = "m1=lm(dv~condition+factor(stimulus)+factor(participant),data=df2)"
            
            
          #Show feedback on screen
            m1.text.formatted= eval.arguments (m1.text, dv, condition, stimulus, participant, dataname)
            message(" ")
            message2("Estimating regression:\n    ",m1.text.formatted)
            
        #Evaluate the regression
            eval2(m1.text)

        #Get mean effect for condition and its  ci
            m1.mean = summary(m1)$coefficients[2,1]
            se =summary(m1)$coefficients[2,2]
            deg.free = m1$df.residual
            tc = qt(.975,df=deg.free)
            m1.p    = summary(m1)$coefficients[2,4]
            m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)

       
        #Cluster by participant if needed
            if (participant!='') {
              m1.cluster.text = "lmtest::coeftest(m1,vcov=sandwich::vcovCL,type='HC3',cluster=~participant)"
          
             #Show feedback on screen
                m1.cluster.text.formatted = eval.arguments(m1.cluster.text, dv, condition, stimulus, participant, dataname)
                message(" ")
                message2("  Clustering the standard errors:\n    ",m1.cluster.text.formatted)
           
             #Estimate SE
                m1.cluster = eval2(m1.cluster.text)
                se = m1.cluster[2,2]
                m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)
                m1.p = m1.cluster[2,4]
              
            } #End if clustering
            
        #Save 
            m.mean = m1.mean
            m.ci   = m1.ci  
            m.labels='Regression'
            m.p = m1.p
       } #End if regression 
    
   
#-------------------------------------------------------
     
  #4 Stimulus intercepts
     if (any(c('intercepts','all') %in% model)) 
     {


          #Run random model
           #Set up text
            if (participant!='') m2.text = "lmerTest::lmer(dv~condition+(1|stimulus)+(1|participant),data=df2)"
            if (participant=='') m2.text = "lmerTest::lmer(dv~condition+(1|stimulus),data=df2)"
            
          #Show feedback on screen
            m2.text.formatted= eval.arguments (m2.text, dv, condition, stimulus, participant, dataname)
            message(" ")
            message2("Estimating random intercepts model:\n    ",m2.text.formatted)
            m2=eval2(m2.text)

          #Get mean effect for condition and its  ci
            m2.mean = summary(m2)$coefficients[2,1]
            m2.ci  =  get.ci(m2)  #see utils.r function #6
            m2.p =  summary(m2)$coefficients[2,5]
            
          #Add
            lab='Random Intercepts'
            m.mean <- if (exists("m.mean")) c(m.mean, m2.mean)   else m2.mean
            m.ci   <- if (exists("m.ci"))   c(m.ci  , m2.ci)     else m2.ci
            m.labels <- if (exists("m.labels")) c(m.labels,lab)  else lab
            m.p       <- if (exists("m.p"))     c(m.p ,m2.p) else m2.p

              
    } #End if intercepts

#-------------------------------------------------------
      
#5 Stimulus slopes
     if (any(c('slopes','all') %in% model)) {

          #Run random model
            if (participant!='') m3.text ="lmerTest::lmer(dv~condition+(1+condition|stimulus)+(1|participant),data=df2)"
            if (participant=='') m3.text = "lmerTest::lmer(dv~condition+(1+condition|stimulus),data=df2)"
            message(" ")
            m3.text.formatted= eval.arguments (m3.text, dv, condition, stimulus, participant, dataname)
            message2("Estimating random slopes model:\n    ",m3.text.formatted)
            m3=eval2(m3.text)

          #Get mean effect for condition and its  ci
            m3.mean = summary(m3)$coefficients[2,1]
            m3.ci  =  get.ci(m3)  #see utils.r function #6
            m3.p =  summary(m3)$coefficients[2,5]

           #Add
            lab='Random Slopes'
            m.mean   <- if (exists("m.mean"))   c(m.mean, m3.mean) else m3.mean
            m.ci     <- if (exists("m.ci"))     c(m.ci  , m3.ci)   else m3.ci
            m.labels <- if (exists("m.labels")) c(m.labels,lab)    else lab
            m.p      <- if (exists("m.p"))     c(m.p , m3.p)      else m3.p

      } #End if slopes   
      
#-------------------------------------------------------
      
#6 Results

    #Summary results for plotting
      results=namedList(m.mean, m.ci, m.labels, m.p)
      
    #Full models
      if (exists('m1'))         results$regression = m1
      if (exists('m1.cluster')) results$regression.clustered_errors = m1.cluster
      if (exists('m2'))         results$random_intercepts = m2
      if (exists('m3'))         results$random_slopes = m3
  
       return(results)
}
      