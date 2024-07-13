


function run.models(df, dv, stimulus, condition, participant,models)
{
  
#-------------------------------------------------------
  
  #1 Make variable structure compatible with lmer which cannot do df[,dv]
       df2=df
       df2$dv=df[,dv]
       df2$stimulus   = df[,stimulus]
       df2$condition  = df[,condition]
       if (participant!='') df2$participant  = df[,participant]


#-------------------------------------------------------

              
  #2 Regression
     if (any('all', 'regression') %in% models) {
               
         #Fixed effects for participant only if they get different conditions
         #"crossing" participant and condition effects
         
            t=table(df2$participant,df2$condition)
            crossed <- sum(t[,1]*t[,2]>0)>0
            
          #Run regression
            if (crossed==FALSE)  m1 = lm(dv~condition+factor(stimulus),data=df2)
            if (crossed==TRUE)   m1 = lm(dv~condition+factor(stimulus)+factor(participant),data=df2)
          
            
          #Get mean effect for condition and its  ci
            m1.mean = summary(m1)$coefficients[2,1]
            m1.ci   =  confint(m1)[2,]
  
        #Cluster by participant if needed
            if (participant!='') {
              m1.cluster = lmtest::coeftest(m1,vcov=vcovCL,type='HC3',cluster=~participant)
              se = m1.cluster[2,2]
              deg.free = m1$df.residual
              tc = qt(.975,df=deg.free)
              m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)
              
            } #End if clustering
       } #End if regression 
    
       
       overall.estimate = m1.mean
       overall.ci = m1.ci
       overall.label = "Regression"
       
     #2 Regression
     if ('regression' %in% models) {
               
         #Fixed effects for participant only if they get different conditions
         #"crossing" participant and condition effects
         
            t=table(df2$participant,df2$condition)
            crossed <- sum(t[,1]*t[,2]>0)>0
            
          #Run regression
            if (crossed==FALSE)  m1 = lm(dv~condition+factor(stimulus),data=df2)
            if (crossed==TRUE)   m1 = lm(dv~condition+factor(stimulus)+factor(participant),data=df2)
          
            
          #Get mean effect for condition and its  ci
            m1.mean = summary(m1)$coefficients[2,1]
            m1.ci   =  confint(m1)[2,]
  
        #Cluster by participant if needed
            if (participant!='') {
              m1.cluster = lmtest::coeftest(m1,vcov=vcovCL,type='HC3',cluster=~participant)
              se = m1.cluster[2,2]
              deg.free = m1$df.residual
              tc = qt(.975,df=deg.free)
              m1.ci = c(m1.mean - tc*se, m1.mean+tc*se)
              
            } #End if clustering
       } #End if regression 
       
            
  
}