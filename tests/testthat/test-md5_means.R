library(testthat)
library(this.path)
library(stimulus)



#0) Prelims 

#0.1)Set directory (the built in testtaht was not working, use this.dir() instead)
  here=this.path::this.dir()

#0.2 Load the data
    fp=file.path(here,'Data/Salerno & Slepian JPSP Study 4.csv')
    df1=read.csv(fp)
    
#0.3 Nicely formatted condition
    df1$cond=ifelse(df1$intent==1,'Intentional',"Unintentional")
    
#0.4 Additional vars and dfs
    df1$percent=df1$rev/10
    df1$stimulus2=paste0(df1$stimulus,sample(1:6,size=nrow(df1),replace=TRUE))
    df2=subset(df1,stimulus %in% unique(df1$stimulus[1:6]))

#0.5 Figure path
    fig.path = tempfile(fileext='.svg')
    

#0.6 clear cache
    clear_stimulus_cache()
#--------------------------------------
    
#1) BASIC
    test_that("md5 for salerno & slepian means - #1 - Basic", {
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='means',
                            participant='id', save.as=fig.path)    
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='e9b5e455dab8a44e2e35d97d422afe96'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

#2) FLIP CONDITIONS
        test_that("md5 for salerno & slepian means - #2 Flip conditions", {
      
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                flip.condition=TRUE,
                participant='id', save.as=fig.path)    
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='e54bb132325b90648c6f1144d3fc67fd'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })
    
    
#3) VERSION SIMILAR TO OUR PAPER
    test_that("md5 for salerno & slepian means - #3 Similar to paper", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                save.as=fig.path,
                legend.title = 'Condition')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='0060d86d47e122875c2da69386d1ec68'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      

  
#4) customize ylim
    test_that("md5 for salerno & slepian means - #4 Customize ylim", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                save.as=fig.path,
                ylim=c(1,6),
                legend.title = 'Condition')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='15516b99c3211d67ba83ba24faa79c65'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      
    
    
#5) Percentage dv
   test_that("md5 for salerno & slepian means - #5 percent DV", {
        
  
    df1$percent=df1$rev/10
      #Plot it
              stimulus.plot(data=df1, dv='percent',   stimulus='stimulus',   condition = 'intent',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                save.as=fig.path,
                dv.is.percentage = TRUE,
                legend.title = 'Secret act was:')
              
    #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='b20880e31f6b599ad590768a49718a1f'
        #Compare 
               expect_equal(md5.obs, md5.exp)
               
   })     
              
          
#6) 60 stimuli
    test_that("md5 for salerno & slepian means - #6 Sixty stimuli", {
 
    df1$stimulus2=paste0(df1$stimulus,sample(1:6,size=nrow(df1),replace=TRUE))
     stimulus.plot(data=df1, dv='percent',   stimulus='stimulus2',   condition = 'intent',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                flip.conditions = TRUE,
                participant='id',
                save.as=fig.path,
                dv.is.percentage = TRUE,
                legend.title = 'Secret act was:')
       #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='e9f0c10c5012021bbf3e8f76d5d7d5cf'
        #Compare 
               expect_equal(md5.obs, md5.exp)
   })     

    