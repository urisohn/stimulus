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
                            plot.type='effects',
                            simtot=20,
                            participant='id', save.as=fig.path)   
      
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='d47966d933a71904f85a08b44d667e92'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

    
    
#2) FLIP CONDITIONS
        test_that("md5 for salerno & slepian means - #2 Flip conditions", {
      
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                flip.condition=TRUE,
                  plot.type='effects',
                            simtot=20,
                participant='id', save.as=fig.path)    
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='3e3001f4b4bef60cd6aa24388a75df78'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })
    
    
#3) VERSION SIMILAR TO OUR PAPER
    test_that("md5 for salerno & slepian means - #3 Similar to paper", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Condition')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='cdc6a12ccb73e54d4bb6357a3424f234'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      

  
#4) customize ylim
    test_that("md5 for salerno & slepian means - #4 Customize ylim", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                ylim=c(-2,4),
                save.as=fig.path,
                legend.title = 'Condition')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='767b779ca2f99de681bcf46cb0908db6'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      
    
    
#5) Percentage dv
   test_that("md5 for salerno & slepian means - #5 percent DV", {
        
  
  
      #Plot it
               #Plot it
              stimulus.plot(data=df1, dv='percent',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                dv.is.percentage = TRUE,
                save.as=fig.path,
                legend.title = 'Condition')
              
    #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='ac31763b74b7bb6cd0cc7dce08d13366'
        #Compare 
               expect_equal(md5.obs, md5.exp)
               
   })     
              
          
#6) 60 stimuli 
    test_that("md5 for salerno & slepian means - #6 Sixty stimuli", {
 
      stimulus.plot(data=df1, dv='rev',   stimulus='stimulus2',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Condition')
              
       #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='57d4c1dac05cc04f2246becd484c9402'
        #Compare 
               expect_equal(md5.obs, md5.exp)
   })     

    
#7) 6 stimuli 
    test_that("md5 for salerno & slepian means - #6 Sixty stimuli", {
 
    
      stimulus.plot(data=df2, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Condition')
              
       #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='18d7a0c2f837a574410352f7b475f0d2'
        #Compare 
               expect_equal(md5.obs, md5.exp)
   })     
    
    
    
    
#8) WIth models
   test_that("md5 for salerno & slepian means - #1 - Basic", {
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='effects',
                            simtot=20,
                            model='all',
                            participant='id', save.as=fig.path)   
      
     
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='b78aebf549904a794cc7c686abf5860e'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

    