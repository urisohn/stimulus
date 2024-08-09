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
    
#0.4 Figure path
    fig.path = tempfile(fileext='.svg')
    
    fig.path='c:/temp/f1.svg'
    
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
              md5.exp='0cf9384c6366d7ab0cadf952cb65f376'
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
              md5.exp='ef53a6fdf6d93b106e856a46d6002b2e'
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
              md5.exp='8419d3fb642a4d8065c8d901120bc560'
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
              md5.exp='bba5c882a78186be8a09ff06cb1ecf57'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      
    
    
#5) Percentage dv
   test_that("md5 for salerno & slepian means - #5 percent DV", {
        
  
    df1$percent=df1$rev/10
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
              md5.exp='9dd0afee7c4b2be7a26980984a75b833'
        #Compare 
               expect_equal(md5.obs, md5.exp)
               
   })     
              
          
#6) 60 stimuli 
    test_that("md5 for salerno & slepian means - #6 Sixty stimuli", {
 
      df1$stimulus2=paste0(df1$stimulus,sample(1:6,size=nrow(df1),replace=TRUE))
      stimulus.plot(data=df1, dv='rev',   stimulus='stimulus2',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Condition')
              
       #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='56fee05c0b74cbea34b6bf02ed1567bf'
        #Compare 
               expect_equal(md5.obs, md5.exp)
   })     

    
#7) 6 stimuli 
    test_that("md5 for salerno & slepian means - #6 Sixty stimuli", {
 
      df1$stimulus2=paste0(df1$stimulus,sample(1:6,size=nrow(df1),replace=TRUE))
      df2=subset(df1,stimulus %in% unique(df1$stimulus[1:6]))
      stimulus.plot(data=df2, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Effecton OK to share', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Condition')
              
       #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='14a1c4d9bf4768999dbb96d026801106'
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
              md5.exp='0cf9384c6366d7ab0cadf952cb65f376'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

    