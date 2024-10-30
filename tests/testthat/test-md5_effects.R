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
              md5.exp='79aca095e633b02bbb34dcb1d89f59a7'
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
              md5.exp='79aca095e633b02bbb34dcb1d89f59a7'
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
              md5.exp='dad575e2a30dc26ad21ded77d2c6a9d1'
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
              md5.exp='96bb6235deb7e805940c352e8236cecc'
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
              md5.exp='fa14daf1d496f5e6b545592bfc6b1fd4'
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
              md5.exp='a36e3f5a80cf3b74e8ffbca480c4f7b6'
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

    