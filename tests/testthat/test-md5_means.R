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
    
    #fig.path='c:/temp/f1.svg'
    
#--------------------------------------
    
#1) BASIC
    test_that("md5 for salerno & slepian means - #1 - Basic", {
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='means',
                            participant='id', save.as=fig.path)    
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='4da236e88c90a7e7bf31b885ec6abf7b'
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
              md5.exp='dbf442bba1953608e95169643ad266e6'
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
              md5.exp='8a68503428148fdf1f9dcf30c94871d5'
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
              md5.exp='c52850218e0c0131c01ab752b87801bd'
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
              md5.exp='c63035fba3d3bef07ca9e9c44faea699'
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
              md5.exp='250f6cd3fa433004e1550658743b574d'
        #Compare 
               expect_equal(md5.obs, md5.exp)
   })     

    