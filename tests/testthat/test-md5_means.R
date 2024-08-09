library(testthat)
library(this.path)
library(stimulus)



#0) Set directory (the built in testtaht was not working, use this.dir() instead)
  here=this.path::this.dir()

#1) Preliminaries
  
  #1.1 Load the data
    fp=file.path(here,'Data/Salerno & Slepian JPSP Study 4.csv')
    df1=read.csv(fp)
    
  #1.2 Nicely formatted condition
    df1$cond=ifelse(df1$intent==1,'Intentional',"Unintentional")
    
  #1.2 Figure path
    fig.path = tempfile(fileext='.svg')
    
    fig.path='c:/temp/f1.svg'
    
#2) BASIC
    test_that("md5 for salerno & slepian means - #1 - Basic", {
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='means',
                            participant='id', save.as=fig.path)    
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='f3bacf9266c8d8a7c7cc4a3e7323d90a'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

#3) FLIP CONDITIONS
        test_that("md5 for salerno & slepian means - #2 Flip conditions", {
      
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                flip.condition=TRUE,
                participant='id', save.as=fig.path)    
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='43933b1ef1fd9f8a03080b615774d15e'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })
    
    
#4) VERSION SIMILAR TO OUR PAPER
    test_that("md5 for salerno & slepian means - #3 Similar to paper", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                save.as=fig.path,
                legend.title = 'Condition')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='3c9f0675b1688049a102d2ad4c47a962'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      

#4) VERSION SIMILAR TO OUR PAPER
    test_that("md5 for salerno & slepian means - #3 Similar to paper", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'intent',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                participant='id',
                save.as=fig.path,
                legend.title = 'Act was')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='5246f7cc9d132a6d5d9fe7aeb2218447'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      
    
    
#5) Do percentage
    df1$percent=df1$rev/10
      #Plot it
              stimulus.plot(data=df1, dv='percent',   stimulus='stimulus',   condition = 'intent',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                label.low    = 'Not intentional',  label.high   = 'Intentional',
                participant='id',
                save.as=fig.path,
                dv.is.percentage = TRUE,
                legend.title = 'Secret act was:')
    #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='5246f7cc9d132a6d5d9fe7aeb2218447'
        #Compare 
               expect_equal(md5.obs, md5.exp)
    
    
    