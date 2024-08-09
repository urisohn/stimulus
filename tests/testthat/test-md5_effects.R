library(testthat)
library(this.path)
library(stimulus)



#0) Set directory (the built in testtaht was not working, use this.dir() instead)
  here=this.path::this.dir()

#1) Load the data
    fp=file.path(here,'Data/Salerno & Slepian JPSP Study 4.csv')
    df1=read.csv(fp)
    fig.path = tempfile(fileext='.svg')
    
#2) BASIC
    test_that("md5 for salerno & slepian  - #1 - Basic", {
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'intent',  
                plot.type='effects',
                simtot=20,
                participant='id', save.as=fig.path)    
          #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='9eb100c50bb7913cdadd7089cf6e7797'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })

#3) FLIP CONDITIONS
        test_that("md5 for salerno & slepian  - #2 Flip conditions", {
      
          #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'intent',  
                flip.condition=TRUE,
                plot.type='effects',
                simtot=20,
                participant='id', save.as=fig.path)    
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='40668985c6fec03ade43e71e6c4d9c80'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      })
    
    
#4) VERSION SIMILAR TO OUR PAPER
    test_that("md5 for salerno & slepian  - #3 Similar to paper", {
        
         #Plot it
              stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'intent',  
                ylab1='Mean of the DV', ylab2='(OK to Reveal Secret as Punishment: 1-6)', 
                label.low    = 'Not intentional',  label.high   = 'Intentional',
                participant='id',
                plot.type='effects',
                simtot=20,
                save.as=fig.path,
                legend.title = 'Secret act was:')
              
         #Get md5s
              md5.obs=as.character(tools::md5sum(fig.path))
              md5.exp='98e12bfb360637a3d1f521565d5ab330'
        #Compare 
               expect_equal(md5.obs, md5.exp)
      
        
      })
      

  
    
    
    
    