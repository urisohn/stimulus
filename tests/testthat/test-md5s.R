library(testthat)
library(this.path)
library(stimulus)



#0) Prelims 

#0.1)Set directory (the built in testtaht was not working, use this.dir() instead)
  here=this.path::this.dir()
  saved_path=paste0(here,"/saved results")
  
#0.2 Load the data
    fp=file.path(here,'Data/Salerno & Slepian JPSP Study 4.csv')
    df1=read.csv(fp)
    
#0.3 Nicely formatted condition
    df1$cond=ifelse(df1$intent==1,'Intentional',"Unintentional")
    
#0.4 Additional vars and dfs
    df1$percent=df1$rev/10
    df1$stimulus2=paste0(df1$stimulus,sample(1:6,size=nrow(df1),replace=TRUE))
    df2=subset(df1,stimulus %in% unique(df1$stimulus[1:6]))

#0.5 clear cache
    clear_stimulus_cache()
#--------------------------------------
    
#1) Test effects with simtot=20
    
    #Run test
    test_that("Salerno & Slepian Effects with simtot=20", {
          r1= stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='effects',
                            simtot=20,
                            participant='id')   
          
    #Path where results are saved 
          fp=file.path(saved_path,"r1.rds")
        
    #Get MD5s
          md5.saved=get.md5(readRDS(fp))
          md5.new = get.md5(r1)
          
        #Compare observed
            expect_equal(md5.new, md5.saved)
          
            
      })

   
    
  
#2) Test means in Salerno
    
    #Run test
    test_that("Salerno & Slepian means ", {
          r2= stimulus.plot(data=df1, dv='rev',   stimulus='stimulus',   condition = 'cond',  
                            plot.type='means',
                            participant='id')   
          
    #Path where results are saved 
          fp=file.path(saved_path,"r2.rds")
          
    #saved when writing the script
          #saveRDS(r2,fp)
    #Get MD5s
          md5.saved.r2=get.md5(readRDS(fp))
          md5.new.r2 = get.md5(r2)
          
        #Compare observed
            expect_equal(md5.new.r2  ,  md5.saved.r2)
          
            
      }) 

#3) Effects including all 3 models
   test_that("md5 for salerno & slepian means - #1 - Basic", {
          #Plot it
              r3=stimulus.plot(data=df1, dv='rev',   
                            stimulus='stimulus',   
                            condition = 'cond',  
                            plot.type='effects',
                            simtot=20,
                            model='all',
                            participant='id',save.as='c:/temp/abc.svg')   
    
         #Path where results are saved 
          fp3=file.path(saved_path,"r3.rds")
          
         #For some reasonm, saving the entire mixed-model output does not loead to predictable md5
            r3.list = list(r3$model.results$m.mean,r3$model.results$m.ci,r3$model.results$m.labels,r3$model.results$p)
            
              
        #save when writing the script
            saveRDS(r3.list,fp3)
    
        #Get MD5s
          md5.saved.r3=get.md5(readRDS(fp3))
          md5.new.r3 = get.md5(r3.list)
          
        #Compare observed
            expect_equal(md5.new.r3  ,  md5.saved.r3)
         
      })

    