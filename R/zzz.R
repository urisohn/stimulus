
  .onLoad <- function(libname, pkgname) {
  packageStartupMessage('loading stimulus')
  message('--------------------')
  #1. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      
     #Startup msgs
      #Version and URL for website
          packageStartupMessage ("Attached: 'stimulus' (Version: ",packageVersion('groundhog'),  ")") 

      #While developing:
         packageStartupMessage ("#######################################################\n",
                              "This DEV version: 2024 02 07 - 3.35 (Barcelona time)")
      
      } #End on attach
  }
   