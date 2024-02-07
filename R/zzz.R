
  .onLoad <- function(libname, pkgname) {
  
  }
  
  
  #1. Attaching 
    #' @importFrom utils packageVersion compareVersion
     
    .onAttach <- function(libname, pkgname) {
      
     #Startup msgs
      #Version and URL for website
          packageStartupMessage ("Attached: 'stimulus' (Version: ",packageVersion('stimulus'),  ")") 

      #While developing:
         packageStartupMessage ("#######################################################\n",
                              "This DEV version: 2024 02 07 - 3.35 (Barcelona time)")
      
      } #End on attach
  
   