
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
                              "DEVELOPMENT VERSION - DO NOT USE IN PUBLISHABLE PAPERS\n",
                              "***  Subject to breaking changes ***\n",
                              "This version: 2024 06 29 7:39 BT")
      
      } #End on attach
  
   