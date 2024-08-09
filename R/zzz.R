
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
                              "This Version 2024 08 09 - 03.23PM\n",
                              "***  Subject to breaking changes & likely errors  ***")
      
      } #End on attach






