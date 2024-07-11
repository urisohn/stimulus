
#Functions
  #1 Rounding
  #2 Exit
  #3 Clean string
  #4 tidy.t



#----------------------------------

#1 Rounding
  round2 <- function(x, digits = 2) { 
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
  }


  
#2 Exit
     exit <- function(...) {
    message(...)
    invokeRestart("abort")
    }
  
    gstop <- function(msg,format=FALSE) {
    #Format the message with line breaks and border if requested
    if (format==TRUE) msg=format_msg(msg) 
    message(msg)
    message("----------------------------------------")
    exit()
    }
    
    
  
#3 Clean string
   clean_string <- function(input_string) {
      #Remove everything except letters, numbers, and underscores
      #cleaned_string <- gsub("[^A-Za-z0-9._]", "", input_string)
       cleaned_string <- gsub("\"", "", input_string)

  return(cleaned_string)
   }
   
   
   
#4 tidy.t
   
  tidy.t = function(t)
  {
  row=data.frame(
                 m1     = t$estimate[1],
                 m2     = t$estimate[2],
                 effect = t$estimate[1]-t$estimate[2],
                 t      = t$statistic,
                 df     = t$parameter,
                 p      = t$p.value,
                 ciL    = t$conf.int[1],
                 ciH    = t$conf.int[2], 
                 row.names = NULL)    
  return(row)    
    
  }
  

  