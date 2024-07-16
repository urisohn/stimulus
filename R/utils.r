
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
  
  
#5 formatted p-value
    formatted.p <- function(p) {
      format_single_p <- function(single_p) {
        p.clean <- round(single_p, 3)           # Round it
        p.clean <- substr(p.clean, 2, 6)        # Drop the 0
        p.clean <- paste0("=", p.clean)
        if (single_p < .0001) p.clean <- "<.0001"
        if (single_p > .9999) p.clean <- ">.9999"
        p.clean <- paste0("p", p.clean)
        return(p.clean)
      }
  
  # Apply the function to each element of the input vector
  result <- sapply(p, format_single_p)
  return(result)
}


  
#6 Confidence interval from model
     get.ci=function(m)
      {
      coe=summary(m)$coefficients 
      b=coe[2,1]                   #point estimate
      se=coe[2,2]                  #SE
      tc = qt(.975,df=coe[2,3])    #look-up t-distribution for 95% CI for those d.f.
      ci=c(b-tc*se , b+tc*se)
      return(ci)
     }
     
     
#7 Add to list if object exists
   add_named_if_exists <- function(obj_name, lst) {
  if (exists(obj_name, envir = parent.frame())) {
    obj_value <- get(obj_name, envir = parent.frame())
    lst[[obj_name]] <- obj_value
  }
  return(lst)
   }
   
   
#8 Automatically name elements in list with name of the objects in the list
    #https://stackoverflow.com/questions/16951080/can-lists-be-created-that-name-themselves-based-on-input-object-names
    namedList <- function(...) {
      L <- list(...)
      snm <- sapply(substitute(list(...)),deparse)[-1]
      if (is.null(nm <- names(L))) nm <- snm
      if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
      setNames(L,nm)
    }