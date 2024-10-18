
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
   
  tidy_t = function(t)
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
        if (p.clean==0) return ("p<.0001")
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
    
#9 Format as percent
format_percent <- function(x) {
  decimal_places <- auto.decimals(x * 100)
  formatted_number <- sapply(1:length(x), function(i) {
    paste0(formatC(x[i] * 100, format = "f", digits = decimal_places[i]), "%")
  })
  return(formatted_number)
}
    
    
#10 Number of decimals
    auto.decimals <- function(x) {
  sapply(x, function(num) {
    num=abs(num)
    if (is.na(num)) {
      return(NA)
    } else if (num > 100) {
      return(0)
    } else if (num > 10) {
      return(1)
    } else if (num > 0.1) {
      return(2)
    } else if (num > 0.01) {
      return(3)
    } else {
      return(4)
    }
  })
}

    
#11 # Function to compute the MD5 hash of a dataframe
  get.md5 <- function(df) {
    df_serialized <- serialize(df, NULL)
    md5_hash <- digest::digest(df_serialized, algo = "md5")
    return(md5_hash)
  }
  
#12 Does cache exist
  does.cache.d.exist = function(md5k)
  {
  if (exists(".stimulus.cache", envir = .GlobalEnv)) {
   key_exists <- md5k %in% names(.GlobalEnv$.stimulus.cache)
    } else {
      key_exists <- FALSE
    }
    return(key_exists)
    
  }
  
#13 eval2
 eval2 <- function(s)  eval(parse(text=s),  parent.frame())  #Added "parent.frame() because otherwise eval2() will not call the right objects within another function: Fix was found here; https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/eval


#14 eval.arguments
 eval.arguments = function(model_string, dv, condition, stimulus, participant,data)
   {
     model_string <- gsub("dv", dv, model_string)
     model_string <- gsub("condition", condition, model_string)
     model_string <- gsub("stimulus", stimulus, model_string)
     model_string <- gsub("df2", data, model_string)
     model_string <- gsub("participant", participant, model_string)
    return(model_string)
 }
 
#15 message.green
 message2=function(...)
   {
     msg <- paste0(...)
     dark_green <- "\033[32m"
     reset <- "\033[0m"
     message(paste0(dark_green, paste0(msg), reset))
   }

#16 compute mean and CI
  get.semean_ci <- function(x, conf = 0.95) {
    n <- length(x)
    mean_x <- mean(x)
    stderr <- sd(x) / sqrt(n)
    alpha <- 1 - conf
    error_margin <- qt(1 - alpha / 2, df = n - 1) * stderr
    lower_bound <- mean_x - error_margin
    upper_bound <- mean_x + error_margin
  return(data.frame(mean = mean_x, lower = lower_bound, upper = upper_bound))
}
