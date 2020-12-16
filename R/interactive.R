#' Interactive functions
#'
#' @export
#'


continue<-function(){
  x <- readline("continue?: ")  
  return(x)
}


upper_cut <- function(){
  x <- readline("below the signal strength value the frequnency filter is applied: ")  
  y <- readline("max accepted freq error left of signal frequency (negativ value): ")
  z <- readline("max accepted freq error right of signal frequency (positive value): ")
  
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  z <- as.numeric(unlist(strsplit(z, ",")))
  
  return(list(x, y,z))
  
}



fun_fine<- function(){
  x <- readline("lower max signal limit for parameter calculation: ")  
  y <- readline("upper max signal limit for parameter calculation: ")
  
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  
  return(list(x, y))
  
}

fun_param_okay<- function(mid_freq, sd_freq){
  x <- readline(cat(paste0("Mid frequency: ", mid_freq, "; sd: ",sd_freq, " okay? Y/N: ")))  
  
  
  
  #x <- as.numeric(unlist(strsplit(x, ",")))
  
  
  return(x)
  
}

where_cut<- function(){
  x <- readline("upper cut: ")  
  
  
  
  x <- as.numeric(unlist(strsplit(x, ",")))
  
  
  return(x)
  
}

success<- function(){
  x <- readline("success?: ")  
  return(x)
  
}
