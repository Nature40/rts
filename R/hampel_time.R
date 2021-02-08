#' hampel filter
#'
#' @description detect outliers in bearings using hampel filter in a moving time window
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param path_to_data string, dpath to file containing bearings
#' @param col string, string indicating the column containing bearings
#' @param k numeric, window size n seconds
#' @param t0 numeric,threshold for median filter. A high threshold makes the filter more forgiving, a low one will declare more points to be outliers. Default=0.5
#' 
#' 
#'
#' @export
#'
#'


hampel <- function (path_to_data,col, k, t0 = 0.5, animal)
{
  data<-data.table::fread(path_to_data)
  #   x:  vector or time series
  #   k:  window [x_(i-k),...,x_i,...,x_(i+k)]

  data<-as.data.frame(data)
  data$timestamp<-as.POSIXct(data$timestamp)
  data[,paste0("bearings_filtered_", col)]<-data[,col]
  data<-data[!is.na(data[,paste0("bearings_filtered_", col)]),]
  n   <- nrow(data)
  data$outlier<-FALSE# corrected x vector
  ind <- c()       # indices of outliers
  
  L  <- 1.4826     # constants for normal distributions
  # t0 <- 3        # Pearson's 3 sigma edit rule
  
  # we don't look at outliers at the end parts of x !
  for ( i in 1:n ) {
    #print(i)
    x0 <- median(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] )
    S0 <- L * median( abs(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] - x0) )
    if ( abs(data[,col][i]-x0) > t0 * S0 ) {
      data[,paste0("bearings_filtered_", col)][i] <- x0
      ind <- c(ind, i)
    }
  }
  # return a list with 2 components
  data$outlier[ind]<-TRUE
  
  data$timestamp<-as.character(data$timestamp)
  
  data.table::fwrite(data, paste0(animal$path$bearings_filtered, "/",gsub(".csv", "", basename(path_to_data)), "_hample_filtered.csv"))
}


hampel_double <- function (path_to_data,col, k, t0 = 0.5, animal)
{
  data<-data.table::fread(path_to_data)
  #   x:  vector or time series
  #   k:  window [x_(i-k),...,x_i,...,x_(i+k)]
  
  data<-as.data.frame(data)
  data$timestamp<-as.POSIXct(data$timestamp)
  data$bearings_filtered_double<-data[,col]
  data<-data[!is.na(data$bearings_filtered),]
  n   <- nrow(data)
  data$outlier<-FALSE# corrected x vector
  ind <- c()       # indices of outliers
  
  L  <- 1.4826     # constants for normal distributions
  # t0 <- 3        # Pearson's 3 sigma edit rule
  
  # we don't look at outliers at the end parts of x !
  for ( i in 1:n ) {
    #print(i)
    x0 <- median(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] )
    S0 <- L * median( abs(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] - x0) )
    if ( abs(data[,col][i]-x0) > t0 * S0 ) {
      data$bearings_filtered_double[i] <- x0
      ind <- c(ind, i)
    }
  }
  # return a list with 2 components
  data$outlier[ind]<-TRUE
  
  data$timestamp<-as.character(data$timestamp)
  
  data.table::fwrite(data, paste0(animal$path$bearings_filtered, "/",gsub(".csv", "", basename(path_to_data)), "_hample_filtered_double.csv"))
}




hampel_poistion <- function (data,col, k, t0 = 0.5)
{
  
  n   <- nrow(data)
  data<-as.data.frame(data)
  data$timestamp<-as.POSIXct(data$timestamp)
  data$filtered<-data[,col]
  data$outlier<-FALSE# corrected x vector
  ind <- c()       # indices of outliers
  
  L  <- 1.4826     # constants for normal distributions
  # t0 <- 3        # Pearson's 3 sigma edit rule
  
  # we don't look at outliers at the end parts of x !
  for ( i in 1:n ) {
    x0 <- median(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] )
    S0 <- L * median( abs(data[,col][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] - x0) )
    if ( abs(data[,col][i]-x0) > t0 * S0 ) {
      data$filtered[i] <- x0
      ind <- c(ind, i)
    }
  }
  # return a list with 2 components
  data$outlier[ind]<-TRUE
  
  return(data)
}



