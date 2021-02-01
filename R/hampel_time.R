#' hampel filter
#'
#' @description detect outliers in bearings using hampel filter in a moving time window
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param data data.frame, data frame with timestamp as Posixct and bearing data
#' @param x string, string indicating the column containing bearings
#' @param k numeric, window size n seconds
#' @param t0 numeric,threshold for median filter. A high threshold makes the filter more forgiving, a low one will declare more points to be outliers. Default=0.5
#' 
#' 
#'
#' @export
#'
#'


hampel <- function (data,x, k, t0 = 0.5)
{
  #   x:  vector or time series
  #   k:  window [x_(i-k),...,x_i,...,x_(i+k)]
  n   <- nrow(data)
  data$bearing_filtered<-data[,x]
  data$outlier<-FALSE# corrected x vector
  ind <- c()       # indices of outliers
  
  L  <- 1.4826     # constants for normal distributions
  # t0 <- 3        # Pearson's 3 sigma edit rule
  
  # we don't look at outliers at the end parts of x !
  for ( i in 1:n ) {
    x0 <- median(data[,x][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] )
    S0 <- L * median( abs(data[,x][data$timestamp>=data$timestamp[i]-k & data$timestamp<=data$timestamp[i]+k ] - x0) )
    if ( abs(data[,x][i]-x0) > t0 * S0 ) {
      data$bearing_filtered[i] <- x0
      ind <- c(ind, i)
    }
  }
  # return a list with 2 components
  data$outlier[ind]<-TRUE
  
  return(data)
}



