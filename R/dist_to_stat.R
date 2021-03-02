#' distance to station
#'
#' @description calculates the distance of traingulations to the station with maxDB
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param data data.frame, generatet by triangulation function
#' @param antennas file, data.frame with antenna information (Lat,Lon,orientation)
#' 
#' @export
#'
#'




dist_to_stat<-function(data, antennas){
  data$Stat_X<-NA
  data$Stat_y<-NA
  for(i in 1:nrow(data)){
    
    data$Stat_Y[i]<-stations$Latitude[stations$Name==data$maxStation[i]]
    data$Stat_X[i]<-stations$Longitude[stations$Name==data$maxStation[i]]
  }
  
  data$stat_dist<-raster::pointDistance(cbind(data$Stat_X, data$Stat_Y), cbind(data$x, data$y), lonlat=T)
  
  
  return(data)
}
