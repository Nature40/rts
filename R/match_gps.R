#' filter data interactivematch to gps data
#'
#' @description match bearings to gps for accuracy assessment
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param data data.frame, data.frame with bearings
#' @param td_to_gps numeric, timedifference between bearing timestamps (UTC) and gps timestamp
#' @param gps data.frame, gps data
#' @param nth num, subset data for plotting using only every nth column
#' @import data.table
#'
#' @export
#'
#'



match_gps<-function(data, antennas, gps, td_to_gps){
  
  data$timestamp<-as.POSIXct(data$timestamp)
  data$timestamp<-data$timestamp+td_to_gps
  
  #handle times
  gps$gps_time<-as.POSIXct(gps$gps_time)
  gps$time_gps<-gps$gps_time
  gps<-gps[order(gps$time_gps),]
  
  #init match column
  gps<-data.table::data.table(gps)
  data.table::setkey(gps,time_gps)
  
  
  cat("match")
  data<-as.data.frame(data)
  match<-data.table::data.table(data)
  match$time<-match$timestamp
  data.table::setkey(match,time)
  match <- gps[match, roll = "nearest"]
  
  cat("calc timediffs")
  match$timediff<-as.numeric(abs(difftime(match$timestamp, match$gps_time)))
  match<-as.data.frame(match)
  #exclude timediffs>1
  match<-match[match$timediff<=1,]
  
  cat("match data with coordinates from stations")
  stations<-antennas[!duplicated(antennas$Name),]
  match$StatLat<-NA
  match$StatLon<-NA
  
  cat("station coords")
  match<-as.data.frame(match)
  match$StatLat<-stations$Latitude[stations$Name==match$station[1]]
  match$StatLon<-stations$Longitude[stations$Name==match$station[1]]
  
  
  
  cat("calc real bearings")
  match$real_bearing<-NA
  v<-swfscMisc::bearing(match$StatLat, match$StatLon, match$lat, match$lon)
  match$real_bearing<-as.numeric(v[1:nrow(match)])
  
  match$distance<-NA
  cat("calc distance")
  match$distance<-raster::pointDistance(cbind(match$StatLon, match$StatLat), cbind(match$lon, match$lat), lonlat=T)
  return(match)
}






match_gps_triang<-function(data, gps, td_to_gps){
  
  data$timestamp<-as.POSIXct(data$timestamp)
  data$timestamp<-data$timestamp+td_to_gps
  
  #handle times
  gps$gps_time<-as.POSIXct(gps$gps_time)
  gps$time_gps<-gps$gps_time
  gps<-gps[order(gps$time_gps),]
  
  #init match column
  gps<-data.table::data.table(gps)
  data.table::setkey(gps,time_gps)
  
  
  #match
  
  match<-data.table::data.table(as.data.frame(data))
  match$time<-match$timestamp
  data.table::setkey(match,time)
  match <- gps[match, roll = "nearest"]
  
  #calc timediffs
  match$timediff<-as.numeric(abs(difftime(match$timestamp, match$gps_time)))
  match<-as.data.frame(match)
  #exclude timediffs>1
  match<-match[match$timediff<=1,]
  
 
  match$distance<-NA
  
  match$distance<-raster::pointDistance(cbind(match$X, match$Y), cbind(match$lon, match$lat), lonlat=T)
  return(match)
}




