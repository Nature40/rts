#' time match logger data
#'
#' @description timematch of simoultaneously received signals by logger of one station 
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animalID string, label of the tagged individual
#' @param path_to_data string, path to filtered data
#' @param collision logical, if collision corrected data is used a suffix is added to resulting file name
#' 
#'
#' @export
#'
#'



time_match_logger<-function(animal, path_to_data, collision=FALSE){
data<-data.table::fread(path_to_data)
  
data<-data[!is.na(data$max_signal),]
  
  

data$timestamp<-as.POSIXct(data$timestamp)

  
  #order by timestamp
data<-data[order(data$timestamp),]
data$timestamp<-as.character(data$timestamp)

  #reshape
  
data<-data.table::dcast(data, timestamp+station~receiver, value.var = "max_signal", mean)
  
  #save
  if(nrow(data[!is.na(data$`0`),])>10){
  data$`0`<-imputeTS::na_interpolation( data$`0`, option ="linear", maxgap = 2)}
  if(nrow(data[!is.na(data$`1`),])>10){
  data$`1`<-imputeTS::na_interpolation( data$`1`, option ="linear", maxgap = 2)}
  if(nrow(data[!is.na(data$`2`),])>10){
  data$`2`<-imputeTS::na_interpolation( data$`2`, option ="linear", maxgap = 2)}
  if(nrow(data[!is.na(data$`3`),])>10){
  data$`3`<-imputeTS::na_interpolation( data$`3`, option ="linear", maxgap = 2)}
  
data$timestamp<-as.character(data$timestamp)
if(collision==TRUE){data.table::fwrite(data, paste0(animal$path$logger_timematch, "/", gsub("_filtered_collision.csv", "",basename(path_to_data)), "_logger_time_match_collision.csv" ))}

if(collision==FALSE){data.table::fwrite(data, paste0(animal$path$logger_timematch, "/", gsub("_filtered.csv", "",basename(path_to_data)), "_logger_time_match.csv" ))}


}
