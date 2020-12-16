#' identify signal collision
#'
#' @description find collsions between tags of same frequency but different signal duration by finding gaps in the timeseries
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animal10 list, generatet by initanimal-meta information of tag with 10ms duration
#' @param animal20 list, generatet by initanimal-meta information of tag with 20ms duration
#' @param animal40 list, generatet by initanimal-meta information of tag with 40ms duration
#' @param path_to_d10 string, path to filtered file of tag with 10ms duration (same time period and station as d20,d40)
#' @param path_to_d20 string, path to filtered file of tag with 20ms duration (same time period and station as d10,d40)
#' @param path_to_d40 string, path to filtered file of tag with 40ms duration (same time period and station as d10,d20)
#' @param td10 num, expected timedifference between signals of tag with 10ms duration
#' @param td20 num, expected timedifference between signals of tag with 20ms duration
#' 
#'
#' @export
#'
#'




collision_detector<-function(animal10, animal20, animal40, path_to_d10, path_to_d20, path_to_d40, td10, td20){

d10<-data.table::fread(path_to_d10)
d20<-data.table::fread(path_to_d20)
d40<-data.table::fread(path_to_d40)

d10$timestamp<-as.POSIXct(d10$timestamp)
d20$timestamp<-as.POSIXct(d20$timestamp)
d40$timestamp<-as.POSIXct(d40$timestamp)

data10<-data.frame()
data20<-data.frame()
data40<-data.frame()

for ( i in c(0,1,2,3)){
  
  print(i)
  print("d10")
d10_r<-d10[d10$receiver==i,]
d10_r<-d10_r[order(d10_r$timestamp),]
d10_r$collision<-"no"
#d10_r$ID<-1:nrow(d10_r)
print("d20")
d20_r<-d20[d20$receiver==i,]
d20_r<-d20_r[order(d20_r$timestamp),]
d20_r$ID<-1:nrow(d20_r)
d20_r$collision<-"no"
d20_r$timeControld20<-d20_r$timestamp
print("d40")
d40_r<-d40[d40$receiver==i,]
d40_r<-d40_r[order(d40_r$timestamp),]
d40_r$ID<-1:nrow(d40_r)
d40_r$collision<-"no"
d40_r$timeControld40<-d40_r$timestamp

print("timediff")
d10_r$timediff<-abs(difftime( d10_r$timestamp, d10_r$timestamp[2:length(d10_r$timestamp)], units="secs" ))
d20_r$timediff<-abs(difftime( d20_r$timestamp, d20_r$timestamp[2:length(d20_r$timestamp)], units="secs" ))


print("subset timediff")
d10_c<-d10_r[d10_r$timediff>=td10+(td10*0.2) & d10_r$timediff<=2*td10+(td10*0.2), ]
d20_c<-d20_r[d20_r$timediff>=td20+(td20*0.5) & d20_r$timediff<=2*td20+(td20*0.5), ]


print("timediff df")
c10<-data.frame(timestamp=d10_c$timestamp+td10, timestamp_control=d10_c$timestamp+td10 )
c20<-data.frame(timestamp=d20_c$timestamp+td20, timestamp_control=d20_c$timestamp+td20)

print("dat.table")
prepD10<-data.table::data.table(c10)
prepD20<-data.table::data.table(c20)
victimD20<-data.table::data.table(d20_r)
victimD40<-data.table::data.table(d40_r)
print("set key")
data.table::setkey(prepD10,timestamp)
data.table::setkey(prepD20,timestamp)

data.table::setkey(victimD20,timestamp)
data.table::setkey(victimD40,timestamp)



#match nearest
p10v20 <- victimD20[prepD10, roll = "nearest"]
p10v40 <- victimD40[prepD10, roll = "nearest"]

p20v40 <- victimD40[prepD20, roll = "nearest"]


#"timediff 2"
p10v20$timediff<-abs(difftime(p10v20$timeControld20, p10v20$timestamp_control, units="secs" ))
p10v40$timediff<-abs(difftime(p10v40$timeControld40, p10v40$timestamp_control, units="secs" ))
p20v40$timediff<-abs(difftime(p20v40$timeControld40, p20v40$timestamp_control, units="secs" ))


#threshold
p10v20<-p10v20[p10v20$timediff<=0.06,]
p10v40<-p10v40[p10v40$timediff<=0.06,]
p20v40<-p20v40[p20v40$timediff<=0.06,]

d20_r$collision[d20_r$ID %in% p10v20$ID]<-"yes"
d40_r$collision[d40_r$ID %in% p10v40$ID]<-"yes"
d40_r$collision[d40_r$ID %in% p20v40$ID]<-"yes"


data10<-rbind(data10, d10_r)
data20<-rbind(data20, d20_r)
data40<-rbind(data40, d40_r)
}

data.table::fwrite(data10, paste0(animal10$path$filtered, "/",gsub(".csv", "", basename(path_to_d10)), "_collision.csv" ))
data.table::fwrite(data20, paste0(animal20$path$filtered, "/",gsub(".csv", "", basename(path_to_d20)), "_collision.csv" ))
data.table::fwrite(data40, paste0(animal40$path$filtered, "/",gsub(".csv", "", basename(path_to_d40)), "_collision.csv" ))


}







mode<-function(x){abs(unique(x)[which.max(tabulate(match(x, unique(x))))])}
