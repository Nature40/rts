#' bearing calculation
#'
#' @description calculates bearings using linear approximation and acos
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animal list, generatet by initanimal
#' @param antennas file, data.frame with antenna information (Lat,Lon,orientation)
#' @param path_to_data string, path to filtered file
#' @param dbLoss num, gain loss from 0 to 90° antenna orientation
#' @param calibrate logical, if TRUE correction values are applied
#' 
#'
#' @export
#'
#'


calc_bearings<-function(projList,animal, path_to_data, antennas, dbLoss,calibrate){
  
  data<-data.table::fread(path_to_data)
  
  colnames(data)<-as.character(data[1,])
  data<-data[-1,]
  
  if(calibrate){
    fls<-list.files(projList$path$correction, pattern=data$station[1], full.names = TRUE)
    cValues<-plyr::ldply(fls, function(x){data.table::fread(x)})
    data$`0`<-data$`0`+cValues$correction[cValues$receiver==0]
    data$`1`<-data$`1`+cValues$correction[cValues$receiver==1]
    data$`2`<-data$`2`+cValues$correction[cValues$receiver==0]
    data$`3`<-data$`3`+cValues$correction[cValues$receiver==0]
    
    
  }
  
  
  b<-bearings(data=data, dbLoss = dbLoss, antennas = antennas)
  b$antennas<-rowSums(!is.na(b[,c("0","1","2","3"  )]))
  b$max_dB<- apply(b[, c("0","1","2","3"  )], 1, max, na.rm=TRUE)
  if(calibrate==TRUE){
  data.table::fwrite(b, paste0(animal$path$bearings, "/",gsub(".csv", "", basename(path_to_data)), "_bearings_calibrated.csv"))}
  if(calibrate==FALSE){
    data.table::fwrite(b, paste0(animal$path$bearings, "/",gsub(".csv", "", basename(path_to_data)), "_bearings.csv"))}
  
}



bearings<-function(data, dbLoss, antennas){
  
  #lead 
  l1<-(apply(data[,c("0","1","2", "3")], 1, maxn(1)))-1
  #second antenna
  l2<-(apply(data[,c("0","1","2", "3")], 1, maxn(2)))-1
  #third antenna
  l3<-(apply(data[,c("0","1","2", "3")], 1, maxn(3)))-1
  data$lead <- l1
  data$second_antenna<-l2
  data$third_antenna<-l3
  
  #station
  tmp<-antennas[antennas$Name==data$station[1],]
  
  #bearings lead=receiver 0 second=receiver 3
  L0<-data[data$lead==0 & data$second_antenna==3,]
  L0_b<-data[data$lead==0 & data$second_antenna==2 & data$third_antenna==3,]
  L0<-rbind(L0, L0_b)
  #for angle a/LEFT of lead
  l<-tmp$orientation[tmp$receiver==3]
  r<-tmp$orientation[tmp$receiver==0]
  alpha<-angle_between(l,r)
  L0$delta<-(L0$`3`-L0$`0`)/dbLoss
  L0$linear<-1/2*(alpha-alpha*L0$delta/(sin(pi*alpha/180)^2))+l
  L0$acos<-acos(L0$delta)*90/pi+l
  
  #bearings lead=receiver 0 second=receiver 1
  R0<-data[data$lead==0 & data$second_antenna==1,]
  R0_b<-data[data$lead==0 & data$second_antenna==2 & data$third_antenna==1,]
  R0<-rbind(R0, R0_b)
  l<-tmp$orientation[tmp$receiver==0]
  r<-tmp$orientation[tmp$receiver==1]
  alpha<-angle_between(l,r)
  R0$delta<-(R0$`0`-R0$`1`)/dbLoss
  R0$linear<-1/2*(alpha-alpha*R0$delta/(sin(pi*alpha/180)^2))
  R0$acos<-acos(R0$delta)*90/pi
  
  #bearings lead=receiver 1 second=receiver 0
  L1<-data[data$lead==1 & data$second_antenna==0,]
  L1_b<-data[data$lead==1 & data$second_antenna==3 & data$third_antenna==0,]
  L1<-rbind(L1, L1_b)
  l<-tmp$orientation[tmp$receiver==0]
  r<-tmp$orientation[tmp$receiver==1]
  alpha<-angle_between(l,r)
  L1$delta<-(L1$`0`-L1$`1`)/dbLoss
  L1$linear<-1/2*(alpha-alpha*L1$delta/(sin(pi*alpha/180)^2))+l
  L1$acos<-acos(L1$delta)*90/pi+l
  
  #bearings lead=receiver 1 second=receiver 2
  R1<-data[data$lead==1 & data$second_antenna==2,]
  R1_b<-data[data$lead==1 & data$second_antenna==3 & data$third_antenna==2,]
  R1<-rbind(R1, R1_b)
  l<-tmp$orientation[tmp$receiver==1]
  r<-tmp$orientation[tmp$receiver==2]
  alpha<-angle_between(l,r)
  R1$delta<-(R1$`1`-R1$`2`)/dbLoss
  R1$linear<-1/2*(alpha-alpha*R1$delta/(sin(pi*alpha/180)^2))+l
  R1$acos<-acos(R1$delta)*90/pi+l              
  
  #bearings lead=receiver 2 second=receiver 1
  L2<-data[data$lead==2 & data$second_antenna==1 ,]
  L2_b<-data[data$lead==2 & data$second_antenna==0 & data$third_antenna==1,]
  L2<-rbind(L2, L2_b)
  l<-tmp$orientation[tmp$receiver==1]
  r<-tmp$orientation[tmp$receiver==2]
  alpha<-angle_between(l,r)
  L2$delta<-(L2$`1`-L2$`2`)/dbLoss
  L2$linear<-1/2*(alpha-alpha*L2$delta/(sin(pi*alpha/180)^2))+l
  L2$acos<-acos(L2$delta)*90/pi+l
 
  #bearings lead=receiver 2 second=receiver 3
  R2<-data[data$lead==2 & data$second_antenna==3 ,]
  R2_b<-data[data$lead==2 & data$second_antenna==0 & data$third_antenna==3,]
  R2<-rbind(R2, R2_b)
  l<-tmp$orientation[tmp$receiver==2]
  r<-tmp$orientation[tmp$receiver==3]
  alpha<-angle_between(l,r)
  R2$delta<-(R2$`2`-R2$`3`)/dbLoss
  R2$linear<-1/2*(alpha-alpha*R2$delta/(sin(pi*alpha/180)^2))+l
  R2$acos<-acos(R2$delta)*90/pi+l
  
  #bearings lead=receiver 3 second=receiver 2
  L3<-data[data$lead==3 & data$second_antenna==2,]
  L3_b<-data[data$lead==3 & data$second_antenna==1 & data$third_antenna==2,]
  L3<-rbind(L3, L3_b)
  l<-tmp$orientation[tmp$receiver==2]
  r<-tmp$orientation[tmp$receiver==3]
  alpha<-angle_between(l,r)
  L3$delta<-(L3$`2`-L3$`3`)/dbLoss
  L3$linear<-1/2*(alpha-alpha*L3$delta/(sin(pi*alpha/180)^2))+l
  L3$acos<-acos(L3$delta)*90/pi+l
  
  #bearings lead=receiver 3 second=receiver 0
  R3<-data[data$lead==3 & data$second_antenna==0,]
  R3_b<-data[data$lead==3 & data$second_antenna==1 & data$third_antenna==0,]
  R3<-rbind(R3, R3_b)
  l<-tmp$orientation[tmp$receiver==3]
  r<-tmp$orientation[tmp$receiver==0]
  alpha<-angle_between(l,r)
  R3$delta<-(R3$`3`-R3$`0`)/dbLoss
  R3$linear<-1/2*(alpha-alpha*R3$delta/(sin(pi*alpha/180)^2))+l
  R3$acos<-acos(R3$delta)*90/pi+l
  
  data<-rbind(L0,R0,L1,R1,L2,R2,L3,R3)
  return(data)
  
  }

maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

angle_between <- function(angle_a,angle_b){
  ((((angle_b - angle_a) %% 360) + 540) %% 360) - 180
}