library(rts)
library(mapview)
library(rgdal)

plst<-initProject(projroot = "H:/projects/repositories/rts_2020/", logger_data_raw = "I:/data/mof_rts_2020/final/")
ant<-read.csv(paste0(plst$path$ref, "antennas_2020.csv"))
#init animals
anml<-initAnimal(projList = plst, animalID = "calibration_summer", freq = 150199, start = "2020-03-01", end = "2020-05-01")
#generate predictors
fls<-list.files(anml$path$logger_timematch, full.names = TRUE)
machine_vars(fls[7], animal=anml)

#predict with internal model
fls<-list.files(anml$path$vars, full.names=TRUE)
predict_bearings(animal = anml, mxNA = 1, path_to_data = fls[3])

#time match stations
time_match_station(anml, method="ML")

#traingulate
fls<-list.files(anml$path$station_timematch, full.names = TRUE)
triangulate(method="ML",path_to_data=fls[1], animal=anml, antennas = ant)


#get triangulations
fls<-list.files(anml$path$triangulations, full.names = T)
tri<-data.table::fread(fls[2])
tri<-as.data.frame(tri)

#delete data with to flat or to sharp intersection angles
tri<-tri[tri$intersection<=130 & tri$intersection>=30,]
tri<-tri[tri$max_dB_s1>=57 & tri$max_dB_s2>=57,]
tri<-tri[!(is.na(tri$X)|is.na(tri$Y)),]




#trabsform to spatial data
coordinates(tri) <- c("X", "Y")
proj4string(tri) <- CRS("+init=epsg:4326") # WGS 84

coordinates(ant) <- c("Longitude", "Latitude")
proj4string(ant) <- CRS("+init=epsg:4326") # WGS 84

mapview(tri, color="red")+ant



