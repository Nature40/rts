#' triangulation 
#'
#' @description triangulate using telemtr package (not on CRAN) by barry rowlingson (https://github.com/barryrowlingson/telemetr). Methods are described here: https://www.jstor.org/stable/1268030?seq=1#metadata_info_tab_contents. Use remote::install_github("barryrowlingson/telemetr") and set "no errors from warnings" to TRUE before installing the package - Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE).
#'
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animal list, generatet by initanimal
#' @param method string, pattern indicating whether ML bearings or bearings based on physical model should be selected
#' @param tw num, time window in seconds: Locations are calculated using all bearings tw sec before and after timestamp in row
#' @param epsg num, epsg code for coordinate refernce system in use (must be metric). Lat/Lon is default
#' @param antennas data.frame, Data.frame with antenna information
#' @param maxDB num, minimum value for received maximum signal strength. default=60
#' @param maxNa num, max for missing receiver per row. default=2
#' @param plot_results logical, wether to plot bearings during processing or not. Slows down the calculation substantially
#' @param version string, character string for identification
#' @param x string, name of column with bearings. x="bearings_filtered" if hampel filtered bearings are used 
#' @param xlim numeric, x limits for plot
#' @param ylim numeric, y limits for plot
#' @param GPS logical, if true reference gps data is expected in cols lon/lat
#' @import data.table
#'
#' @export
#'
#'


bearing_rolling_fun<-function(animal, antennas, version="ML", tw=5, epsg=4326, maxDB=60, maxNA=2, GPS=FALSE, plot_results=FALSE, x="ML", xlim=c(8.665209, 8.700571), ylim=c(50.826948, 50.849172), keep_cores=2){


# get rid of duplicates in stations
stations<-antennas[!duplicated(antennas$Name),]

#read data and match coordinates of stations
fls<-list.files(animal$path$bearings_filtered,pattern=version, full.names = TRUE)
data<-plyr::ldply(fls, function(x){
  d<-data.table::fread(x)
  d$x<-NA
  d$y<-NA
  d<-as.data.frame(d)
  d$y<-stations$Latitude[stations$Name==d$station[1]]
  d$x<-stations$Longitude[stations$Name==d$station[1]]
  return(d)
  })

#filter by maxDB and naCount
data<-data[data$max_dB>=maxDB & data$naCount<=maxNA,]
# x = colname of bearingsings beari
data$be<-data[,x]

#convert to posixct
data$ftime<-fasttime::fastPOSIXct(data$timestamp)
data$ftime<-data$ftime-(as.numeric(abs(difftime(data$timestamp[1], data$ftime[1], units="sec"))))
data<-data[order(data$ftime),]

#turn stations spatial for plotting
sp::coordinates(stations) <- c("Longitude", "Latitude")
sp::proj4string(stations) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84

if(GPS==TRUE & plot_results==TRUE){
  
  tri_points<-plot_it_gps(data=data, tw=tw, xlim=xlim, ylim=ylim, animal=animal,epsg=epsg, stations=stations, x=x)
  
}

if(GPS==TRUE & plot_results==FALSE){
  
  tri_points<-dont_plot_it_gps(data=data, tw=tw,animal=animal,epsg=epsg, stations=stations, x=x, keep_cores=keep_cores)
  
}

if(GPS==FALSE & plot_results==FALSE){
  
  tri_points<-dont_plot_it(data=data, tw=tw, animal=animal,epsg=epsg, sations=stations, x=x, keep_cores=keep_cores)
  
}

if(GPS==FALSE & plot_results==TRUE){
  
  tri_points<-plot_it(data=data, tw=tw, xlim=xlim, ylim=ylim, animal=animal,epsg=epsg, stations=stations, x=x)
  
}


saveRDS(tri_points, paste0(anml$path$triangulations,"/", anml$meta$animalID, "_telemtr_triangulations_",x, "_", tw, ".rds"))

}



###
plot_it<-function(data, tw, xlim, ylim, animal,epsg, stations, x){
  
  # create data frame to fill
  tri_points<-data.frame()
  
  # triangulate row by row      
  for(i in 1:nrow(data)){
    print(i)
    
    # subset data in time window
    tmp<-data[data$ftime>=(data$ftime[i]-tw) & data$ftime<=(data$ftime[i]+tw),]
    tmp<-as.data.frame(tmp)
    tmp$animal<-animal$meta$animalID
    
    if(any(tmp$be>360)){next}
    if(length(unique(tmp$station))>1){
      
      # calculate triangulations with simple intersection method
      tmp2<-time_match_station(tmp, method=x, tmstmp=data$timestamp[i])
      tmp2<-tri_in(data=tmp2,stations = stations, method = x )
      tmp2$method="intersection"
      
      #data for intersection plotting
      
      intersect<-tmp2
      intersect<-intersect[!is.na(intersect$x),]
      intersect_mean<-tmp2
      intersect_mean$x<-mean(intersect$x, na.rm=TRUE)
      intersect_mean$y<-mean(intersect$y, na.rm=TRUE)
      intersect_mean<-intersect_mean[!is.na(intersect_mean$x),]
      
      
      if(nrow(intersect)==0){
        intersect<-tmp
      }
      
      if(nrow(intersect_mean)==0){
        intersect_mean<-tmp
      }
      
      
      
      sp::coordinates(tmp) <- c("x", "y")
      sp::proj4string(tmp) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      sp::coordinates(intersect) <- c("x", "y")
      sp::proj4string(intersect) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      sp::coordinates(intersect_mean) <- c("x", "y")
      sp::proj4string(intersect_mean) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      
      
      tlmetr<-calc_telemetr(tmp=tmp, i=i, data=data, tmp2=tmp2, tw=tw, animal=animal)
      
      # print intersection
      print(tlmetr$df$intersection_max[1])
      print(tlmetr$df$intersection_min[1])
      
      
    }
    else{next}
    
    
    plot(tmp, xlim=range(8.665209, 8.700571), ylim=range(50.826948, 50.849172))
    points(stations)
    telemetr:::drawVectors(~be|animal,tmp)
    points(tlmetr$mle,pch=19,col="blue")
    points(tlmetr$and,pch=20,col="yellow")
    points(tlmetr$hub,pch=20,col="green")
    points(intersect,pch=20,col="purple")
    points(intersect_mean,pch=20,col="orange")
    
    
    
    if(is.data.frame(tlmetr$df)){
      tri_points<-rbind(tlmetr$df, tri_points)}
  }
  
  return(tri_points)
  
}




dont_plot_it<-function(data, tw, xlim, ylim, animal,epsg, stations, x, keep_cores){
  
  # create data frame to fill
  tri_points<-data.frame()
  # check number of cores
  ncores<-parallel::detectCores()
  # register number of cores - cores_t_keep
  cl <- parallel::makeCluster(ncores-keep_cores)
  doParallel::registerDoParallel(cl)
  
  # triangulate in parallel 
  foreach(i = 6000:nrow(data),) %dopar% {
    
    # subset data in time window
    tmp<-data[data$ftime>=(data$ftime[i]-tw) & data$ftime<=(data$ftime[i]+tw),]
    tmp<-as.data.frame(tmp)
    tmp$animal<-animal$meta$animalID
    
    if(any(tmp$be>360)){next}
    if(length(unique(tmp$station))>1){
      
      # calculate triangulations with simple intersection method
      tmp2<-time_match_station(tmp, method=x, tmstmp=data$timestamp[i])
      tmp2<-tri_in(data=tmp2,stations = stations, method = x )
      tmp2$method="intersection"
      
      # turn tmp to spatial
      sp::coordinates(tmp) <- c("x", "y")
      sp::proj4string(tmp) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      # triangulate using telemtr package
      tlmetr<-calc_telemetr(tmp=tmp, i=i, data=data, tmp2=tmp2, tw=tw, animal=animal)
      
      
    }
    else{next}
    # fill data frame  
    if(is.data.frame(tlmetr$df)){
      tri_points<-rbind(tlmetr$df, tri_points)}
  }
  
  parallel::stopCluster(cl)
  
  return(tri_points)
  
}




plot_it_gps<-function(data, tw, xlim, ylim, animal,epsg, stations, x){
  
  # create data frame to fill
  tri_points<-data.frame()
  
  # triangulate row by row      
  for(i in 1:nrow(data)){
    print(i)
    
    # subset data in time window
    tmp<-data[data$ftime>=(data$ftime[i]-tw) & data$ftime<=(data$ftime[i]+tw),]
    tmp<-as.data.frame(tmp)
    tmp$animal<-animal$meta$animalID
    
    if(any(tmp$be>360)){next}
    if(length(unique(tmp$station))>1){
      
      # calculate triangulations with simple intersection method
      tmp2<-time_match_station(tmp, method=x, tmstmp=data$timestamp[i])
      tmp2<-tri_in(data=tmp2,stations = stations, method = x )
      tmp2$method="intersection"
      
      #data for intersection plotting
      
      intersect<-tmp2
      intersect<-intersect[!is.na(intersect$x),]
      intersect_mean<-tmp2
      intersect_mean$x<-mean(intersect$x, na.rm=TRUE)
      intersect_mean$y<-mean(intersect$y, na.rm=TRUE)
      intersect_mean<-intersect_mean[!is.na(intersect_mean$x),]
      
      
      if(nrow(intersect)==0){
        intersect<-tmp
      }
      
      if(nrow(intersect_mean)==0){
        intersect_mean<-tmp
      }
      
      gps<-tmp
      
      sp::coordinates(tmp) <- c("x", "y")
      sp::proj4string(tmp) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      sp::coordinates(intersect) <- c("x", "y")
      sp::proj4string(intersect) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      sp::coordinates(intersect_mean) <- c("x", "y")
      sp::proj4string(intersect_mean) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      
      sp::coordinates(gps) <- c("lon", "lat")
      sp::proj4string(gps) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      
      
      tlmetr<-calc_telemetr(tmp=tmp, i=i, data=data, tmp2=tmp2, tw=tw, animal=animal)
      
      # clac distance to gps  
      gps<-as.data.frame(gps)
      tlmetr$df$lon<-median(gps$lon)
      tlmetr$df$lat<-median(gps$lat)
      tlmetr$df$dist_gps<-raster::pointDistance(cbind(tlmetr$df$x, tlmetr$df$y), cbind(tlmetr$df$lon, tlmetr$df$lat), lonlat=T)
      print(paste0("mle: ",tlmetr$mle$cor, "min dist: ", min(tlmetr$df$dist_gps, na.rm=TRUE)))
      print(tlmetr$df$intersection_max[1])
      print(tlmetr$df$intersection_min[1])
      
      
    }
    else{next}
    
    
    plot(tmp, xlim=range(8.665209, 8.700571), ylim=range(50.826948, 50.849172))
    points(stations)
    telemetr:::drawVectors(~be|animal,tmp)
    points(tlmetr$mle,pch=19,col="blue")
    points(tlmetr$and,pch=20,col="yellow")
    points(tlmetr$hub,pch=20,col="green")
    points(intersect,pch=20,col="purple")
    points(intersect_mean,pch=20,col="orange")
    points(gps,pch=20,col="red")
    
    
    if(is.data.frame(tlmetr$df)){
      tri_points<-rbind(tlmetr$df, tri_points)}
  }
  
  return(tri_points)
  
}






dont_plot_it_gps<-function(data, tw, xlim, ylim, animal,epsg, stations, x, keep_cores){
  
  # create data frame to fill
  tri_points<-data.frame()
  # check number of cores
  ncores<-parallel::detectCores()
  # register number of cores - cores_t_keep
  cl <- parallel::makeCluster(ncores-keep_cores)
  doParallel::registerDoParallel(cl)
  
  # triangulate in parallel 
  foreach(i = 6000:nrow(data)) %dopar% {
    
    # subset data in time window
    tmp<-data[data$ftime>=(data$ftime[i]-tw) & data$ftime<=(data$ftime[i]+tw),]
    tmp<-as.data.frame(tmp)
    tmp$animal<-animal$meta$animalID
    
    if(any(tmp$be>360)){next}
    if(length(unique(tmp$station))>1){
      
      # calculate triangulations with simple intersection method
      tmp2<-time_match_station(tmp, method=x, tmstmp=data$timestamp[i])
      tmp2<-tri_in(data=tmp2,stations = stations, method = x )
      tmp2$method="intersection"
      
      # turn tmp to spatial
      sp::coordinates(tmp) <- c("x", "y")
      sp::proj4string(tmp) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84
      # triangulate using telemtr package
      tlmetr<-calc_telemetr(tmp=tmp, i=i, data=data, tmp2=tmp2, tw=tw, animal=animal)
      # clac distance to gps    
      tlmetr$df$lon<-median(tmp$lon)
      tlmetr$df$df$lat<-median(tmp$lat)
      tlmetr$df$dist_gps<-raster::pointDistance(cbind(tlmetr$df$x, tlmetr$df$y), cbind(tlmetr$df$lon, tlmetr$df$lat), lonlat=T)
      
      
    }
    # fill data frame  
      tri_points<-rbind(tlmetr$df, tri_points)
  }
  
  parallel::stopCluster(cl)
  
  return(tri_points)
  
}





nice_tmp<-function(tmp2, df, animal, tw){


  tmp2$stations<-paste0("mof_rts_", stringr::str_sub(tmp2$name_s1,-2,-1), "_",stringr::str_sub(tmp2$name_s2,-2,-1))
  tmp2.dt <- data.table::data.table(tmp2)
  tmp2<-tmp2.dt[,list(intersection=mean(intersection, na.rm=TRUE), x=mean(x, na.rm=TRUE), y=mean(y, na.rm=TRUE)), by='stations']
 tmp2<-as.data.frame(tmp2)
   
  tmp2$x<-mean(tmp2$x, na.rm=TRUE)
  tmp2$y<-mean(tmp2$y, na.rm=TRUE)
  tmp2$timestamp<-df$timestamp[1]
  tmp2$animal<-animal$meta$animalID
  tmp2$method<-"intersect"
  tmp2$kappa<-"NA"
  tmp2$err<-"NA"
  tmp2$sd<-"NA"
  tmp2$ijob<-"NA"
  tmp2$maxDB<-df$maxDB[1]
  tmp2$maxStation<-df$maxStation[1]
  tmp2$offset<-tw
  tmp2$Nstations<-df$Nstations[1]
  tmp2$se.x<-NA
  tmp2$se.y<-NA
  tmp2$npts<-NA
  tmp2$intersection_min<-min(tmp2$intersection, na.rm=TRUE)
  tmp2$intersection_max<-max(tmp2$intersection, na.rm=TRUE)
  tmp2$intersection<-NULL
  tmp2$cor<-NA
  
  return(tmp2)
  
}


calc_telemetr<-function(tmp, i, data, tmp2, tw, animal){
  
  tryCatch(
    expr = {
      mle<-telemetr::triang(~be|animal,tmp,method="mle")
      mle$method<-"mle"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      hub<-telemetr::triang(~be|animal,tmp,method="hub")
      hub$method<-"hub"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      and<-telemetr::triang(~be|animal,tmp,method="and")
      and$method<-"and"
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  tryCatch(
    expr = {
      rmr<-telemetr::triang(~be|animal,tmp,method="rmr")
      rmr$method<-"rmr"
      rmr$kappa<-"NA"
      rmr$err<-"NA"
      rmr$sd<-"NA"
      rmr$ijob<-"NA"
      
    },
    error = function(e){ 
      
      rmr<-NULL
    }
  )
  
  
  tryCatch(
    expr = {
      df<-rbind(mle,hub)
    },
    error = function(e){ 
      NULL
    }
  )
  
  tryCatch(
    expr = {
      df<-rbind(df,and)
    },
    error = function(e){ 
      NULL
    }
  )
  
  tryCatch(
    expr = {
      df<-rbind(df,rmr)
    },
    error = function(e){ 
      NULL
    }
  )
  
  
  stats<-"mof_rts"
  for( s in unique(tmp$station)){
    stats<-paste0(stats,"_",gsub("mof_rts_000", "", s) )
    
  }
  
  df<-as.data.frame(df)
  
  tmp2<-nice_tmp(tmp2=tmp2, df=df, animal=animal, tw=tw)
  
  df<-cbind(data.frame(offset=tw,stations=stats, intersection_min=mean(tmp2$intersection_min, na.rm=TRUE), intersection_max=mean(tmp2$intersection_max, na.rm=TRUE)), df)
  
  
  df<-rbind(df, tmp2)
  
  
  df<-cbind(data.frame(Nstations=length(unique(tmp$station)), maxStation=tmp$station[tmp$max_dB==max(tmp$max_dB)],maxDB=max(tmp$max_dB),timestamp=data$timestamp[i],corMLE=mle$cor), df)
  
  df<-df[!is.na(df$x),]
  
  return(list(mle=mle, hub=hub, and=and, df=df))
  
  
}




