#' triangulation 
#'
#' @description triangulate by using telemtr package by barry rowlingson (https://github.com/barryrowlingson/telemetr)
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
#' @param plot_bearings logical, wether to plot bearings during processing or not. Slows down the calculation substantially
#'
#'
#' @export
#'
#'


bearing_rolling_fun<-function(animal, antennas, method="ML", tw=5, epsg=4326, maxDB=60, maxNA=2, plot_bearings=FALSE ){

tri_points<-data.frame()

stations<-antennas[!duplicated(antennas$Name),]

fls<-list.files(anml$path$bearings,pattern=method, full.names = TRUE)

#read data and match coordinates
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

#convert to posixct
data$ftime<-fasttime::fastPOSIXct(data$timestamp)

data$ftime<-data$ftime-(as.numeric(abs(difftime(data$timestamp[1], data$ftime[1], units="sec"))))

data<-data[order(data$ftime),]

for(i in 1:nrow(data)){
  
  #print(i)

  tmp<-data[data$ftime>=(data$ftime[i]-tw) & data$ftime<=(data$ftime[i]+tw),]

if(length(unique(tmp$station))>1){


  tmp$animal<-anml$meta$animalID

  sp::coordinates(tmp) <- c("x", "y")

  sp::proj4string(tmp) <- CRS(paste0("+init=epsg:",epsg)) # WGS 84

  if(plot_bearings==TRUE){
    plot(tmp)
    telemetr:::drawVectors(~ML|animal,tmp)
  }


    tryCatch(
      expr = {
      mle<-telemetr::triang(~ML|animal,tmp,method="mle")
      mle$method<-"mle"
    },
    error = function(e){ 
      cat(paste0("mle not calculates for row ", i))
    }
    )

    tryCatch(
      expr = {
        hub<-telemetr::triang(~ML|animal,tmp,method="hub")
        hub$method<-"hub"
      },
      error = function(e){ 
        cat(paste0("hub not calculates for row ", i))
      }
    )
    
    tryCatch(
      expr = {
        and<-telemetr::triang(~ML|animal,tmp,method="and")
        and$method<-"and"
      },
      error = function(e){ 
        cat(paste0("and not calculates for row ", i))
      }
    )
    
    
    tryCatch(
      expr = {
        rmr<-telemetr::triang(~ML|animal,tmp,method="rmr")
        rmr$method<-"rmr"
        rmr$kappa<-"NA"
        rmr$err<-"NA"
        rmr$sd<-"NA"
        rmr$ijob<-"NA"
        
      },
      error = function(e){ 
        cat(paste0("rmr not calculates for row ", i))
        rmr<-NULL
      }
    )
    
    
    tryCatch(
      expr = {
      df<-rbind(mle,hub)
      },
      error = function(e){ 
        cat(paste0("method does not exist for row ", i))
      }
    )
    
    tryCatch(
      expr = {
      df<-rbind(df,and)
    },
    error = function(e){ 
      cat(paste0("method does not exist for row ", i))
    }
    )
    
    tryCatch(
      expr = {
      df<-rbind(df,rmr)
      },
    error = function(e){ 
      cat(paste0("method does not exist for row ", i))
    }
    )
    

  df$Nstations<-length(unique(tmp$station))

  stats<-"mof_rts"
  for( s in unique(tmp$station)){
  stats<-paste0(stats,"_",gsub("mof_rts_000", "", s) )

}

  df$stations<-stats

  df$maxStation<-tmp$station[tmp$max_dB==max(tmp$max_dB)]

  df$maxDB<-max(tmp$max_dB)

  df$timestamp<-data$timestamp[i]

  df$offset<-tw
}

df<-as.data.frame(df)

tri_points<-rbind(df, tri_points)

}


saveRDS(tri_points, paste0(anml$path$triangulations,"/", anml$meta$animalID, "_telemtr_triangulations.rds"))

}





