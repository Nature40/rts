#' bearing prediction
#'
#' @description predict bearings using circtree model
#'
#'
#' @author Jannis Gottwald
#'
#'
#' 
#' @param animal list, generatet by initanimal
#' @param path_to_data string, path to filtered file
#' @param mxNA num, may number of missing signals accepted
#' @param version string, character string for identification
#' 
#'
#' @export
#'
#'




  predict_bearings<-function(animal, mxNA, path_to_data, version="."){
  
  data<-data.table::fread(path_to_data)

  data$antennas<-rowSums(!is.na(data[,c("a_0","a_1","a_2","a_3"  )]))
  data$naCount<-rowSums(is.na(data[,c("a_0","a_1","a_2","a_3"  )]))
  data<-data[data$naCount<=mxNA,]
  data$max_dB<- apply(data[, c("a_0","a_1","a_2","a_3"  )], 1, max, na.rm=TRUE)
  
  
  mod<-list.files(system.file("extdata", package="tRackIT"),pattern=data$station[1] ,full.names = TRUE)
 
  mod<-readRDS(mod[1])
  

  p = disttree:::predict.distforest(object = mod, newdata = data)
  
  data$ML = make360(p$mu * 180 / pi)
  data<-data[,c("timestamp","station","a_0","a_1", "a_2", "a_3", "antennas","naCount","max_dB","ML")] 
  
  names(data)<-gsub("a_", "", names(data))
  
  data.table::fwrite(data, paste0(anml$path$bearings, "/",data$station[1],"_",version, "_bearings_ML.csv"))
}

make360 <- function(lon) {
  
  ind <- which(lon < 0)
  lon[ind] <- lon[ind] + 360
  
  return(lon)
}


