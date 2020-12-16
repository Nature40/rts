#' create file and parameter catalogue from raw logger data
#'
#' @description Creates a catalogue of all raw logger files in folder
#'
#'
#' @author Jannis Gottwald
#'
#' @param station string, station of hich the files should be catagolized
#' @param projList list, generatet by initProject function
#' @param collection string, path to data collection in station folder from which the files should be catagolized
#' 
#'
#' @export
#'
#'






read.logger.data<-function(projList, station=".", collection="."){
 

  root<-paste0(projList$path$raw, station, "/", collection, "/")
  
  cata<-list.files(projList$path$catalogues, pattern=station, full.names=T)
  catalogue<-read.csv( grep(paste0(collection, collapse="|","*"), cata, value=TRUE))
  
  counter <- 0
  
  signals<-lapply(as.character(catalogue$signals), function(x){#
    #print(counter <<- counter + 1)
    
    print(catalogue$station[1])
    print(x)
    print("read")
    
    tryCatch(
      sig<-read.csv(paste0(root, "/",x), sep=";"),
    #print(sig)
     
     error=function(e) print(paste0("can't read file ", x)))
    
    
    
    
    tryCatch (
      sig$signal_freq<-(sig$signal_freq+catalogue$FREQUENCY[catalogue$signals==x])/1000,
      error=function(e) print(paste0("can?t read file ", x)))
    
    tryCatch (
      sig$receiver<-catalogue$receiver[catalogue$signals==x],
      error=function(e) print(paste0("can?t read file ", x)))
    
    tryCatch (
      sig$station<-catalogue$station[catalogue$signals==x],
      error=function(e) print(paste0("can?t read file ", x)))
      
    tryCatch (
      print(head(sig)),
      error=function(e) print(paste0("can?t read file ", x)))
    
    tryCatch (
      return(sig),
      error=function(e) print(paste0("can?t read file ", x)))
    
      
    })
  
  is_df <- sapply(signals, is.data.frame)
  signals<-signals[is_df]
  signals<-Filter(function(x) {nrow(x) >= 2}, signals)
  signals<-Filter(function(x) {nrow(x) >= 2}, signals)
  is_df <- sapply(signals, is.data.frame)
  signals<-signals[is_df]
  stat_dat<-data.table::rbindlist(signals, fill = F)
  catalogue$time<-as.POSIXct(catalogue$time)
  
  
data.table::fwrite(stat_dat, paste0(projList$path$csv, station, "_FROM_", as.Date(min(catalogue$time)), "_TO_",as.Date(max(catalogue$time)), ".csv" ))

  gc()
    }
  


