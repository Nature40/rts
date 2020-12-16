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
#' @export
#'
#'



file.catalogue<-function(projList, station=".", collection="."){

    root<-paste0(projList$path$raw, station, "/", collection, "/")
    
    tmp_files<-list.files(root , full.names = T)
    
    all_files<-unlist(strsplit(tmp_files, split = "/"))
    
    all_csv<-all_files[grepl("*\\.csv", all_files)]
    all_sig<-as.data.frame(stringr::str_split_fixed(all_csv, "_", 2))
    all_sig$file<-all_csv
    all_time<-as.data.frame(stringr::str_split_fixed(all_sig$V1, "T", 2))
    all_time$time<-NA

    for(j in 1:nrow(all_time))
      {
        
        x<-as.character(all_time$V2[j])
        x2<-substring(as.character(all_time$V2[j]), seq(1, nchar(x)-1, 2), seq(2, nchar(x), 2))
        all_time$time[j]<-paste0(x2[1], ":", x2[2], ":", x2[3])
       }

    all_sig$time<-paste(all_time$V1, all_time$time)
    
    colnames(all_sig)<-c("conf_match","receiver", "signals", "time")
    
    all_sig$receiver<-stringr::str_replace(all_sig$receiver, ".csv", "")
    
    all_conf<-all_files[grepl("*\\.conf", all_files)]
    
    conf_df<-data.frame(config=all_conf, conf_match=stringr::str_replace(all_conf, ".conf", ""))
    
    catalogue<-merge(all_sig, conf_df, by ="conf_match" )
    
    catalogue$GAIN<-NA
    
    catalogue$THRESHOLD<-NA
    
    catalogue$FREQUENCY<-NA

    for (m in unique(catalogue$conf_match))
        {
  
        if(file.exists(paste0(root, "/",catalogue$config[catalogue$conf_match==m][1]))){
        conf<-read.csv(paste0(root, "/",catalogue$config[catalogue$conf_match==m][1]), header=F, sep="=")
        catalogue$GAIN[catalogue$conf_match==m]<-conf$V2[conf$V1=="GAIN"]
        catalogue$THRESHOLD[catalogue$conf_match==m]<-conf$V2[conf$V1=="THRESHOLD"]
        catalogue$FREQUENCY[catalogue$conf_match==m]<-conf$V2[conf$V1=="FREQUENCY"]}
       
        
        else{next}
        }

    catalogue$station<-station
    catalogue$time<-as.POSIXct(catalogue$time)
    write.csv(catalogue, paste0( paste0(projList$path$catalogues, station,"_collection_",collection,"_FROM_", as.Date(min(catalogue$time)), "_TO_",as.Date(max(catalogue$time)), "_file_catalogue.csv")))
}


