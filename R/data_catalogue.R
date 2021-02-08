#' catalogue of all tRackIT csv with start and end date
#'
#' @description Creates a catalogue of all raw logger files in folder
#'
#'
#' @author Jannis Gottwald
#' @param projList list, generatet by initProject function
#' @param in_project logical, if true the catalogue is created from the data in the project folder
#' @param path_to_folder string, e catalogue is created from the data in the indicated folder 
#'
#'
#' @export
#'
#'



data.catalogue<-function(projList, in_project=TRUE, path_to_folder="."){

    if(in_project){
    files<-list.files(projList$path$csv)}
    if(!in_project){
        files<-list.files(path_to_folder)
    }
    from<-unlist(lapply(strsplit(files, "_"), function(x){return(x[which(x=="FROM")+1])}))
    to<-gsub(".csv", "",unlist(lapply(strsplit(files, "_"), function(x){return(x[which(x=="TO")+1])})))
    
    df<-data.frame(file=files, start=from, end=to)
    df$start<-as.POSIXct(df$start)
    df$end<-as.POSIXct(df$end)
    
    write.csv(df,paste0(projList$path$catalogues, "/data_catalogue.csv" ))
    return(df)
}


