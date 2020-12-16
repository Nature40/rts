#' Init Animal Tracking
#'
#' @description Creates directories and a R list with all the needed rts information for one animal
#'
#'
#' @author Marvin Ludwig & Jannis Gottwald
#'
#'
#' @param projList list, generatet by initProject function
#' @param animalID string, label of the tagged individual
#' @param freq num, tag frequency (khz)
#' @param start string, start of tracking YYYY-MM-DD
#' @param end string, end of tracking YYYY-MM-DD
#' 
#'
#' @export
#'
#'




initAnimal = function(projList,
                      animalID,
                      freq,
                      start,
                      end
){

  # create project sturture
  dir.create(paste0(projList$path$ids, animalID), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/filtered_awk"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/filtered"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/logger_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/station_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/imputed"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/calibrated"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/gps_timematch"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/bearings"), showWarnings = FALSE)
  dir.create(paste0(projList$path$ids, animalID, "/triangulations"), showWarnings = FALSE)
  

  # save meta data
  meta = list(animalID = animalID,
                    freq = freq,
                    start = start,
                    end = end)
  # get paths
  path = list(raw = projList$path$csv,
              root = paste0(projList$path$ids, animalID),
              filtered_awk=paste0(projList$path$ids, animalID, "/filtered_awk"),
              filtered=paste0(projList$path$ids, animalID, "/filtered"),
              logger_timematch=paste0(projList$path$ids, animalID, "/logger_timematch"),
              station_timematch=paste0(projList$path$ids, animalID, "/station_timematch"),
              imputed=paste0(projList$path$ids, animalID, "/imputed"),
              calibrated=paste0(projList$path$ids, animalID, "/calibrated"),
              gps_matched=paste0(projList$path$ids, animalID, "/gps_timematch"),
              bearings=paste0(projList$path$ids, animalID, "/bearings"),
              triangulations=paste0(projList$path$ids, animalID, "/triangulations")
              )

  animal = list(meta = meta,
                path = path)

  return(animal)

}



