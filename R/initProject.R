#' Init radio-tracking project
#'
#' @description Creates directories and a R list with all the needed rts information for one animal
#'
#'
#' @author Marvin Ludwig & Jannis Gottwald
#'
#' @param projroot string, project directory
#' @param logger_data_raw string, path to the full RTS dataset
#'
#' @export
#'
#'




initProject = function(projroot = ".",logger_data_raw
){

  rtRoot(projroot)
  # create project sturture
  dir.create(paste0(projroot, "data/catalogues/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "data/logger_data_csv/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/kplv/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/reference_data/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/calibration_curves/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/correction_values/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/individuals/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/batch_awk"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/param_lst"), showWarnings = FALSE)
  dir.create(paste0(projroot, "/data/models"), showWarnings = FALSE)
  dir.create(paste0(projroot, "R/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "results/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "R/scripts/"), showWarnings = FALSE)
  dir.create(paste0(projroot, "R/fun/"), showWarnings = FALSE)
  # save meta data
 
  # get paths
  path = list(raw= logger_data_raw,
              catalogues = paste0(projroot, "data/catalogues/"),
              csv = paste0(projroot, "data/logger_data_csv/"),
              kplv = paste0(projroot, "/data/kplv/"),
              ref= paste0(projroot, "/data/reference_data/"),
              c_Curves= paste0(projroot, "/data/calibration_curves/"),
              correction = paste0(projroot, "/data/correction_values/"),
              ids= paste0(projroot, "/data/individuals/"),
              awk=paste0(projroot, "/data/batch_awk"),
              param_lst=paste0(projroot, "/data/param_lst"),
              models=paste0(projroot, "/data/models"),
              fun=paste0(projroot, "R/fun/")
              )

  project = list(path = path)

  return(project)

}



