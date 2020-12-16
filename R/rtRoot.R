#' Initialise a Radiotracking Project
#'
#' @export
#'



rtRoot = function(projroot = "."){

  dirCreate <- function(x) {
    x <- file.path(projroot, x)
    if(! file.exists(x)) dir.create(x) else warning("Directory '", x, "' already exists.")
  }

  if(projroot != ".") dirCreate("")

  dirCreate("data")




}

