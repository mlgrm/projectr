#' upload a local directory to google drive, recursively
#' 
#' @param from local directory path
#' @param to remove directory path
#' 
drive_update_dir <- function(from, to = from){
  remote <- googledrive::drive_get(to)
}