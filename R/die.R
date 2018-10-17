die_gracefully <- function(...){
  cleanup()
  stop(...)
}

#' clean up after a failed \code{create_project} run
#' 
#' @export
cleanup <- function(){
  setwd(getOption("proj_root", getwd()))
  local_dir <- paste(getOption("proj_path"), 
                     getOption("proj_dir"), 
                     sep = "/"
  )
  message("removing local directory ", local_dir)
  if(file.exists(local_dir))
    unlink(local_dir, recursive = TRUE)
  endpoint <- sprintf("projects/%s%%2F%s", 
                      getOption("proj_user"), 
                      getOption("proj_repo")
  )
  message("deleting project", 
          getOption("proj_server"),"/", 
          getOption("proj_user"),"/",
          getOption("proj_user")
  )
  res <- gitlab_curl(endpoint, "DELETE")
  Sys.sleep(1)
  message("deleting folder ",
          getOption("proj_drive_path"),"/",
          getOption("proj_drive_name"),
          " from drive"
  )
  drive_dir <- paste(getOption("proj_drive_path"), 
                     getOption("proj_drive_name"), 
                     sep = "/"
  )
  tryCatch(googledrive::drive_trash(drive_dir), error = identity)
}