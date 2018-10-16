die_gracefully <- function(...){
  cleanup()
  stop(...)
}

cleanup <- function(){
  setwd("~/R/projectr")
  local_dir <- paste(getOption("proj_path"), 
                     getOption("proj_dir"), 
                     sep = "/"
  )
  if(file.exists(local_dir))
    unlink(local_dir, recursive = TRUE)
  endpoint <- sprintf("projects/%s%%2F%s", 
                      getOption("proj_user"), 
                      getOption("proj_repo")
  )
  gitlab_curl(endpoint, "DELETE")
  Sys.sleep(1)
  drive_dir <- paste(getOption("proj_drive_path"), 
                     getOption("proj_drive_name"), 
                     sep = "/"
  )
  tryCatch(googledrive::drive_trash(drive_dir), error = identity)
}