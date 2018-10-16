#' upload a local directory to google drive, recursively
#' 
#' @param from local directory path
#' @param to remote directory path (not parent)
#' 
#' if \code{to} exists, the contents of \code{from} will be copied into 
#' \code{to}, otherwise \code{to} will first be created.  if a file in \from
#' already exists in \code{to}, it will be updated, otherwise it will be 
#' uploaded.  subdirectories will be recursed
drive_upload_dir <- function(from, to = from, 
                             ignore = NULL, 
                             regex = FALSE,
                             is_new = FALSE){
  # this will be updated if not is_new
  remote_files <- NULL
  if(! is_new){
    remote <- googledrive::drive_get(to)
    remote <- remote[!is_trashed(remote),]
    # if to doesn't resolve to any directory, we don't need to check_exists
    if(! any(is_dir(remote)))
      return(drive_upload_dir(from, to, ignore, regex, is_new = TRUE))
    remote <- remote[is_dir(remote), ]
    if(nrow(remote)!=1) stop("non-unique remote, n=",nrow(remote))
    remote_files <- googledrive::drive_ls(remote)
  } else remote <- googledrive::drive_mkdir(to) # if is_new, make new directory

  # define a function to ignore ignore
  if(! is.null(ignore)){
    if(regex) stop("regex not implemented yet") else
      ignore_fun <- function(f)f[! basename(f) %in% ignore] 
  } else ignore_fun <- identity
  
  # go through the files
  list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE) %>% 
    ignore_fun %>% 
    llply(function(f){
      # recurse on subdirectories
      if(file_test("-d",f)) 
        drive_upload_dir(f,
                         paste(to, basename(f), sep = "/"), 
                         ignore, 
                         regex, 
                         is_new
        ) else if( is_new || ! basename(f) %in% remote_files$name )
          googledrive::drive_upload(f, remote) else{
            remote <- remote_files[remote_files$name == basename(f), ]
            if(nrow(remote) != 1) stop("non-unique remote, n=", nrow(remote))
            googledrive::drive_update(remote, f)
          }
    }) %>% dplyr::bind_rows()
  
}
  
  
is_dir <- function(df)
  laply(df$drive_resource, getElement, name = "mimeType") == 
    "application/vnd.google-apps.folder" &
    ! laply(df$drive_resource, getElement, name = "trashed")

is_trashed <- function(df)
  laply(df$drive_resource, getElement, name = "trashed")