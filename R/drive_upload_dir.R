#' upload a local directory to google drive, recursively
#' 
#' @param from local directory path
#' @param to remote directory path (not parent)
#' @param team_drive if \code{to} is on a team drive specify it here.
#' @param ignore character vector files/folders to ignore, like with .gitignore.
#' defaults to the contents of \code{".driveignore"} in the current directory
#' @param regex whether to interpret \code{ignore} with regex, not yet 
#' implemented
#' 
#' if \code{to} is a string and the path exists, the contents of \code{from} 
#' will be copied into 
#' \code{to}, otherwise \code{to} will first be created.  if a file in 
#' \code{from}
#' already exists in \code{to}, it will be updated, otherwise it will be 
#' uploaded.  subdirectories will be recursed
#' 
#' @export
drive_upload_dir <- function(from, to = from, 
                             team_drive = NULL,
                             ignore = NULL,
                             regex = FALSE,
                             is_new = FALSE){
  if(is.null(ignore) && file.exists(".driveignore")) 
    ignore = readLines(".driveignore")
  if(is.character(to) && !grepl("^/", to)) to <- paste0("/", to)
  # this will be updated if not is_new
  remote_files <- NULL
  if(! is_new){
    remote <- untrashed(retry(
      googledrive::drive_get(to, team_drive = team_drive)
    ))
    # if to doesn't resolve to any directory, we don't need to check_exists
    if(! any(is_dir(remote)))
      return(drive_upload_dir(from, to, 
                              team_drive = team_drive, 
                              ignore = ignore, 
                              regex = regex, 
                              is_new = TRUE))
    remote <- remote[is_dir(remote), ]
    if(nrow(remote)!=1) stop("non-unique remote, n=",nrow(remote))
    remote_files <- retry(googledrive::drive_ls(remote, 
                                                team_drive = team_drive))
    # if is_new, make new directory
  } else {
    remote <- retry(googledrive::drive_mkdir(
      basename(to), parent = untrashed(googledrive::drive_get(dirname(to), 
                                                    team_drive = team_drive))
    ))
  }
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
                         team_drive = team_drive,
                         ignore, 
                         regex, 
                         is_new
        ) else if( is_new || ! basename(f) %in% remote_files$name )
          retry(googledrive::drive_upload(f, remote)) else{
            remote <- remote_files[remote_files$name == basename(f), ]
            if(nrow(remote) != 1) stop("non-unique remote, n=", nrow(remote))
            retry(googledrive::drive_update(remote, f))
          }
    }) %>% dplyr::bind_rows()
}

#' backup local data to drive
#' 
#' @export
backup <- function(ignore = c("local", ".git"), team_drive = NULL){
  files <- drive_update_dir(
    from = paste(getOption("proj_path"), getOption("proj_dir"), sep = "/"),
    to = paste(getOption("proj_drive_path"), 
               getOption("proj_drive_name"), sep = "/"),
    team_drive = team_drive,
    ignore = ignore
  )
  save(files, file = "local/drive_files.Rdata")
}
  
  
is_dir <- function(df)
  laply(df$drive_resource, getElement, name = "mimeType") == 
    "application/vnd.google-apps.folder" &
    ! laply(df$drive_resource, getElement, name = "trashed")

is_trashed <- function(df)
  laply(df$drive_resource, getElement, name = "trashed")

untrashed <- function(df)df[! is_trashed(df),]