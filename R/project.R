#' create a new project
#' 
#' @param path local project location (parent directory)
#' @param name project name
#' @param dir project directory
#' @param repo gitlab repository name
#' @param url gitlab server url
#' @param google_proj google drive location
#' @param google_path google drive projects path
#' 
#'  
#' before this function can be called, there must be a directory called 
#' \code{local} in the working directory containing two authorization files
#' 
#' * \code{gitlab_token} a personal access token from gitlab with full api
#'   scope
#' * \code{drive_cache} a binary cache file authorizing \code{googledrive} to 
#'   access your drive.  this can be created by running:
#'   \code{googledrive::drive_auth(cache = "local/drive_cache")}
#'   
#' @export
#' 
create_project <- function(name,
                           path = getwd(),
                           dir = name,
                           google_proj = name,
                           google_path = "projects",
                           repo = name,
                           server = "gitlab.com",
                           user = getOption("gitlab_user")){
  
  # check for auth files
  if(! all(file.exists(
    "local/gitlab_token", 
    "local/drive_cache")
  )) stop("missing authorization file(s).  see man page for details.")
  
  # set system-wide options
  options(
    proj_dir = name,
    proj_path = path,
    proj_server = server,
    proj_user = user,
    proj_repo = repo,
    proj_token = read_token("local/gitlab_token"),
    proj_drive_path = google_path,
    proj_drive_name = google_proj,
    httr_oauth_cache = "local/drive_cache"
  )
  
  # create the directory and rproj
  if(dir.exists(paste(path, dir, sep = "/"))){
    if(file.exists(paste(path, dir, paste0(name, ".RProj"), sep = "/")))
      stop("project already exists.")
    devtools::setup(paste(path, dir, sep = "/"))
  } else devtools::create(paste(path, dir, sep = "/"))
  root <- setwd(paste(path, dir, sep = "/"))
  options(proj_root = root)
  dir.create("local")
  file.copy(paste(
    root,
    "local/drive_cache",
    sep = "/"
  ), "local/")
  file.copy(paste(
    root,
    "local/gitlab_token",
    sep = "/"
  ), "local/")
  dir.create("data")
  
  # create options.R
  fh <- file("R/options.R") 
  names(options())[grepl("^proj_", names(options())) & 
                     names(options()) != "proj_token"] %>%
    c("http_oauth_cache") %>% 
    paste(., options()[.], sep = " = ", collapse = ",\n  ") %>% 
    paste("options(\n  ", ., ")") %>%
    writeLines(fh)
  close(fh)
  
  # create the remote
  gitlab_curl("projects", "POST", atts = c(name = repo))
  
  # set up git if in linux
  if(Sys.info()["sysname"] == "Linux"){
    if(system("git init") != 0) die_gracefully("git init failed")
    if(system(
      sprintf("git remote add origin git@%s:%s/%s.git",
              server, user, repo)
      ) != 0) die_gracefully("git remote add origin failed")
    if(system("git add .") != 0) die_gracefully("git add failed")
    if(system("git commit -m \"initial commit\"")) 
      die_gracefully("git commit failed")
    if(system("git push -u origin master") != 0) 
      die_gracefully("git push failed")
  } else warning("i don't know how to set up git on ", Sys.info()$sysname,
                 ".  you're on your own.")
  fh <- file(".gitignore", "a")
  writeLines(c("local/", "data/"), fh)
  close(fh)
  
  
  # create the remote directory on google drive
  # message("authenticate to google drive")
  
  drive_upload_dir(paste(path, dir, sep = "/"), 
                   paste(google_path, dir, sep = "/"),
                   ignore = "local")

  # # make sure our projects directory exists
  # gdir <- googledrive::drive_get(google_path)
  # if(nrow(gdir) == 0 || ! any(grepl("/$", gdir$path))) 
  #    googledrive::drive_mkdir(google_path)
  #    
  # # if our project dir doesn't exist, create it
  # gdir <- googledrive::drive_get(paste(google_path, google_proj))
  # if(nrow(gdir) == 0 || ! any(grepl("/$", gdir$path)))
  #   Sys.sleep(1)
  #   googledrive::drive_mkdir(paste(google_path, google_proj, sep = "/"))
  # 
  # # using github.com/odeke-em/drive/ for now
  # fh <- file(".driveignore", "a")
  # writeLines("local/", fh)
  # close(fh)
  # if(Sys.info()["sysname"] == "Linux"){
  #   # if(system("drive init") != 0)
  #   #    die_gracefully("drive init failed")
  #   if(system(sprintf("drive push -destination %s/%s -no-prompt -force .", 
  #                     google_path, google_proj)) != 0)
  #     die_gracefully("drive push failed")
  # } else warning("i don't know how to set up drive on ", Sys.info()$sysname,
  #                ".  you're on your own.")
}

