#' create a new project
#' 
#' @param name project name
#' @param dir project directory
#' @param path local project location
#' @param repo gitlab repository name
#' @param url gitlab server url
#' @param token gitlab personal access token
#' @param google_proj google drive location
#' @param google_path google drive projects path
#' @export
#' 
create_project <- function(name,
                    dir = name,
                    path = getwd(),
                    google_proj = name,
                    google_path = "projects",
                    repo = name,
                    server = "gitlab.com",
                    token = read_token("local/gitlab_token"),
                    user = getOption("gitlab_user")){
  # set system-wide options
  options(
    proj_dir = name,
    proj_path = path,
    proj_server = server,
    proj_user = user,
    proj_repo = repo,
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
  dir.create("local")
  file.copy(paste(
    root,
    getOption("proj_cred_file","local/credentials.json"),
    sep = "/"
  ), "local/")
  file.copy(paste(
    root,
    getOption("proj_cache", "local/drive_cache"),
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
    if(system("git commit -m \"initial commit\"")) die_gracefully("git commit failed")
    if(system("git push -u origin master") != 0) die_gracefully("git push failed")
  } else warning("i don't know how to set up git on ", Sys.info()$sysname,
                 ".  you're on your own.")
  fh <- file(".gitignore", "a")
  writeLines(c("local/", "data/"), fh)
  close(fh)
  
  
  # create the remote directory on google drive
  # message("authenticate to google drive")
  
  # make sure our projects directory exists
  browser()
  gdir <- googledrive::drive_get(google_path)
  if(nrow(gdir) == 0 || ! any(grepl("/$", gdir$path))) 
     googledrive::drive_mkdir(google_path)
     
  # if our project dir doesn't exist, create it
  gdir <- googledrive::drive_get(paste(google_path, google_proj))
  if(nrow(gdir) == 0 || ! any(grepl("/$", gdir$path)))
    googledrive::drive_mkdir(paste(google_path, google_proj, sep = "/"))

  # using github.com/odeke-em/drive/ for now
  fh <- file(".driveignore", "a")
  writeLines("local/", fh)
  close(fh)
  if(Sys.info()["sysname"] == "Linux"){
    message("authenticate to google, again")
    if(system(paste("drive init --service-account-file", 
                    getOption("proj_cred_file"))) != 0) 
       die_gracefully("drive init failed")
    if(system(sprintf("drive push -destination %s/%s -no-prompt", 
                      google_path, google_proj)) != 0)
      die_gracefully("drive push failed")
  } else warning("i don't know how to set up drive on ", Sys.info()$sysname,
                 ".  you're on your own.")
}

