#' create a new project and return its \code{project} object
#' 
#' @param name project name
#' @param path local project location (parent directory)
#' @param dir project directory (\code{name} by convention)
#' @param repo gitlab repository name (\code{name} by convention)
#' @param server gitlab server url
#' @param gitlab_token gitlab personal access token
#' @param drive_proj google drive location
#' @param drive_path google drive projects path
#' @param drive_cache google drive cache location
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
project <- function(name,
                    path = getwd(),
                    dir = name,
                    repo = name,
                    server = "gitlab.com",
                    gitlab_token = read_token("local/gitlab_token"),
                    drive_proj = name,
                    drive_path = "projects",
                    drive_cache = "local/drive_cache",
                    user = getOption("gitlab_user")){
  # check for auth files
  if( ! file.exists(drive_cache)) stop("missing drive_cache file.")
  if( is.null(gitlab_token)) stop("gitlab token is empty")
  if( is.null(user)) stop("need to set user")
  
  # set system-wide options
  opts <- names(options(
    proj_dir = name,
    proj_path = path,
    proj_root = getwd(),
    proj_server = server,
    proj_user = user,
    proj_repo = repo,
    proj_token = gitlab_token,
    proj_drive_path = drive_path,
    proj_drive_name = drive_proj,
    httr_oauth_cache = drive_cache
  ))
  
  # create the directory and rproj
  if(dir.exists(paste(path, dir, sep = "/")))
    if(file.exists(paste(path, dir, paste0(name, ".RProj"), sep = "/")))
      stop("project already exists.")
  rstudioapi::initializeProject(paste(path, dir, sep = "/"))
  root <- setwd(paste(path, dir, sep = "/"))
  dir.create(dirname(drive_cache), recursive = TRUE, showWarnings = FALSE)
  file.copy(paste(
    root,
    drive_cache,
    sep = "/"
  ), dirname(drive_cache))
  dir.create("data")
  
  # create options.R
  fh <- file("local/options.R")
  paste(
    opts, 
    paste0("\"", options()[opts], "\""), 
    sep = " = ", collapse = ",\n  "
  ) %>% 
    paste("options(\n  ", ., "\n)") %>%
    writeLines(fh)
  close(fh)
  
  # create the remote
  message("creating gitlab project ", server, "/", user, "/", repo, ".")
  response <- gitlab_curl("projects", "POST", atts = c(name = repo))
  
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
  writeLines(unique(c("local/", "data/", dirname(drive_cache))), fh)
  close(fh)
  
  # create the remote directory on google drive
  # message("authenticate to google drive")
  
  files <- drive_upload_dir(
    paste(path, dir, sep = "/"), 
    paste(drive_path, drive_proj, sep = "/"),
    ignore = unique(c(dirname(drive_cache), "local", ".git"))
  )
  save(files, file = "local/drive_files.Rdata")

  structure(list(
    git = response, 
    files = files, 
    options = options()[opts]
    ), class = "project"
  )
}

#' open a \code{project} object in rstudio
#'
#' @export
open.project <- function(p, ...)
  rstudioapi::openProject(
    paste(p$options$proj_path,
          p$options$proj_dir, 
          sep = "/"
    ), ...
  )
