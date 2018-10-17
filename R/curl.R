#' do a curl command with default settings for gitlab
#' 
#' @export
gitlab_curl <- function(endpoint, method = "GET", atts = NULL){
  h <- 
    curl::new_handle() %>% 
    curl::handle_setheaders(
      `Private-Token`=getOption("proj_token")
    )
  if(! method %in% c("GET", "POST")) 
    h %<>% curl::handle_setopt(customrequest = method)
  url <- paste0(
    ifelse(grepl("^https://", getOption("proj_server")), 
           "", "https://"),
    getOption("proj_server"),
    "/api/v4/",
    endpoint
  )
  if(!is.null(atts))
    if(method == "POST") 
      h %<>% curl::handle_setopt(
        copypostfields = att_string(atts)
      ) else url %<>% paste(att_string(atts), sep = "?")
  res <- curl::curl_fetch_memory(url, h)
  if(res$status_code >= 300){
    warning("status code:\n", rawToChar(res$content))
    return(res)
  } else message("server returned status code: ", res$status_code)
  res$content %>%  
    rawToChar %>% 
    jsonlite::fromJSON()
}

att_string <- function(atts){
  paste(names(atts), atts, sep = "=", collapse = "&")
}

gitlab_token <- function(path = "local/token"){
  if(is.null(getOption("proj_token"))) 
    options(proj_token = read_token(path))
  getOption("proj_token")
}

read_token <- function(path = "local/token"){
  con <- file(path)
  t <- readLines(con, n = 1, warn = FALSE)
  close(con)
  t
}