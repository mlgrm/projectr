#' keep trying to evaluate an expression until it succeeds
#' 
#' @param expr expression to be evaluated
#' @param sleep seconds to sleep between tries
#' @param max_tries maximum number of tries before we throw up our hands in
#' disgust
#' 
#' @details some functions, especially those calling curl, may fail 
#' non-deterministically.  this function repeatedly wraps \code{expr} in a 
#' new promise and evaluates it until it succeeds or \code{max_tries} 
#' is exceeded
#' 
#' @return the value of the successfully evaluated expression.
retry <- function(expr, sleep = 1, max_tries = 10){
  attempts <- 0
  while( attempts < max_tries ){
    attempts <- attempts + 1
    prom <- pryr::uneval(expr)
    env <- parent.frame()
    result <- tryCatch(eval(prom, envir = env), error = identity)
    if(! is(result,"error")) return(result)
    message("attempt ", attempts, " failed: ")
    cat("\t")
    message(result$message)
    Sys.sleep(sleep)
  }
  stop("maximum number of attempts exceeded")
}
  
may_fail <- function(expr, prob = 0.5){
  ran <- runif(1)
  if(ran<prob) stop("failed") else expr
}