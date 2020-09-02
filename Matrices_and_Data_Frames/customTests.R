 # Returns TRUE if the user has calculated a value equal to that calculated by the given expression.
 calculates_same_value <- function(expr){
   e <- get("e", parent.frame())
   # Calculate what the user should have done.
   eSnap <- cleanEnv(e$snapshot)
   val <- eval(parse(text=expr), eSnap)
   passed <- isTRUE(all.equal(val, e$val))
   if(!passed)e$delta <- list()
   return(passed)
 }

 # Get the swirl state
 getState <- function(){
    # Whenever swirl is running, its callback is at the top of its call stack.
    # Swirl's state, named e, is stored in the environment of the callback.
    environment(sys.function(1))$e
 }

 # Retrieve the log from swirl's state
 getLog <- function(){
    getState()$log
 }

 submit_log <- function(){

    p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

    api_url <- "https://04ldlwudkg.execute-api.us-east-2.amazonaws.com"

    log_ <- getLog()
    nrow_ <- max(unlist(lapply(log_, length)))
    log_tbl <- data.frame(user = rep(log_$user, nrow_),
                          name = rep(log_$user, nrow_),
                          course_name = rep(log_$course_name, nrow_),
                          lesson_name = rep(log_$lesson_name, nrow_),
                          question_number = p(log_$question_number, nrow_, NA),
                          correct = p(log_$correct, nrow_, NA),
                          attempt = p(log_$attempt, nrow_, NA),
                          skipped = p(log_$skipped, nrow_, NA),
                          datetime = p(log_$datetime, nrow_, NA),
                          stringsAsFactors = FALSE)
    name <- log_$user
    course <- log_$course_name
    lesson <- log_$lesson_name

    out <- list(
       completed = TRUE,
       name = name,
       course = course,
       lesson = lesson,
       log = log_tbl
    )

    httr::POST(api_url, body = out, encode = "json")

 }
