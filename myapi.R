# Load libraries
library(plumber)
library(jsonlite)

# SETTINGS  ---------------------------------------------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, x-api-key, userid")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())  # preflight response
  }
  
  forward()
}

# @todo read the api keys list and alter th efollowing to check 
# if the key is inside th elist
# for now:
KEY <- "test"

#* @filter check_api_key
function(req, res) {
  token <- req$HTTP_X_API_KEY
  if (is.null(token) || nchar(token) == 0 || token != KEY) {
    res$status <- 401
    return(list(error = "Missing or invalid key"))
  }
  forward()
}
# END SETTINGS ------------------------------------------------------------

# CAT specs ---------------------------------------------------------------
library(mirt)
library(mirtCAT)
## READ CAT SPECS
link <- "https://github.com/tasospsy/q-cats/raw/refs/heads/main/calibrations/phq9-irt-cal-results.RData"
load(url(link))
J <- length(questions)

## Global settings
sessions <- new.env(parent = emptyenv())

# ------------------------------------------------------------
#  HELPER FUNS
# ------------------------------------------------------------
get_user <- function(userid) sessions[[userid]]
save_user <- function(userid, obj) sessions[[userid]] <- obj

## API SETTINGS

#* @post /init
function(req, res) {
  body <- fromJSON(req$postBody)
  stop_crit <- as.numeric(body$slider)
  
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  # create new session if none exists
  user <- sessions[[userid]]
  if (is.null(user)) {
    user <- list(
      iter = 0L,
      pat = rep(NA, J),
      itembank = rep(TRUE, J),
      stop_crit = stop_crit,
      catdesign = NULL
    )
  }
  
  ## Algorithm
  # w/out stopping rule
  CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE, 
                       start_item = 'MI', design = list(min_items = J))
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  
  user$catdesign <- CATdesign
  user$itembank[next_item_num] <- FALSE
  user$iter <- user$iter + 1L
  user$stop_crit <- stop_crit
  
  # save to RAM
  save_user(userid, user)
  # save to disc:
  # not needed?
  
  list(
    userid = userid,
    iter = user$iter,
    stop_crit = stop_crit,
    next_q = questions[next_item_num],
    item_num = next_item_num,
    itembank = user$itembank
  )
}

# cat api -----------------------------------------------------------------

#* @post /cat
function(req, res) {
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  
  user <- get_user(userid)
  if (is.null(user)) {
    res$status <- 404
    return(list(error = "Session not found; call /init first"))
  }
  
  body <- jsonlite::fromJSON(req$postBody)
  
  resp <- as.numeric(body$q_resp) # a
  item_num <- as.numeric(body$item_num) # b
  ## pass the response to the algorithm
  # update CAT:
  CATdesign <- mirtCAT::updateDesign(user$catdesign,
                                     new_item = item_num, # b
                                     new_response = resp) # a

  user$catdesign <- CATdesign
  user$pat <- CATdesign$person$responses
  
  theta <- round(as.numeric(CATdesign$person$thetas), 3)
  cat(CATdesign$person$thetas_SE_history) #debug
  current_se <- round(as.numeric(
    CATdesign$person$thetas_SE_history[user$iter + 1L, ]
  ), 2)
  
  user$iter <- user$iter + 1L
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  next_item_text <- questions[next_item_num]
  
  # save to RAM
  save_user(userid, user)
  # save to disc:
  df <- list(
    userid = userid,
    responses = user$pat,
    theta = theta,
    se = current_se,
    timestamp = Sys.time()
  )
  if(!dir.exists("sessions")) dir.create("sessions", showWarnings = FALSE)
  filepath <- file.path("sessions", paste0(
    userid, "_PHQ9", 
    "_session.json"
  ))
  write_json(df, filepath, pretty = TRUE, auto_unbox = TRUE)
  
  list(
    userid = userid,
    iter = user$iter,
    pat = user$pat,
    item_num = next_item_num,
    item = next_item_text,
    se_thetahat = current_se,
    thetahat = theta
  )
} 

#* @get /download
function(req, res) {
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  
  pattern <- paste0("^", userid, "_.*_session\\.json$")
  files <- list.files("sessions", pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    res$status <- 404
    return(list(error = paste("No session file found for user:", userid)))
  }
  
  #@todo for multiple files (zip?)!
  for(fl in files) plumber::include_file(fl)
}
