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
catName <- "PHQ9"

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
    user <- list(
      iter = 0L,
      pat = rep(NA, J),
      itembank = rep(TRUE, J),
      stop_crit = stop_crit,
      catdesign = NULL
    )
  
  ## Algorithm
  CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE, 
                       start_item = 'MI', 
                       design = list(min_items = J)) #v0.1 w/ stop on Qualtrics
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  
  user$catdesign <- CATdesign
  user$itembank[next_item_num] <- FALSE
  user$iter <- user$iter + 1L
  user$stop_crit <- stop_crit
  
  # save to RAM
  save_user(userid, user)
  
  list(
    userid = userid,
    iter = user$iter,
    stop_crit = user$stop_crit,
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
  current_se <- round(as.numeric(
    CATdesign$person$thetas_SE_history[user$iter + 1L, ]
  ), 2)
  cat(userid, ": SE of theta est. is ",current_se,"\n")
  
  
  if(current_se > user$stop_crit) { #Continue:
    next_item_num <- mirtCAT::findNextItem(CATdesign)
    user$iter <- user$iter + 1L
    next_item_text <- questions[next_item_num]
    # save responses to RAM
    save_user(userid, user)
    # list to return:
    return(
      list(
        userid = userid,
        iter = user$iter,
        pat = user$pat,
        item_num = next_item_num,
        item = next_item_text,
        se_thetahat = current_se,
        thetahat = theta,
        stop = 0
      )
    )} 
  
  if(current_se <= user$stop_crit || sum(!is.na(user$pat)) == J) {# stop! & save
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
      userid, "_", catName, 
      "_session.json"
    ))
    write_json(df, filepath, pretty = TRUE, auto_unbox = TRUE)
    return(
      list(
        userid = userid,
        iter = user$iter,
        pat = user$pat,
        item_num = NA,
        item = NA,
        se_thetahat = current_se,
        thetahat = theta,
        stop = 1
      )
    )} 
} 

#* @get /user-results
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
  
  patt <- paste0("^", userid, "_.*_session\\.json$")
  file <- list.files("sessions", pattern = patt, full.names = TRUE)
  if (length(file) == 0) {
    res$status <- 404
    return(list(error = paste("No session file found for user:", userid)))
  }
  if (length(file) > 1) {
    res$status <- 404
    return(list(error = paste("Multiple sessions for user:", userid)))
  }
  jsonlite::fromJSON(file) #just return it as list
}

#* @get /get-data
#* @serializer csv
function(req, res) {
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  
  files <- list.files("sessions", full.names = TRUE)
  tmp <- list()
  for(f in 1:length(files)) tmp[[f]] <- jsonlite::fromJSON(files[f])
  res.df <- as.data.frame(do.call(rbind, tmp))
  # list-columns to comma-separated strings
  res.df[] <- lapply(res.df, function(col) {
    if (is.list(col)) sapply(col, toString) else col
  })
  
  res$setHeader(
    "Content-Disposition",
    paste0('attachment; filename="session_data_', format(Sys.time(), "%Y-%m-%d_%H-%M"), '.csv"')
  )
  return(res.df)
}

#* @post /upload
#* @parser multi
function(req, res) {
  
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  upload_dir <- "uploads"
  if (!dir.exists(upload_dir)) 
    dir.create(upload_dir, showWarnings = FALSE)
  
  #upload <- read.csv(text=rawToChar(req$body[[1]]$value))
  print(str(req$body[[1]]))
  
  #dest <- file.path("uploads", req$body[[1]]$filename)
  #write.csv(dest, showWarnings = FALSE)
  
  list(
    status = "ok",
    saved_as = basename(dest)
  )
}


