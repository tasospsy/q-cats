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
##
catName <- "PHQ9" #@todo

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
if(file.exists("uploads/irt-calib-results.RData")){
  load("uploads/irt-calib-results.RData")
  cat("IRT Calibration file is loaded\n")
} else {
  cat("IRT Calibration file is not uploaded\n")
}

if (file.exists("uploads/items_df.csv")) {
  df <- read.csv("uploads/items_df.csv")
  J <- nrow(df)
  cat("CSV file is loaded\n")
} else {
  cat("CSV file is not uploaded\n")
}

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
    next_q = df$Item[next_item_num],
    choices = as.list(as.character(df[next_item_num, 3:6])),
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
    # save responses to RAM
    save_user(userid, user)
    # list to return:
    return(
      list(
        userid = userid,
        iter = user$iter,
        pat = user$pat,
        item_num = next_item_num,
        item = df$Item[next_item_num],
        choices = as.list(as.character(df[next_item_num, 3:6])),
        se_thetahat = current_se,
        thetahat = theta,
        stop = 0
      )
    )} 
  
  if(current_se <= user$stop_crit || sum(!is.na(user$pat)) == J) {# stop! & save
    # save to disc:
    res.df <- list(
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
    write_json(res.df, filepath, pretty = TRUE, auto_unbox = TRUE)
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
