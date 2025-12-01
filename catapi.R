# Load libraries
library(plumber)
library(jsonlite)
library(mirt)
library(mirtCAT)
# @todo read the api keys list and alter th efollowing to check 
# if the key is inside th elist
# for now:
KEY <- "test"
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

#* @filter check_api_key
function(req, res) {
  token <- req$HTTP_X_API_KEY
  if (is.null(token) || nchar(token) == 0 || token != KEY) {
    res$status <- 401
    return(list(error = "Missing or invalid key"))
  }
  forward()
}

## Global settings
sessions <- new.env(parent = emptyenv())

# ------------------------------------------------------------
#  HELPER FUNS
# ------------------------------------------------------------
get_user <- function(catName, userid) sessions[[catName]][[userid]]
save_user <- function(catname, userid, obj) sessions[[catName]][[userid]] <- obj

## API SETTINGS

#* @post /init
function(req, res) {
  key <- req$HTTP_X_API_KEY
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  catName <- req$HTTP_CATNAME
  if (is.null(catName) || catName == "") {
    res$status <- 400
    return(list(error = "Missing catName header"))
  }
  ## -- read config if exists
  dir <- paste0("uploads/", key)
  configdest <- file.path(dir, "config.json")
  if(file.exists(configdest)){
    config <- jsonlite::fromJSON(dest) 
    cat("config is loaded\n")
  } else {
    res$status <- 400
    return(list(error = "config file not found. Follow Step 2 at https://webcat.github.io/ ."))
  }
  ## -- read csv if exists
  csvdest <- file.path(dir, "itembank.csv")
  if (file.exists(csvdest)) {
    df <- read.csv(csvdest)
    J <- nrow(df)
    cat("Itembank is loaded\n")
  } else {
    res$status <- 400
    return(list(error = "Itembank .csv file not found. Follow Step 1 at https://webcat.github.io/ ."))
  }
  ## -- read mirt object if exists
  RDSdest <- file.path(dir, "mirt_object.RDS")
  if (file.exists(RDSdest)) {
    mod <- readRDS(RDSdest)
    cat("mirt object is loaded\n")
  } else {
    res$status <- 400
    return(list(error = "CAT is not configured."))
  }
  ## check if catName in config == catName on Qualtrics 
  if (catName != config$catName) {
    res$status <- 400
    return(list(error = "CatName does not match with config file."))
  }
  
  if(config$catType == "variable") stop_crit <- as.numeric(config$minSE)
  if(config$catType == "fixed") stop_crit <- as.numeric(config$maxItems)
  
  # Initialize new user session structure
  user <- list(
    iter = 0L,
    pat = rep(NA, J),
    #itembank = rep(TRUE, J),
    stop_crit = stop_crit,
    catdesign = NULL
  )
  
  # Algorithm setup
  CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE, 
                       start_item = 'MI', 
                       design = list(min_items = J))
  
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  
  user$catdesign <- CATdesign
  user$itembank[next_item_num] <- FALSE
  user$iter <- user$iter + 1L
  
  save_user(catName, userid, user)
  
  list(
    userid = userid,
    iter = user$iter,
    #stop_crit = user$stop_crit,
    next_q = df$Item[next_item_num],
    choices = as.character(df[next_item_num, 3:6]),
    item_num = next_item_num,
    #itembank = user$itembank
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
  ## catName?
  user <- get_user(catName, userid)
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
 # resp in qualtrics??????????
  
  user$catdesign <- CATdesign
  user$pat <- CATdesign$person$responses
  
  theta <- round(as.numeric(CATdesign$person$thetas), 3)
  current_se <- round(as.numeric(
    CATdesign$person$thetas_SE_history[user$iter + 1L, ]
  ), 2)
  cat(userid, ": SE of theta est. is ",current_se,"\n")
  
  terminate <- FALSE
  if(config$catType == "variable"){
    if(current_se <= user$stop_crit || sum(!is.na(user$pat)) == J) {
      terminate <- TRUE
    }}
  if(config$catType == "fixed"){
    if(sum(!is.na(user$pat)) == user$stop_crit){
      terminate <- TRUE
    }
  }

  if(terminate) {# stop & save
    # save to disc:
    user.res.df <- list(
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
    write_json(user.res.df, filepath, pretty = TRUE, auto_unbox = TRUE)
    return(
      list(
        userid = userid,
        iter = user$iter,
        pat = user$pat,
        item_num = NA,
        item = NA,
        se_thetahat = current_se,
        thetahat = theta,
        stop = 1 #@todo I have two dummy vars for the same thing, stop and terminate
      )
    )} else { #terminate = FALSE
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
          choices = as.character(df[next_item_num, 3:6]), #@todo
          se_thetahat = current_se,
          thetahat = theta,
          stop = 0
        )
      )}
}

