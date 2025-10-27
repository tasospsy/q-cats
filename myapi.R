# Load libraries
library(plumber)
library(jsonlite)

# SETTINGS  ---------------------------------------------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, x-api-key")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())  # preflight response
  }
  
  forward()
}

#* @filter check_api_key
function(req, res) {
  token <- req$HTTP_X_API_KEY
  if (is.null(token) || token != "MmO1HH7Oeer6iZjgNsLsjENOmXWTJtmq2oPJXhla") {
    res$status <- 401
    return(list(error = "Unauthorized"))
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
## Env variables
myenv <- new.env()
myenv$stop_crit <- NULL # user defined standard error
myenv$qs <- questions # user defined standard error
myenv$mod <- mod
myenv$iter <- 0L
myenv$catdesign <- NULL

#* @post /init
function(req, res) {
  if (is.null(myenv$pat)) {
    myenv$pat <- rep(NA, J)
    myenv$itembank <- rep(TRUE, J)
  }
  body <- jsonlite::fromJSON(req$postBody)
  stop_crit <- as.numeric(body$slider) 
  myenv$stop_crit <- stop_crit
  ## Algorithm
  # w/out stopping rule
  CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE, 
                       start_item = 'MI', design = list(min_items = J))
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  myenv$itembank[next_item_num] <- FALSE
  myenv$catdesign <- CATdesign
  myenv$iter = myenv$iter + 1L
  list(iter = myenv$iter,
       stop_crit = stop_crit, 
       next_q = myenv$qs[next_item_num], 
       item_num = next_item_num, 
       itembank = myenv$itembank)
}

# cat api -----------------------------------------------------------------

#* @post /cat
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  
  resp <- as.numeric(body$q_resp) # a
  item_num <- as.numeric(body$item_num) # b
  ## pass the response to the algorithm
  # update CAT:
  CATdesign <- mirtCAT::updateDesign(myenv$catdesign, 
                                     new_item = item_num, # b
                                     new_response = resp) # a
  #iter <- sum(!is.na(CATdesign$person$items_answered))
  myenv$pat <- CATdesign$person$responses
  theta <- round(as.numeric(CATdesign$person$thetas),3) # current theta estimate
  current_se <- round(as.numeric(CATdesign$person$thetas_SE_history[myenv$iter + 1L,]),2)
  ## next item:
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  #myenv$itembank[next_item_num] <- FALSE
  myenv$catdesign <- CATdesign
  
  list(iter = myenv$iter + 1L,
       pat = myenv$pat, 
       item_num = next_item_num, 
       item = myenv$qs[next_item_num], # read and post the item chr,
       se_thetahat = current_se,
       thetahat = theta)
} 
