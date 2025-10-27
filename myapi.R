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
## READ CAT SPECS
link <- "https://github.com/tasospsy/q-cats/raw/refs/heads/main/calibrations/phq9-irt-cal-results.RData"
load(url(link))
J <- length(questions)
## Env variables
myenv <- new.env()
myenv$ud_se <- NULL # user defined standard error
myenv$qs <- questions # user defined standard error
myenv$mod <- mod

#* @post /test
function(req, res) {
  if (is.null(myenv$pat)) {
    myenv$pat <- rep(NA, J)
    myenv$itembank <- rep(TRUE, J)
  }
  body <- jsonlite::fromJSON(req$postBody)
  se <- as.numeric(body$slider) 
  
  next_item_num <- sample(which(myenv$itembank), 1)
  myenv$itembank[next_item_num] <- FALSE
  
  myenv$ud_se <- se
  
  list(se = se, 
       next_q = myenv$qs[next_item_num], 
       item_num = next_item_num, 
       itembank = myenv$itembank)
}

# cat api -----------------------------------------------------------------

#* @post /cat
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  
  resp <- as.numeric(body$q_resp) 
  item_num <- as.numeric(body$item_num)
  
  myenv$pat[item_num] <- resp
  
  next_item_num <- sample(which(myenv$itembank), 1)
  myenv$itembank[next_item_num] <- FALSE
  
  item <- myenv$qs[next_item_num] # read the item chr
  
  list(pat = myenv$pat, 
       item_num = next_item_num, 
       item = item,
       itembank = myenv$itembank)
} 
