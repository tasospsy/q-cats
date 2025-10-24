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
# Make Question and Choices characters
questions <- c(
  "Little interest or pleasure in doing things",
  "Feeling down, depressed, or hopeless",
  "Trouble falling or staying asleep, or sleeping too much",
  "Feeling tired or having little energy",
  "Poor appetite or overeating",
  "Feeling bad about yourself or that you are a failure or  have let yourself or your family down",
  "Trouble concentrating on things, such as reading the  newspaper or watching television",
  "Moving or speaking so slowly that other people could have noticed.  Or the opposite    being so figety or restless that you have been  moving around a lot more than usual",
  "Thoughts that you would be better off dead, or of hurting yourself"
)
J <- length(questions)
## Env variables
myenv <- new.env()
myenv$ud_se <- NULL # user defined standard error
myenv$qs <- questions # user defined standard error


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
