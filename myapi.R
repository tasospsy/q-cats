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

## Env variables
myenv <- new.env()
myenv$ud_se <- NULL # user defined standard error
myenv$qs <- questions # user defined standard error

#* @post /test
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  # Debug: print what Qualtrics sent
  #print(body) 
  se <- as.numeric(body$slider) 
  message("\nIter is: ", as.numeric(body$iter))
  
  next_q_number <- sample(1:9,1)
  next_q <- myenv$qs[next_q_number]
  message(paste("\n", next_q, "\n"))
  myenv$ud_se <- se
  
  list(se = se, next_q = next_q)
}
