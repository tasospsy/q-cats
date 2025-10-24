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
## Env variables
myenv <- new.env()
myenv$ud_se <- NULL # user defined standard error

#* @post /test
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  # Debug: print what Qualtrics sent
  #print(body) 
  se <- as.numeric(body$slider) 
  message("Iter is: ", as.numeric(body$iter))
  myenv$ud_se <- se
  list(se = se)
}
