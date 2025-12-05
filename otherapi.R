# -------------------------------------------------------------------------
# HELPER FUNCTIONS API ----------------------------------------------------
# -------------------------------------------------------------------------

KEY <- "test" #@todo

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

#* USER RESULTS TO JSON 
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

#* GET RESULTS IN CSV TABLE
#* @get /get-data
#* @serializer csv
#* @param user Optional user ID or name to filter
#* @param catName
#* @param from Optional start date in YYYY-MM-DD format
#* @param to Optional end date in YYYY-MM-DD format
function(req, res, catName = NULL, user = NULL, from = NULL, to = NULL) {
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  files <- list.files("sessions", full.names = TRUE)
  
  # cat name filtering
  if (!is.null(catName)) {
    files <- files[grepl(catName, basename(files))]
  }
  tmp <- vector("list", length(files))
  for (f in seq_along(files)) {
    tmp[[f]] <- jsonlite::fromJSON(files[f])
  }
  res.df <- as.data.frame(do.call(rbind, tmp))
  
  # Convert list-columns to strings
  res.df[] <- lapply(res.df, function(col) {
    if (is.list(col)) sapply(col, toString) else col
  })
  
  # Date filtering
  if (!is.null(from)) {
    res.df <- res.df[as.Date(res.df$date) >= as.Date(from), ]
  }
  if (!is.null(to)) {
    res.df <- res.df[as.Date(res.df$date) <= as.Date(to), ]
  }
  # User filtering
  if (!is.null(user)) {
    res.df <- res.df[res.df$user == user, ]
  }
  
  res$setHeader(
    "Content-Disposition",
    paste0('attachment; filename="session_data_', format(Sys.time(), "%Y-%m-%d_%H-%M"), '.csv"')
  )
  return(res.df)
}

#* UPLOAD FILES FROM GITHUB PAGE
#* @post /upload
#* @parser multi
function(req, res) {
  
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  upload_dir <- paste0("uploads/", api_key)
  if (!dir.exists(upload_dir)) 
    dir.create(upload_dir, showWarnings = FALSE)
  
  for(i in 1:length(req$body)){
    dest <- file.path(upload_dir, req$body[[i]]$filename)
    writeBin(req$body[[i]]$value, dest)  
  }
  
  list(
    status = "ok",
    saved_as = basename(dest)
  )
}

#* Upload a CAT config JSON
#* @post /cat-config
function(req, res) {
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  save_dir <- paste0("uploads/", api_key)
  if (dir.exists(save_dir)) {
    # if exists create and write the config.csv
    dest <- file.path(save_dir, "config.json")
    config <- jsonlite::fromJSON(req$postBody)
    str(config); cat(dest,"\n") #debug
  } else {
    res$status <- 401
    return(list(error = "Directory not found. Perhaps itembank.csv is also missing."))
  }
  ## --- 
  ## Model cat
  modeltype <- "2PL" #@todo checklist config$irtmodel
  csvdest <- file.path(save_dir, "itembank.csv")
  df <- read.csv(csvdest)
  #@todo str(df) checks!
  if(0==0) { #@todo if Raw Data NOT uploaded..
    # use item parameters to construct the mirt object
    if(1==1) df$d <- -(df$b*df$a) #@todo check button b -> d
    # Create 
    tmp <- cbind('a1' = df$a, 'd' = df$d) #following mirt:: guide
    rownames(tmp) <- df$Name
    mod <- mirtCAT::generate.mirt_object(parameters = tmp, modeltype)
    #print(coef(mod, simplify = TRUE)$items)
    #print(M2(mod))
  }
  if(0==1) { #@todo if Raw Data ...
    # fit a model
  }
  config$fit <- as.list(mirt::M2(mod))
  # write config.json with model fit
  jsonlite::write_json(config, dest, pretty = TRUE, auto_unbox = TRUE) #uto_unbox :ensures single-element vectors are written as scalars (not arrays).)
  # write mirt object to .RDS file
  RDSdest <- file.path(save_dir,"mirt_object.RDS")
  saveRDS(mod, RDSdest)
  
  list(status = "ok")
}


#* @post /df-json
#* @serializer json
function(req, res){
  api_key <- req$HTTP_X_API_KEY
  if (is.null(api_key) || api_key != KEY) {
    res$status <- 401
    return(list(error = "Unauthorized: invalid API key"))
  }
  payload <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  
  #-- run the function 
  make_itembank_df <- function(J,categories,factors,choices){
    exampledf <- data.frame(
      ItemLabel = paste0("X", 1:J),
      Item = paste0("Text for Item ", 1:J)
    )
    for (c in 1:choices) {
      exampledf[[paste0("Choice", c)]] <- paste0("Answer", c)
    }
    # polytmous
    if (categories>2) exampledf$Solution <- paste(0:(categories-1), collapse = "") #or choices
    
    if (categories==2) exampledf$Solution <- paste(c("1", rep("0", choices-1)), collapse = "")
    
    for (f in 1:factors) {
      exampledf[[paste0("F", f)]] <- 0
    }
    exampledf$F1 <- 1
    for (f in 1:factors) {
      exampledf[[paste0("a", f)]] <- 0
    }
    exampledf$a1 <- 1
    for (c in 1:(categories-1)) {
      exampledf[[paste0("b", c)]] <- 0
    }
    exampledf$g <- 0 #0
    exampledf$u <- 1 #1
    return(exampledf)
  }
  return(make_itembank_df(J =          as.numeric(payload$J),
                          categories = as.numeric(payload$categories),
                          factors =    as.numeric(payload$factors),
                          choices =    as.numeric(payload$choices)))
}