# -------------------------------------------------------------------------
# HELPER FUNCTIONS API ----------------------------------------------------
# -------------------------------------------------------------------------
#* USER RESULTS TO JSON 
KEY <- "test" #@todo
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

#* UPLOAD FILES FROM GITHUB PAGE
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
  
  for(i in 1:length(req$body)){
    dest <- file.path("uploads", req$body[[i]]$filename)
    writeBin(req$body[[i]]$value, dest)  
  }
  
  list(
    status = "ok",
    saved_as = basename(dest)
  )
}
