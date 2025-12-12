# -------------------------------------------------------------------------
# HELPER FUNCTIONS API ----------------------------------------------------
# -------------------------------------------------------------------------
# Load libraries
library(plumber)
library(jsonlite)
library(mirt)
library(mirtCAT)


# Settings ----------------------------------------------------------------
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

# Check Api Key Global
##* @filter check_api_key
#function(req, res) {
#  api_key <- req$HTTP_X_API_KEY
#  known_keys <- readLines("~/genKeys.txt")
#  if (is.null(api_key) || !api_key %in% known_keys) {
#    res$status <- 401
#    return(list(error = "Missing or invalid key"))
#  }
#  forward()
#}

# Home /personal-key --------------------------------------------------------------------
#*  Generate Personal Key
#*  @get /personal-key
function(req, res){
  pool <- list(LETTERS,
               letters)
  KEY <- paste(replicate(n = 5, paste(sapply(pool,function(x) sample(x,1,replace = TRUE)),
                                      collapse = ""), 
                         simplify = FALSE), 
               collapse = "")
  
  new_dir <- paste0("~/", KEY) # create KEY-dir
  dir.create(new_dir, showWarnings = FALSE)
  
  txt_file_path <- file.path("~", "genKeys.txt") # write the KEy to main txt
  dest <- file(txt_file_path, "ab")
  writeBin(charToRaw(paste0(KEY, "\n")), dest)
  close(dest)
  return(KEY)
}

# Step 0 /itembank-blueprint ------------------------------------------------------------------
#* @post /itembank-blueprint
#* @serializer csv
function(req, res){
  payload <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  
  #-- run the function 
  make_itembank_df <- function(J,itemType,factors,responseOptions){
    if(itemType == "Dichotomous Items") categories <- 2
    if(itemType == "Polytomous Items") categories <- responseOptions
    exampledf <- data.frame(
      ItemLabel = paste0("X", 1:J),
      Item = paste0("Text for Item ", 1:J)
    )
    for (c in 1:responseOptions) {
      exampledf[[paste0("Choice", c)]] <- paste0("Option", c)
    }
    if (categories>2) exampledf$Scores <- as.character(paste(0:(categories-1), collapse = "")) 
    if (categories==2) {
      exampledf$Scores <- paste(c("1", rep("0", responseOptions-1)), collapse = "")
    }
    for (f in 1:factors) exampledf[[paste0("a", f)]] <- NA
    for (c in 1:(categories-1)) exampledf[[paste0("d", c)]] <- NA
    if (categories==2) names(exampledf)[names(exampledf) == "d1"] <- "d"
    return(exampledf)
  } # end of function
  
  res <- make_itembank_df(J =   as.numeric(payload$J),
                          itemType = payload$itemType,
                          factors =    as.numeric(payload$factors),
                          responseOptions =    as.numeric(payload$responseOptions))
  attr(res, "plumber") <- list(serializer = serializer_csv(quote = TRUE))
  return(res)
}



# Step 1 /upload ------------------------------------------------------------------
#* @post /upload
#* @parser multi
function(req, res) {
  
  api_key <- req$HTTP_X_API_KEY
  known_keys <- trimws(readLines("~/genKeys.txt"))
  if (is.null(api_key) || !api_key %in% known_keys) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  upload_dir <- paste0(api_key,"/uploads")
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
# Step 2 /cat-config ------------------------------------------------------------------
#* Upload a CAT config JSON
#* @post /cat-config
function(req, res) {
  api_key <- req$HTTP_X_API_KEY
  known_keys <-trimws(readLines("~/genKeys.txt"))
  if (is.null(api_key) || !api_key %in% known_keys) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  save_dir <- paste0(api_key,"/uploads")
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
  modeltype <- config$model 
  csvdest <- file.path(save_dir, "itembank.csv")
  df <- read.csv(csvdest, colClasses = c(Scores = "character"))
  str(df) #debug
  if(0==0) { #@todo if Raw Data NOT uploaded..
    # use item parameters to construct the mirt object
    nms <- names(df)
    if(config$reparam == "yes") {
      A <- unlist(ifelse(ncol(df[nms[grepl("^a", nms)]])>1, rowSums(df[nms[grepl("^a", nms)]]), df[nms[grepl("^a", nms)]]))
      df[nms[grepl("^d", nms)]] <- apply(df[nms[grepl("^d", nms)]], 2, function(di) -(di*A)) #@todo check button b -> d
    }
    tmp <- df[nms[grepl("^(d|a)", nms)]]
    rownames(tmp) <- df$ItemLabel
    mod <- mirtCAT::generate.mirt_object(parameters = tmp, modeltype)
  }
  if(0==1) { #@todo if Raw Data ...
    # fit a model
  }
  #config$fit <- as.list(mirt::M2(mod))
  # write config.json with model fit
  jsonlite::write_json(config, dest, pretty = TRUE, auto_unbox = TRUE) #uto_unbox :ensures single-element vectors are written as scalars (not arrays).)
  # write mirt object to .RDS file
  RDSdest <- file.path(save_dir,"mirt_object.RDS")
  saveRDS(mod, RDSdest)
  
  list(status = "ok")
}
# Step 3 /init & /cat------------------------------------------------------------------
## Global settings
sessions <- new.env(parent = emptyenv())
get_user <- function(catname, userid) sessions[[catname]][[userid]]
save_user <- function(catname, userid, obj) sessions[[catname]][[userid]] <- obj

#* @post /init
function(req, res) {
  key <- req$HTTP_X_API_KEY
  known_keys <- readLines("~/genKeys.txt")
  if (is.null(key) || !key %in% known_keys) {
    res$status <- 401
    return(list(error = "Missing or invalid key"))
  }
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  catName <- as.character(req$HTTP_CATNAME)
  if (is.null(catName) || catName == "") {
    res$status <- 400
    return(list(error = "Missing catName header"))
  }
  ## -- read config if exists
  dir <- paste0("~/",key,"/uploads")
  configdest <- file.path(dir, "config.json")
  if(file.exists(configdest)){
    config <- jsonlite::fromJSON(configdest) 
    cat("config is loaded\n")
  } else {
    res$status <- 400
    return(list(error = "config file not found. Follow Step 2 at https://webcat.github.io/ ."))
  }
  ## -- read csv if exists
  csvdest <- file.path(dir, "itembank.csv")
  if (file.exists(csvdest)) {
    df <- read.csv(csvdest, colClasses = c(Scores = "character"))
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
    stop_crit = stop_crit,
    catdesign = NULL,
    config = config,
    df = df,
    J = J
  )
  
  # Algorithm setup
  CATdesign <- mirtCAT(mo = mod, criteria = 'MI', design_elements = TRUE, 
                       start_item = 'MI', 
                       design = list(min_items = J))
  
  next_item_num <- mirtCAT::findNextItem(CATdesign)
  
  user$catdesign <- CATdesign
  user$iter <- user$iter + 1L
  
  save_user(catName, userid, user)
  
  list(
    userid = userid,
    iter = user$iter,
    #stop_crit = user$stop_crit,
    next_q = df$Item[next_item_num],
    choices = as.character(df[next_item_num, names(df)[grepl("Choice", names(df))]]),
    item_num = next_item_num
  )
}


#* @post /cat
function(req, res) {
  key <- req$HTTP_X_API_KEY
  known_keys <- readLines("~/genKeys.txt")
  if (is.null(key) || !key %in% known_keys) {
    res$status <- 401
    return(list(error = "Missing or invalid key"))
  }
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  catName <- as.character(req$HTTP_CATNAME)
  if (is.null(catName) || catName == "") {
    res$status <- 400
    return(list(error = "Missing catName header"))
  }
  user <- get_user(catName, userid)
  if (is.null(user)) {
    res$status <- 404
    return(list(error = "Session not found; call /init first"))
  }
  df <- user$df
  body <- jsonlite::fromJSON(req$postBody)
  item_num <- as.numeric(body$item_num) # a
  
  resp_position <- as.numeric(body$q_resp) # 1, 2, 3, ..., max(categrory)
  chr_string <- as.character(df$Scores[item_num])
  resp <- unlist(strsplit(chr_string, split = ""))[resp_position] # takes the 'resp_position' element of the splitted 'chr_string' 
  ## pass the response to the algorithm
  # update CAT:
  CATdesign <- mirtCAT::updateDesign(user$catdesign,
                                     new_item = item_num, # at this item..
                                     new_response = resp) # ..this answer
  user$catdesign <- CATdesign
  user$pat <- CATdesign$person$responses
  
  theta <- round(as.numeric(CATdesign$person$thetas), 3)
  current_se <- round(as.numeric(
    CATdesign$person$thetas_SE_history[user$iter + 1L, ]
  ), 2)
  cat(catName, " - ", userid, ": SE is ",current_se,"@ iter: ",user$iter,"\n")
  terminate <- FALSE
  if(user$config$catType == "variable"){
    if(current_se <= user$stop_crit || sum(!is.na(user$pat)) == user$J) {
      terminate <- TRUE
    }}
  if(user$config$catType == "fixed"){
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
    if(!dir.exists(file.path(key, "sessions"))) dir.create(file.path(key, "sessions"), showWarnings = FALSE)
    filepath <- file.path(key, "sessions", paste0(userid, "_", catName, "_session.json"))
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
    )
    rm(user)
  } else { #terminate = FALSE
    next_item_num <- mirtCAT::findNextItem(CATdesign)
    user$iter <- user$iter + 1L
    # save responses to RAM
    save_user(catName, userid, user)
    # list to return:
    return(
      list(
        userid = userid,
        iter = user$iter,
        pat = user$pat,
        item_num = next_item_num,
        item = df$Item[next_item_num],
        choices = as.character(df[next_item_num, names(df)[grepl("Choice", names(df))]]),
        se_thetahat = current_se,
        thetahat = theta,
        stop = 0
      )
    )}
}

# Step 4 /user-results------------------------------------------------------------------

#* USER RESULTS TO JSON 
#* @get /user-results
function(req, res) {
  api_key <- req$HTTP_X_API_KEY
  known_keys <-trimws(readLines("~/genKeys.txt"))
  if (is.null(api_key) || !api_key %in% known_keys) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  userid <- req$HTTP_USERID
  if (is.null(userid) || userid == "") {
    res$status <- 400
    return(list(error = "Missing userid header"))
  }
  patt <- paste0("^", userid, "_.*_session\\.json$")
  dir <- paste0(api_key, "/sessions")
  file <- list.files(dir, pattern = patt, full.names = TRUE)
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
  known_keys <-trimws(readLines("~/genKeys.txt"))
  if (is.null(api_key) || !api_key %in% known_keys) {
    res$status <- 401
    return(list(error = "Invalid or missing API key"))
  }
  dir <- paste0(api_key, "/sessions")
  files <- list.files(dir, full.names = TRUE)
  
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

# END ---------------------------------------------------------------------













