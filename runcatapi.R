## log file added
log_file <- file("catapi.log", open = "a")
sink(log_file, type = "output",append = FALSE, split = TRUE)
sink(log_file, type = "message",append = FALSE)
# Start Plumber API on port 8000
library(plumber)
pr <- plumber::plumb("catapi.R")
pr$run(host = "0.0.0.0", port = 8000)
