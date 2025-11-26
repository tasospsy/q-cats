# Start Plumber API on port 8000
library(plumber)
pr <- plumber::plumb("catapi.R")
pr$run(host = "0.0.0.0", port = 8000)
