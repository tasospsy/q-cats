# Start Plumber API on port 8000
pr <- plumber::plumb("myapi.R")
pr$run(host = "0.0.0.0", port = 8000)
