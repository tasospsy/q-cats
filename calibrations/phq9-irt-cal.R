# -------------------------------------------------------------------------
# phq9 cat settings -------------------------------------------------------
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

## Define answer options
#choices <- list(
#  c("0) Not at all", 
#    "1) Several days", 
#    "2) More than half the days", 
#    "3) Nearly every day")
#)

#choices <- matrix(NA, nitems, 5) 

# irt CAT calibration -----------------------------------------------------
library(mirtCAT)
# load the data
load(url("https://github.com/tasospsy/q-cats/raw/refs/heads/main/data/PHQ9-NHANES.RData"))
X <- rbind(PHQ9test, PHQ9vali)
if(ncol(X) != length(questions)) stop("Number of columns != number of items")

mod <- mirt(X, 1, itemtype = 'graded')
#summary(mod)
coef(mod, simplify = TRUE)

## Pass to server 'questions', 'choices', 'mod' objects
save(
  questions, mod,
  file = "calibrations/phq9-irt-cal-results.RData"
)
