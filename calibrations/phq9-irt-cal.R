# -------------------------------------------------------------------------
# phq9 cat settings -------------------------------------------------------
# Make Question and Choices characters
questions <- c(
  "Over the last 2 weeks, how often have you been bothered by little interest or pleasure in doing things",
  "Over the last 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless",
  "Over the last 2 weeks, how often have you been bothered by trouble falling or staying asleep, or sleeping too much",
  "Over the last 2 weeks, how often have you been bothered by feeling tired or having little energy",
  "Over the last 2 weeks, how often have you been bothered by poor appetite or overeating",
  "Over the last 2 weeks, how often have you been bothered by feeling bad about yourself or that you are a failure or  have let yourself or your family down",
  "Over the last 2 weeks, how often have you been bothered by trouble concentrating on things, such as reading the  newspaper or watching television",
  "Over the last 2 weeks, how often have you been bothered by moving or speaking so slowly that other people could have noticed.  Or the opposite    being so figety or restless that you have been  moving around a lot more than usual",
  "Over the last 2 weeks, how often have you been bothered by thoughts that you would be better off dead, or of hurting yourself"
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
