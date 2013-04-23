library(tcltk2)

RepeatPre <- function() {
  source("preprocessing.R")
  cat("RUN preprocessing.R")
}

tclTaskSchedule(10000, RepeatPre(), id = "preprocessing", redo = TRUE)
Sys.sleep(2)

tclTaskGet() # List all (non hidden) tasks
tclTaskDelete(NULL) # finally, delete all pending tasks