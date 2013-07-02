## function to run preprocessing.R
RepeatPre <- function() {
  source("utils/preprocessing.R")
  cat("RUN preprocessing.R")
}


# ## solution 1
# library(tcltk2)
# tclTaskSchedule(120000, RepeatPre(), id = "preprocessing", redo = TRUE)
# Sys.sleep(2)
# #tclTaskGet() # List all (non hidden) tasks
# #tclTaskDelete(NULL) # finally, delete all pending tasks


## solution 2
repeat {
  RepeatPre()
  Sys.sleep(time = 3600)  # to stop execution for 5 sec
}