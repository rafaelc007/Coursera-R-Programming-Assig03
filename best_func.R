t <- function(state, outcome) {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death
	## rate
  
  ## setting up the paths
  current_path <- dirname(getActiveDocumentContext()$path)
  
  
  data_path <- paste(current_path, "/data", sep = "")
  setwd(data_path)
  
  ## loading the data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## separating the work data
  State <- outcome[,7]
  Day_Death_by_HA <- as.numeric(outcome[,11])
  H_name <- outcome[,2]
  work_data <- work_data <- data.frame(H_name, State, Day_Death_by_HA)
  
  ## check validity of state
  valid <- FALSE
  for(state_name in State){
    if(state_name == State){
      valid <- TRUE
    }
  }
  ## if the validity is ok then
  if(valid){
    
  }
}
