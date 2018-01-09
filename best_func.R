library(rstudioapi)
## setting up the paths
current_path <- dirname(getActiveDocumentContext()$path)

data_path <- paste(current_path, "/data", sep = "")
setwd(data_path)

adjust <- function(nome){
  switch (nome,
          "heart attack" = "Heart.Attack",
          "heart failure" = "Heart.Failure",
          "pneumonia" = "Pneumonia", 0)
}

best <- function(state, outcome) {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death
	## rate
  
  ## initialize
  Hospital <- character()
  
  ## loading the data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## separating the work data
  adjusted <- adjust(outcome)
  if(adjusted == 0){
    print("Error: please insert a valid outcome")
    return(Hospital)
  }
  else{
    outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",adjusted,sep = "")
  }
  
  ## separating work data
  State <- outcome_data[,7]
  options(warn=-1)
  Day_Death <- as.numeric(outcome_data[,outcome])
  options(warn=0)
  H_name <- outcome_data[,2]
  work_data <- data.frame(H_name, State, Day_Death)
  
  ## check validity of state
  valid <- FALSE
  for(state_name in levels(factor(State))){
    if(state_name == state){
      valid <- TRUE
      break
    }
  }
  ## if the validity is ok then
  if(valid){
    ## separate interesting data
    work_data <- work_data[work_data[,2] == state,]
    
    ## find the minimum value
    min_pos <- which.min(work_data$Day_Death)
    Hospital <- as.character(work_data$H_name[min_pos])
  }
  else{print("Error: please insert a valid state")}
  Hospital
}
