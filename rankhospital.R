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

check_num <- function(nome, leng = 0){
    if(class(nome) == "character"){
        switch (nome,
            "best" = 1,
            "worst" = leng, 0)
    }
    else if(class(nome) == "numeric"){
        return(as.integer(nome))
    }
    else{return(0)}
}

rankhospital <- function(state, outcome, num = 1){
    ## This function return a character vector containing the 
    ## name of the hospital with the num-est lowest 30-day death
    ## of  heart failure
    
    ## initialize return data
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
        ## removing NAs
        work_data <- work_data[complete.cases(work_data$Day_Death),]
        
        ## checking the num names
        num <- as.integer(check_num(num,nrow(work_data)))
        
        ## rearange data in order
        work_data <-  with(work_data, work_data[order(Day_Death, H_name, State),])
        
        ## find the minimum value
        Hospital <- as.character(work_data$H_name[num])
    }
    else{print("Error: please insert a valid state")}
    return(Hospital)
     
}  
