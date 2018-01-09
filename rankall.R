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
    else if(class(nome) == "integer"){
        return(nome)
    }
    else{return(0)}
}

rankall <- function(outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
    
    ## initialize return data
    Hospitals <- character()
    
    ## loading the data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## separating the work data
    adjusted <- adjust(outcome)
    if(adjusted == 0){
        print("Error: please insert a valid outcome")
        return(Hospitals)
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
    
    ## states defined
    state <- levels(factor(State))
    index <- 1
    for(n_state in state){
        ## separate interesting data
        n_work_data <- work_data[work_data[,2] == n_state,]
        ## removing NAs
        n_work_data <- n_work_data[complete.cases(n_work_data$Day_Death),]
        
        ## checking the num names
        n_num <- as.integer(check_num(num,nrow(n_work_data)))
        
        ## rearange data in order
        n_work_data <-  with(n_work_data, n_work_data[order(Day_Death, H_name, State),])
        
        ## find the minimum value
        Hospitals[index] <- as.character(n_work_data$H_name[n_num])
        index <- index+1
    }
    ranked <- data.frame(state,Hospitals)
    colnames(ranked) <- c("state", "hospital")
    return(ranked)
}
