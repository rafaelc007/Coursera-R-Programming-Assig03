## This scrip reads the data and plots the 30 day mortality rates from
## heart attack found in the data

## set 

library(rstudioapi)
## setting up the paths
current_path <- dirname(getActiveDocumentContext()$path)


data_path <- paste(current_path, "/data", sep = "")
setwd(data_path)

## loading the data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
