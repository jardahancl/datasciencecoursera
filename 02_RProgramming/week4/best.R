best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid, set the illness
  if (!(state %in% data$State)) {
    result <- "invalid state"
  } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    result <- "invalid outcome"
  } else {
    if (outcome == "heart attack") {num_illness <- 11}
    if (outcome == "heart failure") {num_illness <- 17}
    if (outcome == "pneumonia") {num_illness <- 23}
    
    ## Return hospital name in that state with lowest 30-day death rate
    data_use <- subset(data, State == state, select=c(2, 7, num_illness))
    data_NA <- data_use[complete.cases(data_use),]
    data_sort <- data_use[order(suppressWarnings(as.numeric(data_use[, 3])), data_use[, 1]),]
    
    result <- data_sort[1, 1]
  }  
  result
}





