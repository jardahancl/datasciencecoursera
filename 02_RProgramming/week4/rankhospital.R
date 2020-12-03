rankHospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    result <- "invalid state"
  } else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    result <- "invalid outcome"
  ##TODO, possibly later in the code
#  } else if (!( num %in% c("best", "worst") )) {
#    if (num == "best") {num <- 1}
#    if (num == "worst") {num <- "last"}
  } else {
    if (outcome == "heart attack") {num_illness <- 11}
    if (outcome == "heart failure") {num_illness <- 17}
    if (outcome == "pneumonia") {num_illness <- 23}
    
    ## Return hospital name in that state with the given rank 30-day death rate
    data_use <- subset(data, 
                       State == state, 
                       #data[, num_illness] != "Not Available",
                       select=c(2, 7, num_illness))
    data_use <- subset(data_use, data_use[, 3] != "Not Available") 
    data_sort <- data_use[order(as.numeric(data_use[, 3]),
                                data_use[, 1]),]
    nr <- nrow(data_sort)
    
    if (num == "best") {result <- data_sort[1, 1]
    } else if (num == "worst") {result <- data_sort[nr, 1]
    } else {result <- data_sort[num, 1]
#    } else {result <- NA
    }
    
  }  
  result
  
}