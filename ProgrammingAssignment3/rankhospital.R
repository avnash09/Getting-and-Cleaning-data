# Fucntion to get the nth rank Hospital in terms of moratality rate of a particular outcome

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses="character")
  data[,11] <- as.numeric(data[,11]) #heart attack
  data[,17] <- as.numeric(data[,17]) #heart failure
  data[,23] <- as.numeric(data[,23]) #pneumonia
  all_states <- unique(data$State)
  
  #check for valid state and outcome, else proceed
  valid_outcomes <- c("heart attack","heart failure", "pneumonia")
  if(!(state %in% all_states)) {
    stop("Invalid state")
  } else if(!(outcome %in% valid_outcomes)) {
    stop("Invalid outcome")
  } else if(num == "best"){
     rank <- best(state, outcome)
  } else {
      if(outcome == "heart attack") {
      hosp_name <- getHosp_name(data, state, 11, num)
    } else if(outcome == "heart failure"){
      hosp_name <- getHosp_name(data, state, 17, num)
    } else {
      #gets Pneumonia data
      hosp_name <- getHosp_name(data, state, 23, num)
    }
  }
  return(hosp_name)
}

getHosp_name <- function (data, state, idx, rnk) {
  data_subset <- subset(data, data$State == state)
  outcome_subset <- data_subset[, idx]
  len = dim(data_subset[!is.na(outcome_subset),])[1]
  if(rnk == "worst") {
      rank <- getRank(data_subset, outcome_subset, len)
  } else if(rnk > len) {
      rank <- NA
  } else {
      rank <- getRank(data_subset, outcome_subset, rnk)
  }
  return(rank)
}

getRank <- function(data, outcome, num) {
  ndx <- order(outcome,data[,2])
  rank <- data[,2][ndx][num]
  return(rank)
}