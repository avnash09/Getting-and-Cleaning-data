rankall <- function(outcome, num ="best") {
  data <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses="character")
  data[,11] <- as.numeric(data[,11]) #heart attack
  data[,17] <- as.numeric(data[,17]) #heart failure
  data[,23] <- as.numeric(data[,23]) #pneumonia
  all_states <- sort(unique(data$State))
  hsp_name <- as.array(NofStates)
  NofStates = length(all_states)
  
  #check for valid state and outcome, else proceed
  valid_outcomes <- c("heart attack","heart failure", "pneumonia")
  #for(i in 1:NofStates) {
  for(i in 1:NofStates){
  if(!(all_states[i] %in% all_states)) {
    stop("Invalid state")
  } else if(!(outcome %in% valid_outcomes)) {
    stop("Invalid outcome")
  } else if(num == "best") {
      rank <- best(state, outcome) 
  } else{
      if(outcome == "heart attack"){
        hsp_name[i] <- getHsp_Name(data, all_states[i], 11, num)
      } else if(outcome == "heart failure"){
        hsp_name[i] <- getHsp_Name(data, all_states[i], 17, num)
      } else {
        #gets Pneumonia data
        hsp_name[i] <- getHsp_Name(data, all_states[i], 23, num)
      } 
    }
  }
  df <- data.frame(hospital = hsp_name, state = all_states)
  return(df)
}

getHsp_Name <- function(data, state, idx, num) {
  data_subset <- subset(data, data$State == state)
  outcome_subset <- data_subset[, idx]
  len <- dim(data_subset[!is.na(outcome_subset),])[1]
  if(num == "worst") {
    rank <- getRank(data_subset, outcome_subset, len)
  } else if(num > len) {
    rank <- NA
  } else {
    rank <- getRank(data_subset, outcome_subset, num)
  }
  return(rank)
}

getRank <- function(data, outcome, num){
  ord_idx <- order(outcome, data[,2])
  rank <- data[,2][ord_idx][num]
  return(rank)
}
