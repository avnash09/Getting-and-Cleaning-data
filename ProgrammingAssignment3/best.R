# Function to get the Hospital wit best 30-day mortality 

best <- function(state, outcome) {
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
  } else {
      if(outcome == "heart attack"){
        hosp_name <- getHsp_name(data, state, 11)
      } else if(outcome == "heart failure"){
        hosp_name <- getHsp_name(data, state, 17)
      } else {
        #gets Pneumonia data
        hosp_name <- getHsp_name(data, state, 23)
      }
  }
  return(hosp_name)
}

getHsp_name <- function(data, state, outcome_index) {
  data_subset <- subset(data, data$State == state)
  outcome_subset <- data_subset[, outcome_index]
  min_val <- min(outcome_subset, na.rm = TRUE)
  min_val_index <- which(outcome_subset == min_val)
  hosp_name <- data_subset[min_val_index, 2]
  return(hosp_name)
}