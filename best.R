best <- function(state, outcome) {
  ## Read outcome data
  master_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if (!(state %in% master_data[, 7])) {
    stop("invalid state")
  }
  
  if (!(outcome %in% names(outcomes))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in state with the lowest 30-day
  ## death rate
  subset_data <- master_data[ , c(2, 7, outcomes[outcome])]
  colnames(subset_data) <- c("name", "state", "outcome")
  clean_data <- na.omit(subset_data)
  clean_data <- clean_data[order(clean_data$state, clean_data$outcome, clean_data$name), ]
  clean_data.state <- clean_data[clean_data$state == state, ]
  return(clean_data.state$name[1])
}