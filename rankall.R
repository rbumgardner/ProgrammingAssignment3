rankall <- function(outcome, num = "best") {
  ## Read outcome data
  master_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  ## Check that state and outcome are valid
  if (!(outcome %in% names(outcomes))) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  subset_data <- master_data[ , c(2, 7, outcomes[outcome])]
  colnames(subset_data) <- c("name", "state", "outcome")
  clean_data <- na.omit(subset_data)
  clean_data <- clean_data[order(clean_data$state, clean_data$outcome, clean_data$name), ]
  states <- unique(clean_data[ , 2])
  df <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(df) <- c("hospital", "state")
  for (state in states) {
    clean_data.state <- clean_data[clean_data$state == state, ]
    if (num == "best") {
      df <- rbind(df, data.frame("hospital" = clean_data.state$name[1], "state" = state))
    } else if (num == "worst") {
      df <- rbind(df, data.frame("hospital" = clean_data.state$name[nrow(clean_data.state)], "state" = state))
    } else {
      df <- rbind(df, data.frame("hospital" = clean_data.state$name[num], "state" = state))
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df
}